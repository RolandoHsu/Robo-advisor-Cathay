library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(corrplot)
library(stringr)
library(e1071) # skewness kurtosis
library(quantmod)
library(PerformanceAnalytics)
library(purrr)
library(zoo)
library(readxl)
library(data.table)

##### import data #####
cathay_funds <- read.csv("cathay_funds.csv") %>% as.data.table()
fund_risk_rank <- read_xlsx("fund_risk_rank.xlsx") %>% 
  as.data.table() %>% 
  .[,-2]

# 銀行一年期的定存利率
rf_rate <- (0.011867+1)^(1/252)-1

##### clear data #####
funds <- full_join(cathay_funds, fund_risk_rank, key = "基金代號")

colnames(funds) <- c("f_num", "f_name", "f_type", "date", "book_value", "risk_rank")

# 清除不要的東西、計算每日報酬(log)
funds_2 <- funds %>% 
  mutate_at(4, lubridate::ymd) %>% 
  filter(!(str_sub(f_name, 1, 5) == "(已終止)" | is.na(risk_rank) | is.na(f_type))) %>% 
  group_by(f_num, f_name, risk_rank, f_type) %>%
  arrange(f_name, date) %>%
  filter(!(book_value == 0)) %>%
  filter(date >= lubridate::ymd("2017/07/01")) %>%
  mutate(return = Delt(book_value, k=1, type="log")) %>% 
  ungroup() 

# 計算有多少基金會缺少日資料
funds_2_forNA <- funds_2 %>%
  select(f_num, date, book_value) %>%
  spread(f_num, book_value)

NA_sign <- NULL
for (i in 2:ncol(funds_2_forNA)) {
  NA_num <- funds_2_forNA[, i] %>%
    is.na() %>%
    sum()
  NA_sign <- rbind(NA_sign, NA_num)
}

WhohasNA <- NA_sign %>%
  as.data.frame() %>%
  cbind(., names(funds_2_forNA)[-1]) %>%
  rename("f_num" = "names(funds_2_forNA)[-1]") %>% 
  select(f_num, V1) %>%
  filter(V1 != 0)

# 去除掉一年半內沒有資料的
funds_2 <- funds_2 %>% 
  filter(!(f_num %in% WhohasNA$f_num))

# 計算每檔基金的 平均報酬 標準差 變異數 偏態 鋒態 VaR SPR CV
funds_3 <- funds_2 %>% 
  group_by(f_num, f_name, risk_rank, f_type) %>%  
  summarise(mean_return = mean(return, na.rm = T),
            #CAR = Return.cumulative(mean_return, geometric = TRUE),
            sd = sd(return, na.rm = T),
            variance = var(return, na.rm = T),
            skewness = PerformanceAnalytics::skewness(return, na.rm = T),
            kurtosis = PerformanceAnalytics::kurtosis(return, na.rm = T),
            VaR = VaR(return, p = 0.95, method = "gaussian"),
            SharpeRatio = mean_return / sd,
            CV = sd / mean_return) %>% 
  ungroup() %>% 
  mutate(mean_return = map_dbl(mean_return, 
                               function(x) Return.annualized(x, scale = 252, geometric = T)),
         SharpeRatio = SharpeRatio* (252^(1/2)))
# 轉成 factor 方便做summary
funds_3$risk_rank <- as.factor(funds_3$risk_rank)

summary(funds_3)

funds_3_everyRR <- funds_3 %>% 
  arrange(risk_rank) %>% 
  group_by(risk_rank) %>% 
  summarise(
    med_R = median(mean_return), 
    med_var = median(variance)
  )

# cheak NA #
for (i in names(funds_2)) {
  na_num <- funds_2 %>% 
    pull(i) %>% 
    is.na() %>% 
    sum()
  print(na_num)
}

##### choose the best funds in every Level ##### 
risk_choose <- function(ratio, data){
  var_part = data %>% 
    arrange(variance) %>% 
    slice(1 : (nrow(data)*ratio)) %>% 
    select(f_num)
  CV_part = data %>% 
    arrange(CV) %>% 
    slice(1 : (nrow(data)*ratio))%>% 
    select(f_num)
  VaR_part = data %>% 
    arrange(VaR) %>% 
    slice(1 : (nrow(data)*ratio))%>% 
    select(f_num)
  sign = rbind(var_part, CV_part, VaR_part) %>% 
    count(f_num) %>% 
    filter(n == 3) %>% 
    pull(f_num)
  Answer = funds_3 %>% 
    filter(f_num %in% sign)
  return(Answer)
}

return_choose <- function(ratio, data){
  Ave_return = data %>% 
    arrange(desc(mean_return))%>% 
    slice(1 : (nrow(data)*ratio)) %>% 
    select(f_num)
  SP_ratio = data %>% 
    arrange(desc(SharpeRatio)) %>% 
    slice(1 : (nrow(data)*ratio)) %>% 
    select(f_num)
  sign = rbind(Ave_return, SP_ratio) %>% 
    count(f_num) %>% 
    filter(n == 2) %>% 
    pull(f_num)
  Answer = funds_3 %>% 
    filter(f_num %in% sign)
  return(Answer)
}

find_bestfunds <- function(RR, var_ratio, ave_ratio, data, var_first){
  Answer = if (var_first == T) {
    data %>% 
      filter(risk_rank %in% RR) %>% 
      risk_choose(var_ratio, .) %>% 
      return_choose(ave_ratio, .)
  }else{
    data %>% 
      filter(risk_rank %in% RR) %>% 
      return_choose(ave_ratio, .) %>% 
      risk_choose(var_ratio, .)
  }
  return(Answer)
}

RR1 <- find_bestfunds(c("RR1", "RR2"), var_ratio = 0.8, ave_ratio = 0.85, funds_3, var_first = F) %>% 
  arrange(desc(mean_return))
RR2 <- find_bestfunds("RR3", var_ratio = 0.95, ave_ratio = 0.65, funds_3, var_first = F) %>% 
  arrange(desc(mean_return))
RR3 <- find_bestfunds("RR4", var_ratio = 0.95, ave_ratio = 0.65, funds_3, var_first = F) %>% 
  arrange(desc(mean_return))
RR4 <- find_bestfunds("RR5", var_ratio = 0.95, ave_ratio = 0.65, funds_3, var_first = F) %>% 
  arrange(desc(mean_return))

# 各套餐候選人
Level1_fnum <- c("033", "039", "035A", "038C", "038A")
Level2_fnum <- c("033", "038E", "060", "044E", "052D")
Level3_fnum <- c("033", "038F", "060", "044E", "027B")
Level4_fnum <- c("038F", "060", "047", "050", "027B")

###### 檢驗是否為常態 #####
CheckNormal <- function(fund_list){
  for (i in fund_list) {
  normaltest_data <- funds_2 %>% 
    filter(f_num == i) %>% 
    select(date, return) %>% 
    as.data.frame()
  
  if(nrow(normaltest_data) == 0) next
  
  normal_qqplot <- xts(normaltest_data[, -1], normaltest_data[, 1]) %>% 
    PerformanceAnalytics::chart.QQPlot(., main = paste0(i, "_QQplot"))
  
  shapiro.test(normaltest_data[, 2]) %>% print()
  
  }
}
Level_list <- c(Level1_fnum, Level2_fnum, Level3_fnum, Level4_fnum)
map(Level_list, function(x){CheckNormal(x)})

###### 找尋最適權重 #####
##### 找出權重候選人 #####
weight_formal_fun <- function(low, high, sep_sign, Levelx_fnum){
  # build the portfolio candidation #
  weight_element1 <- seq(low, high, sep_sign)
  weight_element2 <- seq(low, high, sep_sign)
  weight_element3 <- seq(low, high, sep_sign)
  weight_element4 <- seq(low, high, sep_sign)
  weight_element5 <- seq(low, high, sep_sign)
  
  weight_candidate <- crossing(weight_element1, weight_element2, weight_element3, weight_element4, weight_element5) %>% 
    mutate(sum = rowSums(.)) %>% 
    filter(sum == 1) %>% 
    .[, -6]
  
  # 計算每一個權重的變異數
  variance_function <- function(x){
    weight_1 <- weight_candidate[x, ] %>% as.numeric() %>% as.matrix(., 5, 1)
    every_var <- t(weight_1) %*% cov_matrix %*% weight_1
    return(every_var)
  }

  # 共變異數矩陣
  cov_matrix <- funds_2 %>% 
    filter(f_num %in% Levelx_fnum) %>% 
    select(f_num, date, return) %>%
    spread(key = f_num, value = return) %>% 
    .[, -1] %>% 
    cov(., use = "na.or.complete")
  
  ##### 挑選出變異數較小的候選人 #####
  weight_formal <- weight_candidate %>% 
    mutate(variance = map_dbl(1: nrow(weight_candidate), variance_function)) %>% 
    arrange(variance) %>% 
    filter(variance <= quantile(variance, 0.2)) %>% 
    .[, 1:5] %>% 
    as.matrix() %>% 
    unname() %>% 
    rbind(c(0.2, 0.2, 0.2, 0.2, 0.2), .) # 第一個為等權重的情況
  
  return(weight_formal)
}

##### 找出所有權重的日報酬 #####  
everyportfolio_fun <- function(weight_formal, Levelx_fnum){  
  ##### 計算每一種投組的各種資訊 #####
  dailyreturn_target <- funds_2 %>% 
    select(f_num, date, return) %>% 
    filter(f_num %in% Levelx_fnum) %>% 
    spread(f_num, return)
  
  dailyreturn <- dailyreturn_target%>% 
    .[, -1] 
  
  ##### 沒有NA的dailyreturn #####
  dailyR_everypro_withoutNA_data <- dailyreturn %>% 
    .[rowSums(is.na(.)) == 0, ] %>% 
    as.matrix() %>% 
    unname() %*% t(weight_formal)
  
  dailyR_everypro_withoutNA <- dailyreturn_target %>% 
    .[rowSums(is.na(.)) == 0, ] %>%
    select(date) %>% 
    cbind(., dailyR_everypro_withoutNA_data)
  
  dailyR_everypro <- dailyR_everypro_withoutNA %>% 
    arrange(date)
  
  # 轉成 xts
  dailyR_everypro <- xts(dailyR_everypro[, -1], dailyR_everypro[, 1])
  
  return(dailyR_everypro)
}

##### 找出最好的權重，並拉出他的日報酬 #####
bestportfolio_fun <- function(R_weight, var_weight, skew_weight){
  dailyR_everypro2 <- colnames(dailyR_everypro) %>% 
    as.data.frame() %>% 
    rename("weight" = ".") %>% 
    mutate(
      annual_return = map(1 : ncol(dailyR_everypro), 
                          function(x) PerformanceAnalytics::Return.annualized(dailyR_everypro[, x])),
      variance = map(1 : ncol(dailyR_everypro), 
                     function(x) var(dailyR_everypro[, x])),
      skewness = map(1 : ncol(dailyR_everypro), 
                     function(x) PerformanceAnalytics::skewness(dailyR_everypro[, x], na.rm = T))
    ) %>% 
    mutate_at(c(2:4), unlist) %>% 
    mutate(
      annual_R_diff = max(annual_return) - annual_return,
      variance_diff = variance - min(variance), 
      skew_diff = max(skewness) - skewness, 
      annual_R_diff_nor = (annual_R_diff - mean(annual_R_diff, na.rm = T))/ sd(annual_R_diff, na.rm = T),
      variance_diff_nor = (variance_diff - mean(variance_diff, na.rm = T))/ sd(variance_diff, na.rm = T), 
      skew_diff_nor = (skew_diff - mean(skew_diff, na.rm = T))/ sd(skew_diff, na.rm = T),
      score = annual_R_diff_nor^R_weight + variance_diff_nor^var_weight + skew_diff_nor^skew_weight
    ) %>% 
    arrange(score) %>% 
    .[1, 1] %>% 
    as.numeric()
  
  bestportfolio <- dailyR_everypro[, dailyR_everypro2]
  return(bestportfolio)
}

weight_formal <- weight_formal_fun(0.05, 0.95, 0.05, Level1_fnum)
dailyR_everypro <- everyportfolio_fun(weight_formal, Level1_fnum)
bestportfolio <- bestportfolio_fun(2, 1, 2)

# level 1 
weight_formal <- weight_formal_fun(0.05, 0.95, 0.05, Level1_fnum)
dailyR_everypro <- everyportfolio_fun(weight_formal, Level1_fnum)
bestportfolio_level1 <- bestportfolio_fun(2, 1, 2)
weight_formal_level1 <- weight_formal
dailyR_everypro_level1 <- dailyR_everypro

# level 2 
weight_formal <- weight_formal_fun(0.05, 0.95, 0.05, Level2_fnum)
dailyR_everypro <- everyportfolio_fun(weight_formal, Level2_fnum)
bestportfolio_level2 <- bestportfolio_fun(2, 1, 2)
weight_formal_level2 <- weight_formal
dailyR_everypro_level2 <- dailyR_everypro

# level 3 
weight_formal <- weight_formal_fun(0.05, 0.95, 0.05, Level3_fnum)
dailyR_everypro <- everyportfolio_fun(weight_formal, Level3_fnum)
bestportfolio_level3 <- bestportfolio_fun(2, 1, 2)
weight_formal_level3 <- weight_formal
dailyR_everypro_level3 <- dailyR_everypro

# level 4 
weight_formal <- weight_formal_fun(0.05, 0.95, 0.05, Level4_fnum)
dailyR_everypro <- everyportfolio_fun(weight_formal, Level4_fnum)
bestportfolio_level4 <- bestportfolio_fun(2, 1, 2)
weight_formal_level4 <- weight_formal
dailyR_everypro_level4 <- dailyR_everypro

weight_formal_level1[90, ] # 最適權重
weight_formal_level2[290, ] # 最適權重
weight_formal_level3[113, ] # 最適權重
weight_formal_level4[334, ] # 最適權重


##### 0050 #####
getSymbols("0050.TW", from="2017-6-1", to = "2018-12-31", src = "yahoo")
TW_0050 <- `0050.TW`[, 6] 

chartSeries(TW_0050, theme = "white")

# 計算 每日累積報酬並繪圖
DR_0050 <- Return.calculate(TW_0050, method = "log") 

plot_0050 <- DR_0050 %>% 
  as.data.frame() %>% 
  rename(dailyR = `0050.TW.Adjusted`) %>% 
  mutate(dailyR = ifelse(is.na(dailyR), 0, dailyR)) %>% 
  .[, 1] %>%
  cumsum() %>% 
  plot(., main = "CAR_plot", xlab = "day", ylab = "CAR", col = "dark red")


##### 評價 #####
# 最佳投組 #  
bestPro_level1 <- dailyR_everypro_level1[, 90]
equalWeight_level1 <- dailyR_everypro_level1[, 1]
bestPro_level2 <- dailyR_everypro_level2[, 290]
equalWeight_level2 <- dailyR_everypro_level2[, 1]
bestPro_level3 <- dailyR_everypro_level3[, 113]
equalWeight_level3 <- dailyR_everypro_level3[, 1]
bestPro_level4 <- dailyR_everypro_level4[, 334]
equalWeight_level4 <- dailyR_everypro_level4[, 1]


# 最佳投組的summary #
summary_function <- function(Data){
  SharpeRatio <- Data %>% 
    PerformanceAnalytics::SharpeRatio.annualized()
  meanR <- Data %>% 
    PerformanceAnalytics::Return.annualized()
  CAR <- Data %>% 
    PerformanceAnalytics::Return.cumulative()
  SD <- Data %>% 
    PerformanceAnalytics::sd.annualized()
  VaR <- Data %>% 
    PerformanceAnalytics::VaR()
  maxDrawdown <- Data %>% 
    PerformanceAnalytics::maxDrawdown()
  BurkeRatio <- Data %>% 
    PerformanceAnalytics::BurkeRatio() 
  CalmarRatio <- Data %>% 
    PerformanceAnalytics::CalmarRatio() # 最大風報比
  summary <- NULL
  summary <- rbind(summary, meanR, CAR, SharpeRatio, BurkeRatio, CalmarRatio, SD, VaR, maxDrawdown)
  return(summary)
}  

summary_function(bestPro_level1)
summary_function(bestPro_level2)
summary_function(bestPro_level3)
summary_function(bestPro_level4)
summary_function(DR_0050)

result <- cbind(summary_function(DR_0050), 
                summary_function(bestPro_level1),
                summary_function(equalWeight_level1),
                summary_function(bestPro_level2),
                summary_function(equalWeight_level2),
                summary_function(bestPro_level3),
                summary_function(equalWeight_level3),
                summary_function(bestPro_level4),
                summary_function(equalWeight_level4))
colnames(result) <- c("TW0050", 
                      "Level1", 
                      "Level1_equ", 
                      "Level2", 
                      "Level2_equ", 
                      "Level3", 
                      "Level3_equ", 
                      "Level4", 
                      "Level4_equ")

#write.csv(result, file = "C:/Users/Rpo/Desktop/result.csv")

##### 計算累積報酬並繪圖
plotdata_0050 <- DR_0050 %>% 
  as.data.frame() %>% 
  rename(dailyR = `0050.TW.Adjusted`) %>% 
  mutate(dailyR = ifelse(is.na(dailyR), 0, dailyR), 
         date = time(DR_0050))

CAR_0050 <- xts(plotdata_0050[, -2], plotdata_0050[, 2]) %>% cumsum()

plot_function <- function(Level_sign ,bestPro_data, equalWeight_data){
  CAR <- cbind(cumsum(bestPro_data), 
                      cumsum(equalWeight_data),
                      CAR_0050) %>% 
    data.frame(time(.), .) %>% 
    mutate(CAR_0050 = na.locf(CAR_0050)) %>% 
    rename(Date = names(.[1]) , Portfolio = names(.[2]), Portfolio_EW = names(.[3])) %>% 
    gather(., 
           Portfolio, 
           Portfolio_EW, 
           names(.[4]), 
           key = "Portfolio", 
           value = "CAR")
  
  CAR_plot <- CAR %>% 
    ggplot(aes(x = Date, y = CAR)) + 
    geom_line(aes(color = Portfolio)) +
    labs(title = Level_sign) +
    theme_light() +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(1.5)))
  
  return(CAR_plot)
}

plot_function("Level_1", bestPro_level1, equalWeight_level1)
plot_function("Level_2", bestPro_level2, equalWeight_level2)
plot_function("Level_3", bestPro_level3, equalWeight_level3)
plot_function("Level_4", bestPro_level4, equalWeight_level4)


library(ICSNP)
test1 <- bestPro_level1["2017-06::2017-12"]
test2 <- bestPro_level1["2018-01::2018-06"]
test3 <- bestPro_level1["2018-06::2018-12"]

test4 <- bestPro_level2["2017-06::2017-12"]
test5 <- bestPro_level2["2018-01::2018-06"]
test6 <- bestPro_level2["2018-06::2018-12"]

test7 <- bestPro_level3["2017-06::2017-12"]
test8 <- bestPro_level3["2018-01::2018-06"]
test9 <- bestPro_level3["2018-06::2018-12"]

test10 <- bestPro_level4["2017-06::2017-12"]
test11 <- bestPro_level4["2018-01::2018-06"]
test12 <- bestPro_level4["2018-06::2018-12"]

rank.ctest(test1, test2, scores = "rank")
rank.ctest(test2, test3, scores = "rank")

rank.ctest(test4, test5, scores = "rank")
rank.ctest(test5, test6, scores = "rank")

rank.ctest(test7, test8, scores = "rank")
rank.ctest(test8, test9, scores = "rank")

rank.ctest(test10, test11, scores = "rank")
rank.ctest(test11, test12, scores = "rank")

