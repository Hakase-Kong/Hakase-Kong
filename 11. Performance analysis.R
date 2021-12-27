rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")


library(dplyr)
library(readxl)
library(xts)
library(timetk)

url = 'https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx'
    
url = 'https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx'

tf = tempfile(fileext = '.xlsx') #임시로 엑셀파일 만들기
download.file(url, tf, mode = 'wb') #tf파일명에 저장, 바이너리 파일 wb

excel_sheets(tf) #해당 엑셀의 시트명들을 확인


df_QMJ = read_xlsx(tf, sheet = 'QMJ Factors', skip = 18) %>%
  select(DATE, Global)
df_MKT = read_xlsx(tf, sheet = 'MKT', skip = 18) %>%
  select(DATE, Global)
df_SMB = read_xlsx(tf, sheet = 'SMB', skip = 18) %>%
  select(DATE, Global)
df_HML_Devil = read_xlsx(tf, sheet = 'HML Devil',
                         skip = 18) %>%
  select(DATE, Global)
df_UMD = read_xlsx(tf, sheet = 'UMD', skip = 18) %>%
  select(DATE, Global)
df_RF = read_xlsx(tf, sheet = 'RF', skip = 18) 
#18행까지는 skip, 엑셀 데이터를 읽어오기, 날짜/수익률 선택


df = Reduce(function(x, y) inner_join(x, y, by = 'DATE'),
            list(df_QMJ, df_MKT, df_SMB,
                 df_HML_Devil,df_UMD, df_RF)) %>%
  setNames(c('DATE','QMJ', 'MKT', 'SMB',
             'HML', 'UMD', 'RF')) %>%
  na.omit() %>%
  mutate(DATE = as.Date(DATE, "%m/%d/%Y"),
         R_excess = QMJ - RF,
         Mkt_excess = MKT - RF) %>%
  tk_xts(date_var = DATE)



#결과 측정 지표 (수익률: 누적/연율화/연도별 수익률, 위험: 변동성/낙폭)
library(PerformanceAnalytics)
chart.CumReturns(df$QMJ)

prod((1+df$QMJ)) - 1 # 누적수익률
mean(df$QMJ) * 12 # 연율화 수익률(산술)
(prod((1+df$QMJ)))^(12 / nrow(df$QMJ)) - 1 # 연율화 수익률(기하)
#일간: 252, 주간: 52, 월간: 12 / 시계열의 관측기간


Return.cumulative(df$QMJ) # 누적수익률
Return.annualized(df$QMJ, geometric = FALSE) # 연율화 수익률(산술)
Return.annualized(df$QMJ) # 연율화 수익률(기하)



sd(df$QMJ) * sqrt(12) # 연율화 변동성
StdDev.annualized(df$QMJ) # 연율화 변동성

SharpeRatio.annualized(df$QMJ, Rf = df$RF, geometric = TRUE)



#낙폭과 최대낙폭
table.Drawdowns(df$QMJ)
maxDrawdown(df$QMJ)
chart.Drawdown(df$QMJ)

CalmarRatio(df$QMJ) #칼마 지수 (연율화 수익률/최대낙폭) 헤지펀드용



#연도별 수익률
apply.yearly(df$QMJ, Return.cumulative) %>% head()


library(lubridate)
library(tidyr)
library(ggplot2)

R.yr = apply.yearly(df$QMJ, Return.cumulative) %>% #연도별 수익률
  fortify.zoo() %>%
  mutate(Index = year(Index)) %>%
  gather(key, value, -Index) %>%
  mutate(key = factor(key, levels = unique(key)))

ggplot(R.yr, aes(x = Index, y = value, fill = key)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle('Yearly Return') +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  scale_y_continuous(expand = c(0.03, 0.03)) +
  scale_x_continuous(breaks = R.yr$Index,
                     expand = c(0.01, 0.01)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank() ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  geom_text(aes(label = paste(round(value * 100, 2), "%"),
                vjust = ifelse(value >= 0, -0.5, 1.5)),
            position = position_dodge(width = 1),
            size = 3)


#승률 및 롤링 윈도우 값
UpsideFrequency(df$QMJ, MAR = 0) #승률 / MAR: 원하는 벤치마크

##롤링 윈도우 (무작위 시점에 투자했을 때 향후 n개월 후 승률)
roll_12 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  UpsideFrequency() #월간수익률 > 롤링윈도우(12) 통계값 > 승률

roll_24 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 24, Return.annualized) %>% na.omit() %>%
  UpsideFrequency() #월간수익률 > 롤링윈도우(24) 통계값 > 승률

roll_36 = df$QMJ %>% apply.monthly(., Return.cumulative) %>%
  rollapply(., 36, Return.annualized) %>% na.omit() %>%
  UpsideFrequency() #월간수익률 > 롤링윈도우(36) 통계값 > 승률

roll_win = cbind(roll_12, roll_24, roll_36)
print(roll_win)


df$QMJ %>% apply.monthly(., Return.cumulative) %>% #연율화 수익률
  rollapply(., 12, Return.annualized) %>% na.omit() %>%
  fortify.zoo() %>%
  ggplot(aes(x = Index, y = QMJ)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = 'red') +
  xlab(NULL) + ylab(NULL)



#팩터 회귀분석 및 테이블로 나타내기 (수익률 회귀분석)
reg = lm(R_excess ~ Mkt_excess + SMB + HML + UMD, data = df)
# summary(reg)
summary(reg)$coefficient
summary(reg)

library(broom)
tidy(reg)

library(stargazer)
stargazer(reg, type = 'text', out = 'reg_table.html')