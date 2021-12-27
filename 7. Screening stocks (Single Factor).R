rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#Beta 계산하기
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('102110.KS', '039490.KS')
getSymbols(symbols)

prices = do.call(cbind, #종가 추출 -> 리스트를 열로 묶어줌
                 lapply(symbols, function(x)Cl(get(x))))

ret = Return.calculate(prices)
ret = ret['2016-01::2018-12']

rm = ret[, 1]
ri = ret[, 2]

reg = lm(ri ~ rm)
summary(reg)


#pch: 점 모양, cex: 점 크기, a: 상수, b: 기울기, lty: 선의 유형
plot(as.numeric(rm), as.numeric(ri), pch = 4, cex = 0.3, 
     xlab = "KOSPI 200", ylab = "Individual Stock",
     xlim = c(-0.02, 0.02), ylim = c(-0.02, 0.02))
abline(a = 0, b = 1, lty = 2)
abline(reg, col = 'red')



# Test
getSymbols('^GSPC', 
           from = '1950-07-01', to = '2021-12-31')
price <- GSPC$GSPC.Close
ret1 = Return.calculate(price)
ret1 = ret1[-1,]

chartSeries(GSPC)
chartSeries(GSPC["2008-12"])

chartSeries(GSPC,
            type="candlesticks",subset="2009-01/2009-03",
            name="SP500 Candlesticks",theme=chartTheme("white"),
            show.grid=F,major.ticks="months",minor.ticks=F,
            up.col="red",dn.col="blue",color.vol=F)
addSMA(n=10,on=1,col="black")

plot(GSPC$GSPC.Adjusted, main="SP500 Index", type="b",pch=2, 
     col="blue", subset="2009-01/2009-03")


#Volatility (Low) Factor (Daily)
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

KOR_price = read.csv('KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts() #xts
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' = 
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price)
std_12m_daily = xts::last(ret, 252) %>% apply(., 2, sd) %>%
  multiply_by(sqrt(252)) #Standard Deviation (Annualized)

std_12m_daily %>% 
  data.frame() %>%
  ggplot(aes(x = (`.`))) +
  geom_histogram(binwidth = 0.01) +
  annotate("rect", xmin = -0.02, xmax = 0.02,
           ymin = 0,
           ymax = sum(std_12m_daily == 0, na.rm = TRUE) * 1.1,
           alpha=0.3, fill="red") +
  xlab(NULL) #Histogram of S.D.

std_12m_daily[std_12m_daily == 0] = NA #거래정지 종목 NA처리
std_12m_daily[rank(std_12m_daily) <= 30] #30위 이하 랭킹


std_12m_daily[rank(std_12m_daily) <= 30] %>% #오름차순 디폴트
  data.frame() %>%
  ggplot(aes(x = rep(1:30), y = `.`)) +
  geom_col() +
  xlab(NULL) #저변동 종목부터 1순위 ~


invest_lowvol = rank(std_12m_daily) <= 30
KOR_ticker[invest_lowvol, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` = round(std_12m_daily[invest_lowvol], 4))


#Volatility (Low) Factor (Weekly)
std_12m_weekly = xts::last(ret, 252) %>%
  apply.weekly(Return.cumulative) %>%
  apply(., 2, sd) %>% multiply_by(sqrt(52))

std_12m_weekly[std_12m_weekly == 0] = NA
std_12m_weekly[rank(std_12m_weekly) <= 30]

invest_lowvol_weekly = rank(std_12m_weekly) <= 30
KOR_ticker[invest_lowvol_weekly, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` =
           round(std_12m_weekly[invest_lowvol_weekly], 4))

intersect(KOR_ticker[invest_lowvol, '종목명'],
          KOR_ticker[invest_lowvol_weekly, '종목명'])



#Momentum Factor (12month)
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)

KOR_price = read.csv('KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) %>% xts::last(252) #최근 252일
ret_12m = ret %>% sapply(., function(x) {            #    수익률
  prod(1+x) - 1 #각 종목의 누적수익률 계산
})
ret_12m[rank(-ret_12m) <= 30] #내림차순이 선호된다.

invest_mom = rank(-ret_12m) <= 30
KOR_ticker[invest_mom, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom], 4))


#Momentum Factor (Risk-Adjusted)
ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})
std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))
sharpe_12m = ret_12m / std_12m #Sharpe's ratio (Risk Free rate X)


invest_mom_sharpe = rank(-sharpe_12m) <= 30 #내림차순
KOR_ticker[invest_mom_sharpe, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom_sharpe], 2),
         `변동성` = round(std_12m[invest_mom_sharpe], 2),
         `위험조정 수익률` =
           round(sharpe_12m[invest_mom_sharpe], 2)) %>%
  as_tibble() %>%
  print(n = Inf)

intersect(KOR_ticker[invest_mom, '종목명'],
          KOR_ticker[invest_mom_sharpe, '종목명'])

##Graphing
library(xts)
library(tidyr)
library(ggplot2)

KOR_price[, invest_mom_sharpe] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) %>%
  ggplot(aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap(. ~ ticker, scales = 'free') +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

#Value Factor (Low PBR)
library(stringr)
library(ggplot2)
library(dplyr)

KOR_value = read.csv('KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

invest_pbr = rank(KOR_value$PBR) <= 30
KOR_ticker[invest_pbr, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`PBR` = round(KOR_value[invest_pbr, 'PBR'], 4)) %>%
  arrange(.,PBR)

#각 지표 결합하기! (상관관계)
library(corrplot)

rank_value = KOR_value %>% 
  mutate_all(list(~min_rank(.))) #모든 열에 함수 적용 + 순위

cor(rank_value, use = 'complete.obs') %>% #NA종목 삭제
  round(., 2) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col = colorRampPalette(
             c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))


rank_sum = rank_value %>%
  rowSums()

invest_value = rank(rank_sum) <= 30

KOR_ticker[invest_value, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(KOR_value[invest_value, ], 2))

intersect(KOR_ticker[invest_pbr, '종목명'],
          KOR_ticker[invest_value, '종목명'])

#Quality Factor

##F-Score
### Profitability (수익성)
### Earnings stability (수익의 안정성)
### Capital structure (기업 구조)
### Growth (수익의 성장성)
### Accounting quality (회계적 우량성)
### Payout/dilution (배당)
### Investment (투자)

library(stringr)
library(ggplot2)
library(dplyr)

KOR_fs = readRDS('KOR_fs.Rds')
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

# 수익성
ROA = KOR_fs$'지배주주순이익' / KOR_fs$'자산'
CFO = KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산'
ACCURUAL = CFO - ROA

# 재무성과
LEV = KOR_fs$'장기차입금' / KOR_fs$'자산'
LIQ = KOR_fs$'유동자산' / KOR_fs$'유동부채'
OFFER = KOR_fs$'유상증자'

# 운영 효율성
MARGIN = KOR_fs$'매출총이익' / KOR_fs$'매출액'
TURN = KOR_fs$'매출액' / KOR_fs$'자산'

#### Scoring
if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

F_1 = as.integer(ROA[, num_col] > 0)
F_2 = as.integer(CFO[, num_col] > 0)
F_3 = as.integer(ROA[, num_col] - ROA[, (num_col-1)] > 0)
F_4 = as.integer(ACCURUAL[, num_col] > 0) 
F_5 = as.integer(LEV[, num_col] - LEV[, (num_col-1)] <= 0) 
F_6 = as.integer(LIQ[, num_col] - LIQ[, (num_col-1)] > 0)
F_7 = as.integer(is.na(OFFER[,num_col]) |
                   OFFER[,num_col] <= 0)
F_8 = as.integer(MARGIN[, num_col] -
                   MARGIN[, (num_col-1)] > 0)
F_9 = as.integer(TURN[,num_col] - TURN[,(num_col-1)] > 0)


F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9) 
F_Score = F_Table %>%
  apply(., 1, sum, na.rm = TRUE) %>%
  setNames(KOR_ticker$`종목명`)

(F_dist = prop.table(table(F_Score)) %>% round(3))


F_dist %>%
  data.frame() %>%
  ggplot(aes(x = F_Score, y = Freq,
             label = paste0(Freq * 100, '%'))) +
  geom_bar(stat = 'identity') +
  geom_text(color = 'black', size = 3, vjust = -0.4) +
  scale_y_continuous(expand = c(0, 0, 0, 0.05),
                     labels = scales::percent) +
  ylab(NULL) +
  theme_classic() 

invest_F_Score = F_Score %in% c(9)
KOR_ticker[invest_F_Score, ] %>% 
  select(`종목코드`, `종목명`) %>%
  mutate(`F-Score` = F_Score[invest_F_Score])


#각 지표를 결합하기
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

KOR_fs = readRDS('KOR_fs.Rds')
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)


if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]
quality_gpa = (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]
quality_cfo =
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))


rank_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.))))

cor(rank_quality, use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))


rank_sum = rank_quality %>%
  rowSums()

invest_quality = rank(rank_sum) <= 30

KOR_ticker[invest_quality, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(quality_profit[invest_quality, ], 4))