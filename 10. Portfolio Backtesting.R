rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#Return.portfolio() 인자목록 알아보기
Return.portfolio(R, weights = NULL, wealth.index = FALSE,
                 contribution = FALSE, geometric = TRUE,
                 rebalance_on = c(NA, "years", "quarters", 
                                  "months", "weeks", "days"),
                 value = 1, verbose = FALSE, ...)
##R: 수익률, weights: 리밸런싱 시기의 자산별 목표 비중 (미입력: 동일)
##wealth index: 포트폴리오 시작점이 1인 wealth index에 대한 생성여부
##contribution: 포트폴리오 내에서 자산별 성과기여를 나타내는지에 대한 여부
##geometric: 포트폴리오 수익률 계산시 복리(기하)수익률 적용여부
##rebalance_on: weight 값이 미입력 혹은 매번 같은 비중일 경우, 선택가능
##value: 초기 포트폴리오 가치, 디폴트는 1
##verbose: 부가적인 결과(FALSE: 수익률, TRUE:자산 별 성과기여, 비중, 성과)

##목표 비중은 시계열 형태로 입력되어야함.
###1. 행 이름 혹은 인덱스가 날짜 형태로 입력되어야 한다.
###2. 수익률 데이터와 비중 데이터의 열 개수는 동일해야 하며,
###   각 열에 해당하는 자산은 동일해야한다.
###3. 각 시점의 비중의 합은 1이 되어야 한다.
###4. weight에 값을 입력하지 않을 경우, 동일비중 포트폴리오를 구성하며,
###   포트폴리오 리밸런싱은 하지 않습니다.

##verbose = TRUE 출력값
###returns: 포트폴리오 수익률, contribution: 일자별 개별 자산의 포트폴리오 수익률 기여도
###BOP.Weight: 일자별 개별 자산의 포트폴리오 내 비중(시작시점)
###            리밸런싱이 없을 시 직전기간 EOP.Weight와 동일
###EOP.Weight: 일자별 개별 자산의 포트폴리오 내 비중(종료시점)
###BOP.Value:  일자별 개별 자산의 가치(시작시점)
###            리밸런싱이 없을 시 직전 기간 EOP.Value와 동일
###EOP.Value:  일자별 개별 자산의 가치(종료시점)


#전통적인 60대 40 포트폴리오 백테스트 (주식60%, 채권 40%)
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

ticker = c('SPY', 'TLT')
getSymbols(ticker)

prices = do.call(cbind,
                 lapply(ticker, function(x) Ad(get(x))))
rets = Return.calculate(prices) %>% na.omit()

cor(rets)


portfolio = Return.portfolio(R = rets,
                             weights = c(0.6, 0.4),
                             rebalance_on = 'years',
                             verbose = TRUE)


portfolios = cbind(rets, portfolio$returns) %>%
  setNames(c('주식', '채권', '60대 40'))

charts.PerformanceSummary(portfolios,
                          main = '60대 40 포트폴리오')


turnover = xts(
  rowSums(abs(portfolio$BOP.Weight - #lag: 한 단계씩 내림(미룸)
                timeSeries::lag(portfolio$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(portfolio$BOP.Weight))

chart.TimeSeries(turnover)


#시점 선택 전략 백테스트 (시점별 비중이 다른 형태)
##10개월 이평선 vs 주가 비교 (Market timing)
library(quantmod)
library(PerformanceAnalytics)

symbols = c('SPY', 'SHY')
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x))))
rets = na.omit(Return.calculate(prices))


#기본 메커니즘
ep = endpoints(rets, on = 'months') #weeks, quarters, years
###endpoints(x, on= 'months', k=1) #k: 구간 길이
print(ep)


wts = list() #각 시점의 비중이 입력될 리스트
lookback = 10 #n개월 이평선 n=10

i = lookback + 1 #11
sub_price = prices[ep[i-lookback] : ep[i] , 1] #ep[1] : ep[11]

head(sub_price, 3)
tail(sub_price, 3)

sma = mean(sub_price) #평균

wt = rep(0, 2) #wt[1]: 주식의 투자비중, wt[2]: 현금의 투자비중
wt[1] = ifelse(last(sub_price) > sma, 1, 0)
wt[2] = 1 - wt[1]

wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))

##for loop
ep = endpoints(rets, on = 'months')
wts = list()
lookback = 10

for (i in (lookback+1) : length(ep)) { # 11 : 181
  sub_price = prices[ep[i-lookback] : ep[i] , 1]
  sma = mean(sub_price)
  wt = rep(0, 2)
  wt[1] = ifelse(last(sub_price) > sma, 1, 0)
  wt[2] = 1 - wt[1]
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
}

wts = do.call(rbind, wts)


Tactical = Return.portfolio(rets, wts, verbose = TRUE)
portfolios = na.omit(cbind(rets[,1], Tactical$returns)) %>%
  setNames(c('매수 후 보유', '시점 선택 전략'))

charts.PerformanceSummary(portfolios,
                          main = "Buy & Hold vs Tactical")

turnover = xts(rowSums(abs(Tactical$BOP.Weight -
                             timeSeries::lag(Tactical$EOP.Weight)),
                       na.rm = TRUE),
               order.by = index(Tactical$BOP.Weight))

chart.TimeSeries(turnover)



#동적 자산배분 백테스트
##1. 글로벌 10개 자산 중 과거 12개월 수익률이 높은 5개 자산 선택
##2. 최소분산 포트폴리오, 개별 투자비중: 최소 10%, 최대 30% 제약조건
##3. 매월 리밸런싱을 실시
library(quantmod)
library(PerformanceAnalytics)
library(RiskPortfolios)
library(tidyr)
library(dplyr)
library(ggplot2)

symbols = c('SPY', # 미국 주식
            'IEV', # 유럽 주식 
            'EWJ', # 일본 주식
            'EEM', # 이머징 주식
            'TLT', # 미국 장기채
            'IEF', # 미국 중기채
            'IYR', # 미국 리츠
            'RWX', # 글로벌 리츠
            'GLD', # 금
            'DBC'  # 상품
)
getSymbols(symbols, src = 'yahoo')

prices = do.call(cbind, lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

ep = endpoints(rets, on = 'months') #매월 말일의 위치 구하기
wts = list() #매월의 투자비중
lookback = 12 #수익률을 측정할 과거 n개월, n=12
wt_zero = rep(0, 10) %>% setNames(colnames(rets)) #0벡터에 이름넣기


for (i in (lookback+1) : length(ep)) { #13 : 181
  sub_ret = rets[ep[i-lookback] : ep[i] , ]
  cum = Return.cumulative(sub_ret)
  
  K = rank(-cum) <= 5 #수익률 상위 5개 자산 선택 및 내림차순
  covmat = cov(sub_ret[, K]) #상위 5개 자산 분산-공분산 행렬
  
  wt = wt_zero #임시로 비중을 저장하는 곳
  wt[K] = optimalPortfolio(covmat, #최소분산 포트폴리오
                           control = list(type = 'minvol',
                                          constraint = 'user',
                                          LB = rep(0.10, 5),
                                          UB = rep(0.30, 5)))
  
  wts[[i]] = xts(t(wt), order.by = index(rets[ep[i]]))
} #wts의 i번째 리스트에 저장 (구해진 해를 wt의 K번째 값에 입력)

wts = do.call(rbind, wts) #저장된 리스트를 테이블 형태로 바꾸기


GDAA = Return.portfolio(rets, wts, verbose = TRUE)
charts.PerformanceSummary(GDAA$returns, main = '동적자산배분')


wts %>% fortify.zoo() %>%
  gather(key, value, -Index) %>%
  mutate(Index = as.Date(Index)) %>%
  mutate(key = factor(key, levels = unique(key))) %>%
  ggplot(aes(x = Index, y = value)) +
  geom_area(aes(color = key, fill = key),
            position = 'stack') +
  xlab(NULL) + ylab(NULL) +  theme_bw() +
  scale_x_date(date_breaks="years", date_labels="%Y",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, size = 8),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(byrow = TRUE))


GDAA$turnover = xts(
  rowSums(abs(GDAA$BOP.Weight -
                timeSeries::lag(GDAA$EOP.Weight)),
          na.rm = TRUE),
  order.by = index(GDAA$BOP.Weight))

chart.TimeSeries(GDAA$turnover)


fee = 0.0030
GDAA$net = GDAA$returns - GDAA$turnover*fee


cbind(GDAA$returns, GDAA$net) %>%
  setNames(c('No Fee', 'After Fee')) %>%
  charts.PerformanceSummary(main = 'GDAA')