rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#자산별 ETF 수익률 크롤링
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

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

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

#상관계수 표 만들기 (분산-공분산)
library(tidyr)
library(dplyr)
library(corrplot)

cor(rets) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 0.7,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar = c(0,0,0.5,0))

covmat = cov(rets) #분산-공분산

#최소분산 포트폴리오
#slsqp (최적화)
install.packages("nloptr")
library("nloptr")

slsqp(x0, fn, gr = NULL, lower = NULL, upper = NULL,
      hin = NULL, hinjac = NULL, heq = NULL, heqjac = NULL,
      nl.info = FALSE, control = list(), ...)
##x0: 초기값, fn: 목적함수 
##hin: 부등위 제약조건, heq: 등위 제약조건

objective = function(w) {
  obj = t(w) %*% covmat %*% w
  return(obj)
}

hin.objective = function(w) {
  return(w)
  }
  
heq.objective = function(w) {
  sum_w = sum(w)
  return( sum_w - 1 )
  }

result = slsqp( x0 = rep(0.1, 10), #0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
                fn = objective,
                hin = hin.objective,
                heq = heq.objective)

print(result$par) #Weight
print(result$value) #Variance
result$value %>% sqrt() %>% print() #Standard Deviation

w_1 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_1)


#solve.Qp (최적화)
install.packages("quadprog")
library("quadprog")

solve.QP(Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE)
##Dmat: 목적함수 D (분산,공분산 행렬), dvec: 목적함수 d 
##Amat: 제약조건 A^T(비중,비중합), bvec: 제약조건 b_0 (비중>0, 비중합=1) 
##meq: bvec의 몇 번째까지를 등위 제약조건으로 설정할지 (비중합=1)

Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10))) #전치행렬
bvec = c(1, rep(0, 10), -rep(1, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

print(result$solution) #Weight
print(result$value) #Variance
result$value %>% sqrt() %>% print() #Standard Deviation

w_2 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_2)


#optimalPortfolio (최적화)
install.packages("RiskPortfolios")
library("RiskPortfolios")
optimalPortfolio(Sigma, mu = NULL, semiDev = NULL,
                 control = list())
##Sigma: 분산-공분산 행렬, mu: 기대수익률, semiDev: 세미편차
##control:  포트폴리오 종류 및 제약조건
###type (minvol: 최소분산, invvol: 역변동성, erc: 위험균형
###      maxdiv: 최대분산, riskeff: 위험-효율적)
###constraint (lo: 최소 투자 비중이 0보다 큼 (long-only), 
###            user: 최소(LB) 및 최대 투자 비중(UB) 설정)

library(RiskPortfolios)

w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'lo')) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_3)


#최적화 결과 비교 1
library(ggplot2)

data.frame(w_1) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_1)) +
  geom_col() +
  xlab(NULL) + ylab(NULL)


#최소 및 최대 투자비중 제약조건
##slsqp()
result = slsqp( x0 = rep(0.1, 10),
                fn = objective,
                hin = hin.objective,
                heq = heq.objective,
                lower = rep(0.05, 10), #투자비중 5% ~ 20%
                upper = rep(0.20, 10))

w_4 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_4)


##solve.Qp()
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10)))
bvec = c(1, rep(0.05, 10), -rep(0.20, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w_5 = result$solution %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_5)


##optimalPortfolio()
w_6 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'user',
                                      LB = rep(0.05, 10),
                                      UB = rep(0.20, 10))) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w_6)


#최적화 결과 비교 2
data.frame(w_4) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w_4)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)


##각 자산별 제약조건의 추가 1
###solve.Qp()
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10))) 
bvec = c(1, c(0.10, 0.10, 0.05, 0.05, 0.10,
              0.10, 0.05, 0.05, 0.03, 0.03),
         -c(0.25, 0.25, 0.20, 0.20, 0.20,
            0.20, 0.10, 0.10, 0.08, 0.08))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))


#최대분산효과 포트폴리오
#solve.Qp (Duality 최적화)
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(sqrt(diag(covmat)), diag(10)))
bvec = c(1, rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w) #비중의 합이 1을 초과함

w = (w / sum(w)) %>% #비중의 합이 1이 되도록 표준화
  round(., 4)

print(w)

data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_col() +
  xlab(NULL) + ylab(NULL)


#optimalPortfolio()
w = optimalPortfolio(covmat,
                     control = list(type = 'maxdiv',
                                    constraint = 'lo')) %>%
  round(., 4)

print(w)


#최소 및 최대 투자비중 제약조건
#solve.Qp()
Dmat = covmat
dvec = rep(0, 10)
Alb = -rep(0.05, 10) %*% matrix(1, 1, 10) + diag(10) #-lb*e^T+I
Aub = rep(0.20, 10) %*% matrix(1, 1, 10) - diag(10)  # ub*e^T-I

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)


data.frame(w) %>%
  ggplot(aes(x = factor(rownames(.), levels = rownames(.)),
             y = w)) +
  geom_col() +
  geom_hline(aes(yintercept = 0.05), color = 'red') +
  geom_hline(aes(yintercept = 0.20), color = 'red') +
  xlab(NULL) + ylab(NULL)

##각 자산 별 제약조건의 추가 2
###solve.Qp()
Dmat = covmat
dvec = rep(0, 10)
Alb = -c(0.10, 0.10, 0.05, 0.05, 0.10,
         0.10, 0.05, 0.05, 0.03, 0.03) %*%
  matrix(1, 1, 10) + diag(10)
Aub = c(0.25, 0.25, 0.20, 0.20, 0.20,
        0.20, 0.10, 0.10, 0.08, 0.08) %*%
  matrix(1, 1, 10) - diag(10)

Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
bvec = c(1, rep(0, 10), rep(0, 10))
meq = 1

result = solve.QP(Dmat, dvec, Amat, bvec, meq)

w = result$solution 
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets))

print(w)


#위험균형 포트폴리오 (자산별 한계위험기여도가 같은 경우)
get_RC = function(w, covmat) {
  port_vol = t(w) %*% covmat %*% w #w'Ow
  port_std = sqrt(port_vol) #S.D
  
  MRC = (covmat %*% w) / as.numeric(port_std) #Ow/std(w'Ow)
  RC = MRC * w #Ow/std(w'Ow) * w, 한계위험기여도도 비율에 가중됨.
  RC = c(RC / sum(RC))
  
  return(RC)
} #기본 메커니즘

##주식 60%, 채권 40% 포트폴리오의 위험기여도
ret_stock_bond = rets[, c(1, 5)] #미국주식 수익률, 미국 장기채
cov_stock_bond = cov(ret_stock_bond)
RC_stock_bond = get_RC(c(0.6, 0.4), cov_stock_bond)
RC_stock_bond = round(RC_stock_bond, 4)

print(RC_stock_bond)

##rp (최적화)
install.packages("cccp")
library("cccp")


opt = rp(x0 = rep(0.1, 10), #최적화를 위한 초기 입력값 (동일비중)
         P = covmat, #분산-공분산 행렬
         mrc = rep(0.1, 10)) #목표로 하는 각 자산별 위험기여도 값

w = getx(opt) %>% drop() #해를 추출 > 벡터 형태로 변환
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets)) #비중의 합을 1로 만들기

print(w) #자산별 가중비중 확인

get_RC(w, covmat) #자산별 위험기여도 확인



#위험예산 포트폴리오 (자산별 한계위험기여도가 다른 경우)
##rp()
library(cccp)

opt = rp(x0 = rep(0.1, 10), #최적화를 위한 초기 입력값 (동일비중)
         P = covmat, #분산-공분산 행렬
         mrc = c(0.15, 0.15, 0.15, 0.15, 0.10, #목표로 하는 각 자산별 위험기여도 값
                 0.10, 0.05, 0.05, 0.05, 0.05))

w = getx(opt) %>% drop() #해를 추출 > 벡터 형태로 변환
w = (w / sum(w)) %>%
  round(., 4) %>%
  setNames(colnames(rets)) #비중의 합을 1로 만들기

print(w) #자산별 가중비중 확인

get_RC(w, covmat) #자산별 위험기여도 확인




#인덱스 포트폴리오 구성하기
library(stringr)
library(dplyr)

KOR_ticker = read.csv('KOR_ticker.csv',
                      row.names = 1, stringsAsFactors = FALSE) 

KOSPI200 = KOR_ticker %>% filter(시장구분 == 'KOSPI') %>%
  slice(1:200) %>% #1~200행까지 데이터를 선택
  mutate(시가총액비중 = 시가총액 / sum(시가총액))


library(ggplot2)

KOSPI200 %>%  #reorder: 시가총액비중으로 내림차순 정리
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  xlab('종목명') +
  ylab('시가총액비중(%)') +
  scale_y_continuous(labels = scales::percent) #y축을 %로 변경


KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  xlab('종목명') +
  ylab('시가총액비중(로그 스케일링)') +
  scale_y_log10() + #y축을 로그값으로 스케일링
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) + #일부 종목만을 표현함
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  #글자를 회전시키고, 위치를 조정함


##1억으로 KOSPI 200을 복제
KOSPI200 = KOSPI200 %>%
  mutate(매수금액 = 100000000 * 시가총액비중,
             매수주수 = 매수금액 / 종가)

KOSPI200 %>% select(매수금액, 매수주수) %>% head()

KOSPI200 = KOSPI200 %>% mutate(매수주수 = floor(매수주수)) #내림처리
KOSPI200 %>% select(매수금액, 매수주수) %>% head()

inv_money = KOSPI200 %>% mutate(실제매수금액 = 종가 * 매수주수) %>%
  summarize(sum(실제매수금액))

print(inv_money)



#팩터를 이용한 인핸스드 포트폴리오 구성하기
KOSPI200 = KOSPI200 %>% select(종목명, PBR, 시가총액비중) %>%
  mutate(PBR = as.numeric(PBR)) 

##단순가감법
KOSPI200 = KOSPI200 %>% #상위 100종목 +5bp, 나머지 -5bp
  mutate(랭킹 = rank(PBR),
           조절비중 = ifelse(랭킹 <= 100, 시가총액비중 + 0.0005, 시가총액비중 - 0.0005),
           조절비중 = ifelse(조절비중 < 0, 0, 조절비중),
           조절비중 = 조절비중 / sum(조절비중),
           차이 = 조절비중 - 시가총액비중) 

library(tidyr)

head(KOSPI200)
tail(KOSPI200)


KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200, aes(x = reorder(종목명, -시가총액비중), y = 조절비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


KOSPI200_mod = KOSPI200 %>% arrange(PBR) #PBR기준 오름차순

KOSPI200_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /10000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 10000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



#팩터에 대한 전체 종목의 틸트 
#(팩터가 강한 종목의 경우 더욱 많은 비중을 더하고,
# 팩터가 약한 종목의 경우 더욱 많은 비중을 빼는 포트폴리오
KOSPI200_tilt = KOSPI200 %>%
  select(종목명, PBR, 시가총액비중, 랭킹) %>%
  mutate(zscore = -scale(랭킹), #Z-score x (-1)
         cdf = pnorm(zscore), #누적확률분포 구하기
         투자비중 = 시가총액비중 * cdf,
         투자비중 = 투자비중 / sum(투자비중),
         차이 = 투자비중 - 시가총액비중)

head(KOSPI200_tilt)
tail(KOSPI200_tilt)


KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() + #50bp 선 긋기
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


KOSPI200_tilt = KOSPI200_tilt %>%
  mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
  mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
  mutate(투자비중 = 투자비중 / sum(투자비중), 
             차이 = 투자비중 - 시가총액비중)

head(KOSPI200_tilt)


while (max(abs(KOSPI200_tilt$차이)) > (0.005 + 0.00001)) {
  KOSPI200_tilt = KOSPI200_tilt %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 < -0.005, 시가총액비중 - 0.005, 투자비중))) %>%
    mutate_at(vars(투자비중), list(~ifelse(차이 > 0.005, 시가총액비중 + 0.005, 투자비중))) %>%
    mutate(투자비중 = 투자비중 / sum(투자비중), 
               차이 = 투자비중 - 시가총액비중)
}

head(KOSPI200_tilt)


KOSPI200_tilt %>%
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 차이)) +
  geom_point() +
  geom_hline(aes(yintercept = 0.005), color = 'red') + 
  geom_hline(aes(yintercept = -0.005), color = 'red') +
  xlab('종목명') +
  ylab('비중 차이(%)') +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


KOSPI200 %>% 
  ggplot(aes(x = reorder(종목명, -시가총액비중), y = 시가총액비중)) +
  geom_point() +
  geom_point(data = KOSPI200_tilt, aes(x = reorder(종목명, -시가총액비중), y = 투자비중),
             color = 'red', shape = 4) +
  xlab('종목명') +
  ylab('비중(%)') +
  coord_cartesian(ylim = c(0, 0.03)) +
  scale_x_discrete(breaks = KOSPI200[seq(1, 200, by = 5), '종목명']) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


KOSPI200_tilt_mod = KOSPI200_tilt %>% arrange(PBR)

KOSPI200_tilt_mod %>% 
  ggplot(aes(x = reorder(종목명, PBR), y = 차이)) +
  geom_point() +
  geom_col(aes(x = reorder(종목명, PBR), y = PBR /2000), fill = 'blue', alpha = 0.2) +
  xlab('종목명') +
  ylab('차이(%)') +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~. * 2000, name = "PBR")) +
  scale_x_discrete(breaks = KOSPI200_mod[seq(1, 200, by = 10), '종목명']) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))