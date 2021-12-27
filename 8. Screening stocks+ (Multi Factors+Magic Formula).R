rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#랭킹 상위 종목 섹터정보 확인
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)

KOR_price = read.csv('KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {
  prod(1+x) - 1
})

invest_mom = rank(-ret_12m) <= 30


KOR_sector = read.csv('KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)
data_market = left_join(KOR_ticker, KOR_sector,
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))

data_market[invest_mom, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic()


##섹터 중립 포트폴리오
sector_neutral = data_market %>%
  select(`종목코드`, `SEC_NM_KOR`) %>%
  mutate(`ret` = ret_12m) %>%
  group_by(`SEC_NM_KOR`) %>%
  mutate(scale_per_sector = scale(`ret`), #그룹별 정규화
         scale_per_sector = ifelse(is.na(`SEC_NM_KOR`),
                                   NA, scale_per_sector))

invest_mom_neutral =
  rank(-sector_neutral$scale_per_sector) <= 30 #내림차순

data_market[invest_mom_neutral, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic()



#마법공식
library(stringr)
library(dplyr)

KOR_value = read.csv('KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('KOR_fs.Rds')
KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

data_pbr = KOR_value['PBR']

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col = str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

data_gpa =
  (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col] %>%
  setNames('GPA')

cbind(data_pbr, -data_gpa) %>% #PBR 오름차순, GPA 내림차순
  cor(method = 'spearman', use = 'complete.obs') %>% round(4)


cbind(data_pbr, data_gpa) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>% #5분위
  filter(!is.na(quantile_pbr)) %>% #PBR NA 제외
  group_by(quantile_pbr) %>% #PBR 분위수별 그룹 묶어주기
  summarise(mean_gpa = mean(GPA, na.rm = TRUE)) %>% #그룹별 평균값
  ggplot(aes(x = quantile_pbr, y = mean_gpa)) + #시각화
  geom_col() +
  xlab('PBR') + ylab('GPA')


###
library(stringr)
library(dplyr)

KOR_value = read.csv('KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
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

# 분자
magic_ebit = (KOR_fs$'지배주주순이익' + KOR_fs$'법인세비용' +
                KOR_fs$'이자비용')[num_col]

# 분모
magic_cap = KOR_value$PER * KOR_fs$'지배주주순이익'[num_col]
magic_debt = KOR_fs$'부채'[num_col]
magic_excess_cash_1 = KOR_fs$'유동부채' - KOR_fs$'유동자산' +
  KOR_fs$'현금및현금성자산'
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 =
  (KOR_fs$'현금및현금성자산' - magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_debt - magic_excess_cash_2

# 이익수익률
magic_ey = magic_ebit / magic_ev


# 투하자본 (분모)
magic_ic = ((KOR_fs$'유동자산' - KOR_fs$'유동부채') +
              (KOR_fs$'비유동자산' - KOR_fs$'감가상각비'))[num_col]
magic_roc = magic_ebit / magic_ic


###

invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30 #내림차순

KOR_ticker[invest_magic, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`이익수익률` = round(magic_ey[invest_magic, ], 4),
         `투하자본수익률` = round(magic_roc[invest_magic, ], 4))




#이상치 데이터 제거 (Outlier)
library(magrittr)
library(ggplot2)

KOR_value = read.csv('KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)

max(KOR_value$PBR, na.rm = TRUE)

KOR_value %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)

##Trimming (잘 사용되지 않음)
library(dplyr)

value_trim = KOR_value %>% #상하위 1% 데이터는 NA로 변경
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99, NA, PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01, NA, PBR))

value_trim %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)

##Winsorizing (이상치 데이터 대체)
value_winsor = KOR_value %>% #상하위 1% 초과데이터는 1%로 변형
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99,
                      quantile(., 0.99, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01,
                      quantile(., 0.01, na.rm = TRUE), PBR))

value_winsor %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)



#팩터 결합
library(tidyr)

KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  

##Z-Score로 정규화
KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  


##멀티 팩터 포트폴리오
library(xts)
library(stringr)

KOR_fs = readRDS('KOR_fs.Rds')
KOR_value = read.csv('KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_price = read.csv('KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

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

factor_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_quality %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()


factor_value = KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_value %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()


library(PerformanceAnalytics)
library(dplyr)

ret_3m = Return.calculate(KOR_price) %>% xts::last(60) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_6m = Return.calculate(KOR_price) %>% xts::last(120) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_12m = Return.calculate(KOR_price) %>% xts::last(252) %>%
  sapply(., function(x) {prod(1+x) - 1})
ret_bind = cbind(ret_3m, ret_6m, ret_12m) %>% data.frame()

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_mom %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()


library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))

factor_qvm =
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%
  rowSums()

invest_qvm = rank(factor_qvm) <= 30


library(tidyr)

cbind(quality_profit, invest_qvm) %>%
  gather(key, value, -invest_qvm) %>%
  mutate(value = ifelse(percent_rank(value) > 0.95, NA, value),
         value = ifelse(percent_rank(value) < 0.05, NA, value)) %>%
  ggplot(aes(x = value, fill = invest_qvm)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  theme(legend.position = "bottom") +
  xlab('') + ylab('')


cbind(KOR_value, invest_qvm) %>%
  gather(key, value, -invest_qvm) %>%
  mutate(value = ifelse(percent_rank(value) > 0.95, NA, value),
         value = ifelse(percent_rank(value) < 0.05, NA, value)) %>%
  ggplot(aes(x = value, fill = invest_qvm)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  theme(legend.position = "bottom") +
  xlab('') + ylab('')


cbind(ret_bind, invest_qvm) %>%
  gather(key, value, -invest_qvm) %>%
  mutate(value = ifelse(percent_rank(value) > 0.95, NA, value),
         value = ifelse(percent_rank(value) < 0.05, NA, value)) %>%
  ggplot(aes(x = value, fill = invest_qvm)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  theme(legend.position = "bottom") +
  xlab('') + ylab('')


KOR_ticker[invest_qvm, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm, ], 2)) %>%
  cbind(round(KOR_value$PBR[invest_qvm], 2)) %>%
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M'))

cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()