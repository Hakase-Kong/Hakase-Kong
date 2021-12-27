rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

library(stringr)

KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector = read.csv('KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,'left', 0)
KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)

#join (merge()와 비슷함)
## inner_join() 교집합
## full_join()  합집합
## left_join()  좌측기준
## right_join() 우측기준

library(dplyr)

data_market = left_join(KOR_ticker, KOR_sector,
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))

head(data_market)

glimpse(data_market)

head(names(data_market), 28)

data_market = data_market %>%
  rename(`배당수익률(%)` = `배당수익률`) #rename(tbl, new = old)

head(names(data_market), 28)

##distinct
data_market %>%
  distinct(SEC_NM_KOR) %>% c() #고유한 값 확인


##select
data_market %>%
  select(`종목명`) %>% head()


data_market %>%
  select(`종목명`, `PBR`, `SEC_NM_KOR`) %>% head()


data_market %>%
  select(starts_with('시')) %>% head()


data_market %>%
  select(ends_with('R')) %>% head()


data_market %>%
  select(contains('가')) %>% head()

##mutate (열 생성 및 데이터 변형)
data_market = data_market %>%
  mutate(`PBR` = as.numeric(PBR),
         `PER` = as.numeric(PER),
         `ROE` = PBR / PER,
         `ROE` = round(ROE, 4),
         `size` = ifelse(`시가총액` >=
                           median(`시가총액`, na.rm = TRUE),
                         'big', 'small')
  )

data_market %>%
  select(`종목명`, `ROE`, `size`) %>% head()

###mutate_all()
###mutate_if()
###mutate_at()


##filter (조건을 충족하는 행 선택)
data_market %>%
  select(`종목명`, `PBR`) %>%
  filter(`PBR` < 1) %>% head()


data_market %>%
  select(`종목명`, `PBR`, `PER`, `ROE`) %>%
  filter(PBR < 1 & PER < 20 & ROE > 0.1 ) %>% head()


##summarize (요약 통곗값 계산)
data_market %>%
  summarize(PBR_max = max(PBR, na.rm = TRUE),
            PBR_min = min(PBR, na.rm = TRUE))


##arrange (데이터 정렬)
data_market %>%
  select(PBR) %>%
  arrange(PBR) %>%
  head(5)


data_market %>%
  select(ROE) %>%
  arrange(desc(ROE)) %>% #desc(): 내림차순
  head(5)

##row_number (순위 계산)
data_market %>%
  mutate(PBR_rank = row_number(PBR)) %>%
  select(`종목명`, PBR, PBR_rank) %>%
  arrange(PBR) %>%
  head(5)

data_market %>%
  mutate(ROE_rank = row_number(desc(ROE))) %>%
  select(`종목명`, ROE, ROE_rank) %>%
  arrange(desc(ROE)) %>% #desc(): 내림차순
  head(5)

###min_rank()
###dense_rank()
###percent_rank()


##ntile (분위수 계산)
data_market %>%
  mutate(PBR_tile = ntile(PBR, n = 5)) %>%
  select(PBR, PBR_tile) %>%
  head()

data_market %>%
  mutate(PBR_tile = ntile(PBR, n = 10)) %>%
  select(PBR, PBR_tile) %>%
  head()


##group_by (그룹별로 데이터 묶기)
data_market %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n()) #섹터별 숫자


data_market %>%
  group_by(`SEC_NM_KOR`) %>% 
  summarize(PBR_median = median(PBR, na.rm = TRUE)) %>%
  arrange(PBR_median) #섹터별 PBR 중간값 랭킹


data_market %>%
  group_by(`시장구분`, `SEC_NM_KOR`) %>%
  summarize(PBR_median = median(PBR, na.rm = TRUE)) %>%
  arrange(PBR_median) #시장별, 섹터별 PBR 중간값 랭킹