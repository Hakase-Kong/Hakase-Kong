rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#KRX Industry sector categorization

##KOSPI
library(httr)
library(rvest)
library(readr)

gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  mktId = 'STK',
  trdDd = '20211213', #Today Date
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>% #Send Query to URL
  read_html() %>% #Read html
  html_text()  #Extract text in HTML

down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
print(down_sector_KS)


##KOSDAQ
gen_otp_data = list(
  mktId = 'KSQ', # 코스닥으로 변경
  trdDd = '20211213',
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_sector_KQ = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
print(down_sector_KQ)
down_sector = rbind(down_sector_KS, down_sector_KQ)

write.csv(down_sector, 'krx_sector.csv')


#Individual stock index crawling
library(httr)
library(rvest)
library(readr)

gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = '20211213', #Today Date
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
print(down_ind)
write.csv(down_ind, 'krx_ind.csv')


#Naver Finance Recent data Crawling (Xpath) Test
library(httr)
library(rvest)
library(stringr)

url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_0"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

print(biz_day)

#################################################################

#Recent data Crawling (Xpath + OTP)
library(httr)
library(rvest)
library(stringr)
library(readr)

# 최근 영업일 구하기
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

# 코스피 업종분류 OTP 발급
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  mktId = 'STK',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스피 업종분류 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

# 코스닥 업종분류 OTP 발급
gen_otp_data = list(
  mktId = 'KSQ',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스닥 업종분류 데이터 다운로드
down_sector_KQ = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

down_sector = rbind(down_sector_KS, down_sector_KQ)

write.csv(down_sector, 'krx_sector.csv')

# 개별종목 지표 OTP 발급
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = biz_day, # 최근영업일로 변경
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 개별종목 지표 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'krx_ind.csv')


# 데이터 정리하기
down_sector = read.csv('krx_sector.csv', row.names = 1,
                       stringsAsFactors = FALSE)
down_ind = read.csv('krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)

intersect(names(down_sector), names(down_ind)) #중복열 찾기

setdiff(down_sector[, '종목명'], down_ind[ ,'종목명']) #차이점

KOR_ticker = merge(down_sector, down_ind, #공통항목 종목만 합체
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE #TRUE: 합집합, False: 교집합
)

KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액']), ]
print(head(KOR_ticker)) #시가총액 내림차순 정리 (R은 오름차순 기본)

KOR_ticker[grepl('스팩', KOR_ticker[, '종목명']), '종목명'] #스팩, 우선주 제거
KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) != 0, '종목명']

KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]

rownames(KOR_ticker) = NULL
write.csv(KOR_ticker, 'KOR_ticker.csv')

#WICS Sector Information Crawling (Test)

library(jsonlite)

url = 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20211213&sec_cd=G10'
data = fromJSON(url)

lapply(data, head)

#WICS Sector Information Crawling

sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector = do.call(rbind, data_sector)
write.csv(data_sector, 'KOR_sector.csv')

#Naver Finance Adjusted price Crawling (Sansung Electronics)
https://api.finance.naver.com/siseJson.naver?symbol=005930&requestType=0&count=300&timeframe=day

library(stringr)

KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

library(xts)

ifelse(dir.exists('KOR_price'), FALSE,
       dir.create('KOR_price'))

i = 1
name = KOR_ticker$'종목코드'[i]

price = xts(NA, order.by = Sys.Date())
print(price)


library(httr)
library(rvest)
library(lubridate)
library(stringr)
library(readr)

from = (Sys.Date() - years(3)) %>% str_remove_all('-') #Time Period
to = Sys.Date() %>% str_remove_all('-')

url = paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=', name,
             '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')

data = GET(url)
data_html = data %>% read_html %>%
  html_text() %>%
  read_csv()

print(data_html)

#Testing graphing Samsung Electronics
test = data_html[c(1:6)]
colnames(test) = (c('Date','Open','High','Low','Close','Volume'))
test = na.omit(test)
test$Date = parse_number(test$Date)
test$Date = ymd(test$Date)
test = tk_xts(test, date_var = Date)

chartSeries(test,
            type="candlesticks",subset="2020-01/2021-12",
            name="Samsung Electronics",theme=chartTheme("white"),
            show.grid=F,major.ticks="months",minor.ticks=F,
            up.col="red",dn.col="blue",color.vol=F)
addSMA(n=10,on=1,col="black")
addMACD() #  add MACD indicator to current chart
chartSeries(test,TA=c(addVo(),addBBands())) #add volume and Bollinger Bands from TTR

chartSeries(test)
chartSeries(test, subset='last 4 months')
chartSeries(test, subset='2019::2020-01')
chartSeries(test,theme=chartTheme('white'))
chartSeries(test,TA=NULL)   #no volume

setTA(test)
chartSeries(test)   #draws chart again, this time will all indicators present
############################################################################

library(timetk)

price = data_html[c(1, 5)]
colnames(price) = (c('Date', 'Price'))
price = na.omit(price)
price$Date = parse_number(price$Date) #Eliminate , Only number
price$Date = ymd(price$Date) #yyyymmdd -> yyyy-mm-dd
price = tk_xts(price, date_var = Date) #Index (date), Date column X

print(tail(price))

write.csv(data.frame(price),
          paste0('KOR_price/', name, '_price.csv'))

#Naver Finance Adjusted price Crawling (Every stocks)
library(httr)
library(rvest)
library(stringr)
library(xts)
library(lubridate)
library(readr)
library(timetk)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name = KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  from = (Sys.Date() - years(3)) %>% str_remove_all('-') # 시작일
  to = Sys.Date() %>% str_remove_all('-') # 종료일
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url = paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=', name,
                 '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')
    
    # 이 후 과정은 위와 동일함
    # 데이터 다운로드
    data = GET(url)
    data_html = data %>% read_html %>%
      html_text() %>%
      read_csv()
    
    # 필요한 열만 선택 후 클렌징
    price = data_html[c(1, 5)]
    colnames(price) = (c('Date', 'Price'))
    price = na.omit(price)
    price$Date = parse_number(price$Date)
    price$Date = ymd(price$Date)
    price = tk_xts(price, date_var = Date)
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 폴더 내 csv 파일로 저장
  write.csv(data.frame(price),
            paste0('KOR_price/', name, '_price.csv'))
  
  # 타임슬립 적용
  Sys.sleep(2)
}

#Naver Finance Fundamental data Crawling (Samsung Electronics)
library(httr)
library(rvest)

ifelse(dir.exists('KOR_fs'), FALSE,
       dir.create('KOR_fs'))

Sys.setlocale("LC_ALL", "English")

url = paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
data = data %>%
  read_html() %>%
  html_table()

Sys.setlocale("LC_ALL", "Korean")

lapply(data, function(x) {
  head(x, 3)})

##
data_IS = data[[1]] #year
data_BS = data[[3]]
data_CF = data[[5]]

print(names(data_IS))
data_IS = data_IS[, 1:(ncol(data_IS)-2)]

data_fs = rbind(data_IS, data_BS, data_CF) %>% data.frame()
data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                    '', data_fs[, 1])
data_fs = data_fs[!duplicated(data_fs[, 1]), ]

rownames(data_fs) = NULL
rownames(data_fs) = data_fs[, 1]
data_fs[, 1] = NULL

data_fs = data_fs[, substr(colnames(data_fs), 7, 8) == '12'] #
print(head(data_fs))

sapply(data_fs, typeof)


data_fs = sapply(data_fs, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))

print(head(data_fs))

sapply(data_fs, typeof)

write.csv(data_fs, 'KOR_fs/005930_fs.csv')

##
data_IS = data[[2]] #quarter
data_BS = data[[4]]
data_CF = data[[6]]

print(names(data_IS))
data_IS = data_IS[, 1:(ncol(data_IS)-2)]

data_fs = rbind(data_IS, data_BS, data_CF) %>% data.frame()
data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                    '', data_fs[, 1])
data_fs = data_fs[!duplicated(data_fs[, 1]), ]

rownames(data_fs) = NULL
rownames(data_fs) = data_fs[, 1]
data_fs[, 1] = NULL

data_fs = data_fs[, substr(colnames(data_fs), 7, 8) == '12']
print(head(data_fs))

sapply(data_fs, typeof)


data_fs = sapply(data_fs, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))

print(head(data_fs))

sapply(data_fs, typeof)

write.csv(data_fs, 'KOR_fs/005930_fs.csv')


#Relative Valuation (Samsung Electronics)
ifelse(dir.exists('KOR_value'), FALSE,
       dir.create('KOR_value'))

value_type = c('지배주주순이익',
               '자본',
               '영업활동으로인한현금흐름',
               '매출액')

value_index = data_fs[match(value_type, rownames(data_fs)),
                      ncol(data_fs)]
print(value_index)


url = 'http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930'
data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))

price = read_html(data) %>%
  html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
  html_text() %>%
  parse_number()

print(price)


share = read_html(data) %>%
  html_node(
    xpath =
      '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
  html_text()

print(share) #Ordinary / Primary

share = share %>%
  strsplit('/') %>%
  unlist() %>%
  .[1] %>%
  parse_number()

print(share)

data_value = price / (value_index * 100000000 / share)
names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
data_value[data_value < 0] = NA

print(data_value)

write.csv(data_value, 'KOR_value/005930_value.csv')



#Fundamental data and Relative Valuation Crawling (Every stocks)
library(stringr)
library(httr)
library(rvest)
library(readr)

KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')

ifelse(dir.exists('KOR_fs'), FALSE,
       dir.create('KOR_fs'))
ifelse(dir.exists('KOR_value'), FALSE,
       dir.create('KOR_value'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    
    Sys.setlocale('LC_ALL', 'English')
    
    # url 생성
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    
    # 이 후 과정은 위와 동일함
    
    # 데이터 다운로드 후 테이블 추출
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                          AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    
    Sys.setlocale('LC_ALL', 'Korean')
    
    # 3개 재무제표를 하나로 합치기
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs = rbind(data_IS, data_BS, data_CF) %>% data.frame()
    
    # 데이터 클랜징
    data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                        '', data_fs[, 1])
    data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[, 1]
    data_fs[, 1] = NULL
    
    # 12월 재무제표만 선택
    data_fs =
      data_fs[, substr(colnames(data_fs), 7, 8) == "12"]
    
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    
    
    # 가치지표 분모부분
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    
    # 해당 재무데이터만 선택
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)]
    
    # Snapshot 페이지 불러오기
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
    
    # 현재 주가 크롤링
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    
    # 보통주 발행주식수 크롤링
    share = read_html(data) %>%
      html_node(
        xpath =
          '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    
    # 가치지표 계산
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(data_fs, paste0('KOR_fs/', name, '_fs.csv'))
  
  # 가치지표 저장
  write.csv(data_value, paste0('KOR_value/', name,
                               '_value.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(2)
}