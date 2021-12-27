rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#Quandl Crawling Example
url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl = read.csv(url.aapl) #Quandl은 무료로 이용하는데 한계가 있다.

head(data.aapl)

#Yahoo Finance API Crawling Example
library(quantmod)
getSymbols('AAPL')

head(AAPL)
chart_Series(Ad(AAPL))

## Setting specific time period
Apple = getSymbols('AAPL',
                  from = '1950-01-01', to = '2021-12-31',
                  auto.assign = FALSE)
head(Apple)
chart_Series(Ad(Apple))

`S&P500` = getSymbols('SPY',
                  from = '1950-01-01', to = '2021-12-31',
                  auto.assign = FALSE)
head(`S&P500`)
chart_Series(Ad(`S&P500`))

### Crawling multiple price data at once
ticker = c('FB', 'NVDA')
getSymbols(ticker)

head(FB)
head(NVDA)
chart_Series(Ad(FB))
chart_Series(Ad(NVDA))

#### Crawling Korean stock market data (KOSPI, KOSDAQ)
getSymbols('005930.KS', #KOSPI
           from = '1950-01-01', to = '2021-12-31')
tail(Ad(`005930.KS`))
tail(Cl(`005930.KS`))
View(`005930.KS`)

getSymbols("068760.KQ", #KOSDAQ
           from = '1950-01-01', to = '2021-12-31')
tail(Cl(`068760.KQ`))
View(`068760.KQ`)

#FRED Crawling (Bond interest, Exchange rate)
getSymbols('DGS10', src='FRED') #US Treasury 10yrs interest rate
chart_Series(DGS10)

getSymbols('DEXKOUS', src='FRED')
tail(DEXKOUS)
chart_Series(DEXKOUS)





#Open Dart API
#Renviron
file.edit("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice/.Renviron")
dart_api = '34d13a3f9d403f5e76843e7fffb13c6c43a2f261'

#고유번호 다운로드
library(httr)
library(rvest)

codezip_url = paste0(
  'https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=',dart_api)

codezip_data = GET(codezip_url)
print(codezip_data)

codezip_data$headers[["content-disposition"]]


tf = tempfile(fileext = '.zip')

writeBin(
  content(codezip_data, as = "raw"),
  file.path(tf)
)

nm = unzip(tf, list = TRUE)
print(nm)


library(xml2)

code_data = read_xml(unzip(tf, nm$Name))
print(code_data)


corp_code = code_data %>% html_nodes('corp_code') %>% html_text()
corp_name = code_data %>% html_nodes('corp_name') %>% html_text()
corp_stock = code_data %>% html_nodes('stock_code') %>% html_text()

corp_list = data.frame(
  'code' = corp_code,
  'name' = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = FALSE
)

nrow(corp_list)

head(corp_list)

corp_list = corp_list[corp_list$stock != " ", ]

write.csv(corp_list, 'corp_list.csv')


#공시검색
##전체공시 검색
library(lubridate)
library(stringr)
library(jsonlite)

bgn_date = (Sys.Date() - days(7)) %>% str_remove_all('-')
end_date = (Sys.Date() ) %>% str_remove_all('-')
notice_url = paste0('https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,'&bgn_de=',
                    bgn_date,'&end_de=',end_date,'&page_no=1&page_count=100')

notice_data = fromJSON(notice_url) 
notice_data = notice_data[['list']]

head(notice_data)

##특정기업 공시 검색
bgn_date = (Sys.Date() - days(30)) %>% str_remove_all('-')
end_date = (Sys.Date() ) %>% str_remove_all('-')
corp_code = '00126380'

notice_url_ss = paste0(
  'https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,
  '&corp_code=', corp_code, 
  '&bgn_de=', bgn_date,'&end_de=',
  end_date,'&page_no=1&page_count=100')

notice_data_ss = fromJSON(notice_url_ss) 
notice_data_ss = notice_data_ss[['list']]

head(notice_data_ss)

notice_url_exam = notice_data_ss[1, 'rcept_no']
notice_dart_url = paste0(
  'http://dart.fss.or.kr/dsaf001/main.do?rcpNo=',notice_url_exam)

print(notice_dart_url)

##사업보고서 주요정보
###배당에 관한 사항
corp_code = '00126380' #공시대상회사의 고유번호(8자리)
bsns_year = '2019' #사업연도(4자리)
reprt_code = '11011' #1분기보고서 : 11013, 반기보고서 : 11012
                     #3분기보고서 : 11014, 사업보고서 : 11011

url_div = paste0('https://opendart.fss.or.kr/api/alotMatter.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', corp_code,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code
)

div_data_ss = fromJSON(url_div) 
div_data_ss = div_data_ss[['list']]

head(div_data_ss)


###단일회사 및 다중회사 주요계정
####단일회사 (삼성전자)
corp_code = '00126380'
bsns_year = '2019'
reprt_code = '11011'

url_single = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_single = fromJSON(url_single) 
fs_data_single = fs_data_single[['list']]

head(fs_data_single)

####다중회사 (삼성전자, 셀트리온, KT)
corp_code = c('00126380,00413046,00190321')
bsns_year = '2019'
reprt_code = '11011'

url_multiple = paste0(
  'https://opendart.fss.or.kr/api/fnlttMultiAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_multiple = fromJSON(url_multiple) 
fs_data_multiple = fs_data_multiple[['list']]

fs_data_list = fs_data_multiple %>% split(f = .$corp_code)

lapply(fs_data_list, head, 2)



###단일회사 전체 재무제표
corp_code = '00126380'
bsns_year = 2019
reprt_code = '11011'

url_fs_all = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code,'&fs_div=CFS'
)

fs_data_all = fromJSON(url_fs_all) 
fs_data_all = fs_data_all[['list']]

head(fs_data_all)

yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
yr_name = seq(bsns_year, (bsns_year - yr_count + 1))

fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
  cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])

colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name

head(fs_data_all)



###전 종목 전체 재무제표 데이터 수집하기
library(stringr)

KOR_ticker = read.csv('KOR_ticker.csv', row.names = 1)
corp_list =  read.csv('corp_list.csv', row.names = 1)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

corp_list$'code' =
  str_pad(corp_list$'code', 8, side = c('left'), pad = '0')

corp_list$'stock' =
  str_pad(corp_list$'stock', 6, side = c('left'), pad = '0')

ticker_list = KOR_ticker %>% left_join(corp_list, by = c('종목코드' = 'stock')) %>%
  select('종목코드', '종목명', 'code')

ifelse(dir.exists('dart_fs'), FALSE, dir.create('dart_fs'))



bsns_year = 2019
reprt_code = '11011'

for(i in 1 : nrow(ticker_list) ) {
  
  data_fs = c()
  name = ticker_list$code[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  
  tryCatch({
    
    # url 생성
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', name,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code,'&fs_div=CFS'
    )
    
    # JSON 다운로드
    fs_data_all = fromJSON(url) 
    fs_data_all = fs_data_all[['list']]
    
    # 만일 연결재무제표 없어서 NULL 반환시
    # reprt_code를 OFS 즉 재무제표 다운로드
    if (is.null(fs_data_all)) {
      
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                   dart_api, 
                   '&corp_code=', name,
                   '&bsns_year=', bsns_year,
                   '&reprt_code=', reprt_code,'&fs_div=OFS'
      )
      
      fs_data_all = fromJSON(url) 
      fs_data_all = fs_data_all[['list']]
      
    }
    
    
    # 데이터 선택 후 열이름을 연도로 변경
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year, (bsns_year - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
      cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(fs_data_all, paste0('dart_fs/', ticker_list$종목코드[i], '_fs_dart.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(2)
}