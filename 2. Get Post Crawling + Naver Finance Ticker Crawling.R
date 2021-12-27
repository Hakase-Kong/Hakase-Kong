rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#Naver Finance Headline Crawling (GET)
library(rvest)
library(httr)

url1 = 'https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258'
data1 = GET(url1)

print(data1)

data_title = data1 %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes('dl') %>%
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_attr('title')
print(data_title)

#KRX Today's Listing  Crawling (POST)
library(httr)
library(rvest)

Sys.setlocale("LC_ALL", "English")
url2 = 'https://kind.krx.co.kr/disclosure/todaydisclosure.do'
data2 = POST(url2, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'N',
                selDate = '2021-12-17'
              ))

data2 = read_html(data2) %>%
  html_table(fill = TRUE) %>%
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")
print(head(data2))



#Naver Finance Ticker Crawling (Test)
library(httr)
library(rvest)

i = 0
ticker = list()
url4 = paste0('https://finance.naver.com/sise/',
             'sise_market_sum.nhn?sosok=',i,'&page=1')
down_table = GET(url4)

navi.final = read_html(down_table, encoding = 'EUC-KR') %>%
  html_nodes(., '.pgRR') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')
print(navi.final)

navi.final = navi.final %>%
  strsplit(., '=') %>%
  unlist() %>%
  tail(., 1) %>%
  as.numeric()
print(navi.final)


i = 0 # 코스피
j = 1 # 첫번째 페이지
url5 = paste0('https://finance.naver.com/sise/',
             'sise_market_sum.nhn?sosok=',i,"&page=",j)
down_table1 = GET(url5)

Sys.setlocale("LC_ALL", "English")

table = read_html(down_table1, encoding = "EUC-KR") %>%
  html_table(fill = TRUE)

table1 = table[[1]]
table2 = table[[2]]
table3 = table[[3]]

Sys.setlocale("LC_ALL", "Korean")
print(head(table1))
print(head(table2))
print(head(table3))

table2[, ncol(table2)] = NULL
table2 = na.omit(table2)
print(head(table2))


symbol = read_html(down_table1, encoding = 'EUC-KR') %>%
  html_nodes(., 'tbody') %>%
  html_nodes(., 'td') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')

print(head(symbol, 10))

library(stringr)

symbol = sapply(symbol, function(x) {
  str_sub(x, -6, -1) 
})

print(head(symbol, 10))

symbol = unique(symbol)
print(head(symbol, 10))

table2$N = symbol
colnames(table2)[1] = '종목코드'

rownames(table2) = NULL
ticker[[j]] = table2



#Naver Finance Ticker Crawling
rm(list=ls())
data = list()
i = 0 # 코스피
j = 1 # 첫번째 페이지

# i = 0 은 코스피, i = 1 은 코스닥 종목
for (i in 0:1) {
  
  ticker = list()
  url =
    paste0('https://finance.naver.com/sise/',
           'sise_market_sum.nhn?sosok=',i,'&page=1')
  
  down_table = GET(url)
  
  # 최종 페이지 번호 찾아주기
  navi.final = read_html(down_table, encoding = "EUC-KR") %>%
    html_nodes(., ".pgRR") %>%
    html_nodes(., "a") %>%
    html_attr(.,"href") %>%
    strsplit(., "=") %>%
    unlist() %>%
    tail(., 1) %>%
    as.numeric()
  
  # 첫번째 부터 마지막 페이지까지 for loop를 이용하여 테이블 추출하기
  for (j in 1:navi.final) {
    
    # 각 페이지에 해당하는 url 생성
    url = paste0(
      'https://finance.naver.com/sise/',
      'sise_market_sum.nhn?sosok=',i,"&page=",j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL", "English")
    # 한글 오류 방지를 위해 영어로 로케일 언어 변경
    
    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    table = table[[2]] # 원하는 테이블 추출
    
    Sys.setlocale("LC_ALL", "Korean")
    # 한글을 읽기위해 로케일 언어 재변경
    
    table[, ncol(table)] = NULL # 토론식 부분 삭제
    table = na.omit(table) # 빈 행 삭제
    
    # 6자리 티커만 추출
    symbol = read_html(down_table, encoding = "EUC-KR") %>%
      html_nodes(., "tbody") %>%
      html_nodes(., "td") %>%
      html_nodes(., "a") %>%
      html_attr(., "href")
    
    symbol = sapply(symbol, function(x) {
      str_sub(x, -6, -1) 
    })
    
    symbol = unique(symbol)
    
    # 테이블에 티커 넣어준 후, 테이블 정리
    table$N = symbol
    colnames(table)[1] = "종목코드"
    
    rownames(table) = NULL
    ticker[[j]] = table
    
    Sys.sleep(0.5) # 페이지 당 0.5초의 슬립 적용
  }
  
  # do.call을 통해 리스트를 데이터 프레임으로 묶기
  ticker = do.call(rbind, ticker)
  data[[i + 1]] = ticker
}

# 코스피와 코스닥 테이블 묶기
data = do.call(rbind, data)