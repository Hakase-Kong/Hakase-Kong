rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice")

#ggplot2 making graph + plotting
library(ggplot2)

data(diamonds)
head(diamonds)

ggplot(data = diamonds, aes(x = carat, y = price))

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point()

#ggplot2 plotting + color
library(magrittr)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut))


#ggplot2 Facets (Categorizing)
diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(. ~ cut)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(. ~ cut)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  facet_grid(. ~ cut)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  facet_wrap(. ~ cut)

#ggplot2 Statistics
diamonds %>%
  ggplot(aes(x = cut, y = carat)) +
  stat_summary_bin(fun.y = 'mean', geom = 'bar')

#ggplot2 Coordinates
diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 20000))

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut)) +
  coord_flip()


#ggplot2 Theme
diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  theme_bw() + #Background color = White
  labs(title = 'Relation between Carat & Price', #Title
       x = 'Carat', y = 'Price') +  #Name of X, Y axis
  theme(legend.position = 'bottom', #Legend
        panel.grid.major.x = element_blank(), #Eliminating
        panel.grid.minor.x = element_blank(), #the
        panel.grid.major.y = element_blank(), #panel
        panel.grid.minor.y = element_blank()  #grid
  ) +
  scale_y_continuous( #Adding , to the 10^3 and append $
    labels = function(x) {
      paste0('$', 
             format(x, big.mark = ','))
    })


#ggplot2 geom_point (산점도 나타내기)
library(ggplot2)

ggplot(data_market, aes(x = ROE, y = PBR)) +
  geom_point()

ggplot(data_market, aes(x = ROE, y = PBR)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 0.30), ylim = c(0, 3))



ggplot(data_market, aes(x = ROE, y = PBR,
                        color = `시장구분`,
                        shape = `시장구분`)) +
  geom_point() +
  geom_smooth(method = 'lm') + #평활선 추가 (glm, gam, loess etc.)
  coord_cartesian(xlim = c(0, 0.30), ylim = c(0, 3))


#ggplot2 geom_histogram (히스토그램 나타내기)
ggplot(data_market, aes(x = PBR)) +
  geom_histogram(binwidth = 0.1) + #binwidth: 막대의 너비
  coord_cartesian(xlim = c(0, 10))


ggplot(data_market, aes(x = PBR)) +
  geom_histogram(aes(y = ..density..), #밀도함수
                 binwidth = 0.1,
                 color = 'sky blue', fill = 'sky blue') + 
  coord_cartesian(xlim = c(0, 10)) +
  geom_density(color = 'red') + #밀도곡선
  geom_vline(aes(xintercept = median(PBR, na.rm = TRUE)), #세로선
             color = 'blue') + #PBR의 중간값으로...
  geom_text(aes(label = median(PBR, na.rm = TRUE), #글자쓰기
                x = median(PBR, na.rm = TRUE), y = 0.05),
            col = 'black', size = 6, hjust = -0.5)


#ggplot2 geom_boxplot (박스 플롯 나타내기)
ggplot(data_market, aes(x = SEC_NM_KOR, y = PBR)) +
  geom_boxplot() +
  coord_flip() #x,y축 반전


#ggplot2 geom_bar (막대 그래프 나타내기)
data_market %>%
  group_by(SEC_NM_KOR) %>% #섹터별로 묶어주기
  summarize(n = n()) %>% #그룹별 데이터 개수 구하기
  ggplot(aes(x = SEC_NM_KOR, y = n)) +
  geom_bar(stat = 'identity') + #n 데이터를 그대로 이용
  theme_classic() #배경 테마


data_market %>%
  filter(!is.na(SEC_NM_KOR)) %>% #NA항목 삭제
  group_by(SEC_NM_KOR) %>% #섹터별로 묶어주기
  summarize(n = n()) %>% #그룹별 데이터 개수 구하기
  ggplot(aes(x = reorder(SEC_NM_KOR, n), y = n, label = n)) +
  geom_bar(stat = 'identity') +
  geom_text(color = 'black', size = 4, hjust = -0.3) + #종목개수
  xlab(NULL) + #라벨 삭제
  ylab(NULL) + #라벨 삭제
  coord_flip() + #x,y축 반전
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + #그림의 간격넓히기
  theme_classic()


#dplyr + ggplot
data_market %>%
  filter(!is.na(SEC_NM_KOR)) %>%
  group_by(SEC_NM_KOR) %>%
  summarize(ROE_sector = median(ROE, na.rm = TRUE),
            PBR_sector = median(PBR, na.rm = TRUE))

data_market %>%
  filter(!is.na(SEC_NM_KOR)) %>%
  group_by(SEC_NM_KOR) %>%
  summarize(ROE_sector = median(ROE, na.rm = TRUE),
            PBR_sector = median(PBR, na.rm = TRUE)) %>%
  ggplot(aes(x = ROE_sector, y = PBR_sector,
             color = SEC_NM_KOR, label = SEC_NM_KOR)) +
  geom_point() +
  geom_text(color = 'black', size = 3, vjust = 1.3) +
  theme(legend.position = 'bottom',
        legend.title = element_blank())


#Stock Price Graphing
library(quantmod)

getSymbols('SPY')
prices = Cl(SPY)
plot(prices, main = 'Price')

getSymbols('SPY',
           from = '1950-01-01', to = '2021-12-31')
prices = Cl(SPY)
plot(prices, main = 'Price')

##ggplot2
library(ggplot2)

SPY %>%
  ggplot(aes(x = Index, y = SPY.Close)) +
  geom_line()



##Interactive Charting!!
##dygraphs
library(dygraphs)

dygraph(prices) %>%
  dyRangeSelector()


##highcharter
library(highcharter)

highchart(type = 'stock') %>%
  hc_add_series(prices) %>%
  hc_scrollbar(enabled = FALSE)


##plotly
library(plotly)

p = SPY %>%
  ggplot(aes(x = Index, y = SPY.Close)) +
  geom_line()

ggplotly(p)

###파이프 오퍼레이터 연결가능
prices %>%
  fortify.zoo %>%
  plot_ly(x= ~Index, y = ~SPY.Close ) %>% #x,y축 설정
  add_lines()



#연도별 수익률 나타내기
library(PerformanceAnalytics)

ret_yearly = prices %>%
  Return.calculate() %>%
  apply.yearly(., Return.cumulative) %>% #연도별 수익률 구하기
  round(4) %>% #반올림
  fortify.zoo() %>% #인덱스에 있는 시간 데이터를 Index 열로 이동
  mutate(Index = as.numeric(substring(Index, 1, 4))) #연도만 남기기

ggplot(ret_yearly, aes(x = Index, y = SPY.Close)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = ret_yearly$Index, #x축에 연도적기
                     expand = c(0.01, 0.01)) +
  geom_text(aes(label = paste(round(SPY.Close * 100, 2), "%"),
                vjust = ifelse(SPY.Close >= 0, -0.5, 1.5)),
            position = position_dodge(width = 1),
            size = 3) + #수익률이 0보다 크면 위쪽, 작으면 아래쪽
  xlab(NULL) + ylab(NULL) #라벨 제거