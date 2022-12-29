# 5-1 speech_park.txt를 불러와 분석에 적합하게 전처리한 다음 연설문에서 명사 추출.
setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
library(tidyr)
library(tidytext)
library(tidyverse)

raw_speech <- readLines("speech_park.txt", encoding = "UTF-8")

speech <- raw_speech %>%
  str_replace_all("[^가-힣]", " ") %>%
  str_squish() %>%
  as_tibble() %>%
  filter(value!='')

noun_token <- speech %>%
  unnest_tokens(input = value,
                output= word,
                token = extractNoun) %>%
  count(word, sort = T) %>%
  filter(str_length(word) > 1)
noun_token


# 5-2 가장 자주 사용된 단어 20개 추출.
word_count <- speech %>%
  unnest_tokens(input = value,
                output= word,
                token = 'words') %>%
  count(word, sort = T) %>%
  filter(str_length(word) > 1)%>%
  head(20)
word_count



# 5-3 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프 만들기.
word_count %>%
  ggplot(aes(x=reorder(word, n), y = n)) +
  geom_col(colour='blue', fill='skyblue') +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.4) +
  labs(title = '박근혜 대통령 출마 연설문 단어 빈도',
       x = NULL, y = NULL)


# 5-4 전처리 하지 않은 연설문에서 연속된 공백을 제거하고 tibble 구조로 변환한 다음 문장 기준으로 토큰화.
st_speech <- raw_speech %>%
  str_squish() %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = 'sentences')
st_speech


# 5-5 연설문에서 '경제가 사용된 문장 출력.
a <- st_speech %>% 
  filter(str_detect(string = sentence, 
                    pattern = '경제'))
a