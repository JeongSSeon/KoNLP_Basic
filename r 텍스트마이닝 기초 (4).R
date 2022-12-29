# 4-(1) speech_park.txt를 불러와 분석에 맞게 전처리한 다음 띄어쓰기 기준으로 토큰화.
library(tidytext)
a <- readLines("speech_park.txt", encoding = "UTF-8")
speech <- tibble(value = a)
word_token <- speech %>% unnest_tokens(input = value, output= word, token = 'words')
word_token

# 4-(2) 가장 자주 사용된 단어 20개를 추출.
word_count <- word_token %>%
  count(word, sort = T) %>%
  filter(str_length(word) > 1)%>%
  head(20)
word_count

# 4-(3) 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대그래프 만들기.
word_count %>%
  ggplot(aes(x=reorder(word, n), y = n)) +
  geom_col(colour='blue', fill='skyblue') +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.4) +
  labs(title = '박근혜 대통령 출마 연설문 단어 빈도',
       x = NULL, y = NULL) +
  theme_set(theme_gray(base_family = 'nanumgothic'))




a %>% as.data.frame()
write.csv(a, "moon_20170279_박정선.csv")

