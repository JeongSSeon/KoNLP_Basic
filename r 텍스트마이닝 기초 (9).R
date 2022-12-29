library(tidyverse)
library(tidytext)
library(tidyselect)
library(tidygraph)
library(Sejong)
library(showtext)
library(rmarkdown)
library(reprex)
library(readxl)
library(NIADic)
library(magrittr)
library(KoNLP)
library(knitr)
library(ggrepel)
library(ggraph)

setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
raw_news_comment <- read_csv("news_comment_BTS.csv")



#Q1. "news_comment_BTS.csv"를 불러온 다음 행 번호를 나타낸 변수를 추가하고 분석에 적합하게 전처리하기.

news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number()) %>% print()



#Q2. 댓글에서 명사, 동사, 형용사를 추출하고 "/"으로 시작하는 모든 문자를 "다"로 바꾸기.

library(KoNLP)
comment_pos <- news_comment %>%
  unnest_tokens(input = reply, output = word,
                token = SimplePos22, drop = F) %>%
  separate_rows(word, sep = "[+]") %>% print()

comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"), 
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_length(word) >= 2) %>%
  arrange(id) %>% print() 



#Q3. 다음 코드를 이용하여 유사어를 통일한 다음 한 댓글이 하나의 행이 되도록 단어를 결합하기.

comment_new <- comment_new %>%
  mutate(word = ifelse(str_detect(word, "축하"), "축하", word),
         word = ifelse(str_detect(word, "방탄"), "방탄", word),
         word = ifelse(str_detect(word, "대단"), "대단", word),
         word = ifelse(str_detect(word, "자랑"), "자랑", word))

#comment_new <- comment_new %>%
#               mutate(word = ifelse(str_detect(word, "방탄") &
#                                    !str_detect(word, "방탄복"), "방탄", word), #방탄복은 그대로 놔두기
#                      word = ifelse(word == "대단해", "대단", word),
#                      word = ifelse(word == "자랑스러워", "자랑", word),
#                      word = ifelse(str_detect(word, "축하"), "축하", word))

# 한 댓글이 하나의 행이 되도록 결합
line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " ")) %>% print()





#Q4. 댓글을 바이그램으로 토큰화 한 다음 바이그램 단어쌍 분리하기.

# 바이그램 생성
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = 'ngrams',
                n = 2) %>% print()

# 바이그램 분리
bigram_seperated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% print()




#Q5. 단어쌍 빈도를 구한 다음 네트워크 그래프 데이터를 생성하기.
set.seed(1234)
pair_bigram <- bigram_seperated %>%
  count(word1, word2, sort = T) %>%
  na.omit() %>% print()

graph_bigram <- pair_bigram %>%
  filter(n >= 3) %>% # 적어도 빈도가 3이상만 선택
  as_tbl_graph(direxted = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap())) %>% print()


#Q6. 바이그램을 이용하여 네트워크 그래프를 만들기.
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
set.seed(1234)
graph_bigram %>% ggraph(layout = "fr") + # 레이아웃
  geom_dege_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, #노드 크기
                      color = group),# 노드 색깔
                  show.legend = F) + # 범례 삭제
  sclae_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph()

