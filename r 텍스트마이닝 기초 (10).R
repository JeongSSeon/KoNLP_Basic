library(tidyverse)
setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
raw_speech <- read_csv("speeches_roh.csv")


#Q1. speeches_roh.csv를 불러온 다음 연설문이 들어있는 content를 문장 기준으로 토큰화.
# 문장 토큰화
library(tidytext)
library(KoNLP)
sentences_speech <- raw_speech %>%
  unnest_tokens(input = content,
                output = word,
                token = 'sentences',
                drop = F) %>%
  filter(str_length(word) > 1) %>%
  rename(sentences = word)



#Q2. 문장을 분석에 적합하게 전처리한 다음 명사 추출.
# 전처리
sentences_speech <- sentences_speech %>%
  mutate(sentences = str_replace_all(sentences, "[^가-힣]", " "),
         sentences = str_squish(sentences))
# 명사추출
noun_speech <- sentences_speech %>%
  unnest_tokens(input = sentences,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_length(word) > 1)


#Q3. 연설문 내 중복 단어를 제거하고 빈도가 100회 이하인 단어 추출.
# 중복단어 제거
count_word <- noun_speech %>%
  add_count(word, sort=TRUE) %>%
  filter(!word %in% c("우리", "대통령", "있습니다", "있습니", "여러분", "국민"))
# 단어 빈도 100회 이하 추출
word_count <- count_word %>%
  filter(n<=100)




#Q4. 추출한 단어에서 다음의 불용어 제거하기.
# 불용어 리스트
stopword <- c( "들이", "하다", "하게", "하면", "해서", "이번", "하네",
               "해요", "이것", "니들", "하기", "하지", "한거", "해주",
               "그것", "어디", "여기", "까지", "이거", "하신", "만큼")
# 불용어 제거
word_count <- word_count %>%
  filter(!word %in% stopword)



#Q5. 연설문 별 단어 빈도를 구한 다음 DTM 만들기.
# 연설문 별 단어 빈도
count_word_speech <- word_count %>%
  count(id, word, sort = T)
# DTM 생성
dtm_comment <- count_word_speech %>%
  cast_dtm(document = id, term = word, value = n) %>% print()
# DTM의 내용 확인하기
as.matrix(dtm_comment[1:15, 1:15])



#Q6. 토픽 수를 2~20개로 바꿔가며 LDA 모델을 만든 다음 최적 토픽 수 구하기.
library(ldatuning)
models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:21, # 22개의 LDA 모델을 만듬
                           return_models = T,
                           control = list(seed = 1234))
models %>% select(topics, Griffiths2004) 
FindTopicsNumber_plot(models)



#Q7. 토픽 수 9개인 LDA 모델 생성하기.
library(topicmodels)
lda_model <- LDA(dtm_comment,
                 k = 9,
                 method = "Gibbs",
                 control = list(seed = 1234))
# 모델 내용 확인
glimpse(lda_model)



#Q8. LDA 모델의 beta를 이용해 각 토픽에 등장할 확률이 높은 상위 10개 단어를 추출한 다음 토픽 별 주요 단어를 나타낸 막대 그래프 만들기.
# 단어들이 토픽 별로 들어갈 확률 베타 포함한 df
library(reshape2)
term_topic <- tidy(lda_model, matrix = "beta") %>%
  mutate(topic_name = paste("Topic", topic)) %>% # 토픽 이름 변수 설정
  print()
# beta 내용 확인
# 토픽 별 단어 수
term_topic %>% count(topic)

# 토픽별 beta 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties =F) %>% print() # 동점 제외
# 막대그래프 작성
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto() #한글폰트설정

library(scales)
top_term_topic %>% ggplot(aes(x = reorder_within(x=term, by=beta, within=topic_name), 
                              y = beta,
                              fill = factor(topic_name))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic_name, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4, # 축 눈금을 4개 내외로 정하기
                     labels = number_format(accuracy = .01)) + # 눈금 소수점 첫째 자리에서 반올림
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



#Q9. LDA 모델의 gamma 를 이용해 연설문 원문을 확률이 가장 높은 토픽으로 분류하기.
# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma") %>% # 문서(id)가 토픽에 들어갈 확률을 포함한 df
  mutate(topic_name = paste("Topic", topic)) %>% print() # 토픽 이름 변수 설정
# gamma 확인
doc_topic %>% count(topic)

# 문서 별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>% print() # top_n(n=1, wt=gamma)과 동일함

# gamma가 동점이 발생한 경우 확인
doc_topic %>% group_by(document) %>%
  top_n(n=1, wt=gamma) %>% 
  count(document) %>% 
  filter(n > 1) %>% print()

# 데이터셋을 결합하기 위해 기준 변수 타입을 integer로 통일
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
speech_content_topic <- raw_speech %>%
  left_join(doc_class, by = c("id" = "document")) %>% print()
# 결합 확인
speech_content_topic %>% select(id, topic)



#Q10. 토픽 별 문서 수 출력하기.
# 토픽 별 문서 수 확인
speech_content_topic <- news_comment_topic %>%
  na.omit()
speech_content_topic1 <- speech_content_topic %>% count(topic) %>% print()



#Q11. 문서가 가장 많은 토픽의 연설문을 gamma가 높은 순으로 출력하고 내용이 비슷한지 살펴보기.
# gamma가 높은 주요 문서가 먼저 출력되도록 정렬
content_topic <- speech_content_topic %>%
  arrange(-gamma)
content_topic %>% select(topic, gamma, content)

# 토픽별 주요 단어 목록
top_terms <- term_topic %>%
  group_by(topic_name) %>%
  slice_max(order_by=beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", ")) %>% print()

# 내용 확인
# 토픽 1 내용 확인
content_topic %>% filter(topic == 1 & str_detect(content, "회담")) %>%
  head(20) %>% 
  pull(content) # pull{dplyr}: 단일 컬럼만 출력함

