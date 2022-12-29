setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
raw_news_comment <- read_csv("news_comment_BTS.csv")

senti_dic <- read_delim("C:/Users/jspar/OneDrive/Documents/학교/전공/텍마/KnuSentiLex-master/KnuSentiLex-master/SentiWord_Dict.txt", delim='\t', col_names=c("word", "polarity"))

# Q1."news_comment_BTS.csv"를 불러온 다음 행 번호를 나타낸 변수를 추가하고 분석에 적합하게 전처리.
library(textclean)
news_comment <- raw_news_comment %>%
  mutate(id = row_number(), # 댓글자를 구분하기 위해 id 변수 생성
         reply = str_squish(replace_html(reply)))
news_comment


# Q2. 댓글을 띄어쓰기 기준으로 토큰화하고 감성 사전을 이용해 댓글의 감성 점수 구하기.
#토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply, output = word,
                token = "words", drop = F) %>%
  filter(str_length(word) > 1)
word_comment

# 감성점수 부여
word_comment <- word_comment %>%
  left_join(senti_dic, by = "word") %>% # 감성 사전 결합
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) # 감성 사전에 없으면 중립
word_comment %>% select(word, polarity)

# 감정 분류
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity == 2, "positive",
                            ifelse(polarity == -2, "negative",
                                   "neutural"))) %>% print()

# 댓글별 감정점수 생성                    
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()


# Q3. 감정 범주 별 댓글 빈도를 나타낸 막대 그래프 만들기.
# 댓글의 감정분류
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "positive",
                            ifelse(score <= -1, "negative", "neutural")))

# 댓글의 감정 빈도와 비율 생성
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100) %>% print()

# 댓글의 감정 분류 막대 그래프 생성
frequency_score %>% ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  ylab("댓글 수") +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("positive", "neutural", "negative"))


#Q4. 댓글을 띄어쓰기 기준으로 토큰화한 다음 감정 범주 별 단어 빈도 구하기.
# 한글 단어 토큰화
comment <- score_comment %>%
  unnest_tokens(input = reply, output = word,
                token = "words", # 단어 기준 토큰화
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & # 한글만 추출
           str_length(word) >= 2) %>% print() 

# 감정 및 단어별 빈도 생성
frequency_word <- comment %>%
  count(sentiment, word, sort = T) %>% print()

# 긍정 댓글 고빈도 단어
frequency_word %>% filter(sentiment == "positive")

# 부정 댓글 고빈도 단어
frequency_word %>% filter(sentiment == "negative")


# Q5. 로그 RR을 이용해 긍정 댓글과 부정 댓글에 상대적으로 자주 사용된 단어 10개씩 추출하기.
comment_wide <- frequency_word %>%
  filter(sentiment != " neutural") %>%
  pivot_wider(names_from = sentiment, # sentiment의 범주를 변수로 사용
              values_from = n, # 해당되는 값은 n으로 함
              values_fill = list(n = 0)) %>% print()

# 로그상대위험 logRR
comment_wide <- comment_wide %>%
  mutate(log_RR = log(((positive + 1) / (sum(positive + 1))) /
                        ((negative + 1) / (sum(negative + 1))))) %>% print() 

# 로그 상대위험이 가장 큰 단어 10개씩 추출
top10 <- comment_wide %>%
  mutate(sentiment = ifelse(log_RR > 0, "positive", "negative")) %>%
  group_by(sentiment) %>%
  slice_max(abs(log_RR), n = 10, with_ties = F) %>% print()


# Q6. 긍정 댓글과 부정 댓글에 상대적으로 자주 사용된 단어 각각 10개씩을 선택하여 긍정과 부정이 대비되도록 막대 그래프 만들기.
# 막대그래프 생성
top10 %>% ggplot(aes(x = reorder(word, log_RR), y = log_RR, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# Q7. 'Q3'에서 만든 데이터를 이용해 '긍정 댓글에 가장 자주 사용된 단어'를 언급한 댓글을 감정 점수가 높은 순으로 10개 출력하기.
A <- score_comment %>% select(score, reply) %>%
  arrange(-score) %>%
  head(n=10) %>% print()
