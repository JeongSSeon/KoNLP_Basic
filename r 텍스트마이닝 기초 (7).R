setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
raw_speeches <- read_csv("inaugural_address.csv")


# 1. inaugural_address.csv를 불러와 분석에 적합하게 전처리한 다음 연설문에서 명사 추출.
speeches <- raw_speeches %>%
  mutate(value=str_replace_all(value, pattern="[^가-힣]", replacement=" "),
         value=str_squish(value)) 

speeches_noun <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

frequency <- speeches_noun %>%
  count(president, word) %>%
  filter(str_length(word) > 1) %>%
  print()


# 2. TF-IDF를 이용해 각 연설문에서 상대적으로 중요한 단어 10개씩 추출.
frequecy <- frequecy %>%
  bind_tf_idf(term = word,
              document = president,
              n = n) %>%
  arrange(-tf_idf) %>%
  print()

frequecy %>% filter(president == "문재인")
frequecy %>% filter(president == "박근혜")
frequecy %>% filter(president == "이명박")
frequecy %>% filter(president == "노무현")

# 주요 단어 추출
top10_four <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)


# 3. 각 연설문에서 상대적으로 중요한 단어를 나타낸 막대 그래프 만들기.
# 그래프 순서 정하기
top10_four$president <- factor(top10_four$president,
                               levels = c("문재인", "박근혜", "이명박", "노무현"))

# '나눔고딕' 폰트 적용
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

# 막대 그래프 만들기
top10_four %>% ggplot(aes(x = reorder_within(x=word, by=tf_idf, 
                                             within=president),
                          y = tf_idf, fill = president)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
