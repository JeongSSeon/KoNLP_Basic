setwd('C:/Users/jspar/OneDrive/Documents/학교/전공/텍마')
raw_speeches <- read_csv("speeches_presidents.csv")


# 1. 4명의 대통령의 대선 출마 선언문의 명사를 추출하여 로그오즈비를 구하고 중요한 단어 10개씩을 뽑아서 막대그래프 그리기.
#명사 토큰화 및 추출
speeches <- raw_speeches %>% 
  mutate(value=str_replace_all(value, pattern="[^가-힣]", replacement=" "),
         value=str_squish(value)) 

speeches_noun <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

freq_noun <- speeches_noun %>%
  filter(str_length(word) > 1) %>% # 두 글자 이상 추출
  count(president, word) %>% # 연설문 및 단어별 빈도
  print()

library(tidylo)
freq_noun_lo = freq_noun %>%
  filter(word != "국민") %>%
  bind_log_odds(set = president, feature = word, n = n) %>%
  arrange(log_odds_weighted) %>% # moon에서 비중이 큰 단어
  print() 

top10_lo2 = freq_noun_lo %>%
  group_by(president) %>%
  slice_max(abs(log_odds_weighted), n=10, with_ties=FALSE) %>%
  arrange(-log_odds_weighted) %>% print(n=Inf)

top10_lo2 %>% ggplot(aes(x = reorder_within(x=word, by=log_odds_weighted, within=president),
                         y = log_odds_weighted, fill = president)) +
  geom_col() +
  coord_flip() +
  ylab("ln(OR)") +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


# 2. Speeches_presidents.csv를 불러와 이명박 전 대통령과 노무현 전 대통령의 연설문만을 추출하여 다음에 답하시오.
# 2-(1). 연설문에서 명사를 추출한 다음 연설문 별 단어 빈도를 구하기.
speeches <- raw_speeches %>%
  mutate(value=str_replace_all(value, pattern="[^가-힣]", replacement=" "),
         value=str_squish(value)) 

speeches_noun <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

freq_noun <- speeches_noun %>%
  count(president, word) %>%
  filter(str_length(word) > 1) %>%
  print()

freq_noun %>% filter(president == "이명박")
freq_noun %>% filter(president == "노무현")


# 2-(2). 로그RR을 이용해 두 연설문에서 상대적으로 중요한 단어를 10개씩 추출하기.
library(tidylo)
freq_noun_lo = freq_noun %>%
  bind_log_odds(set = president, feature = word, n = 10) %>%
  arrange(log_odds_weighted)

freq_noun_wide <- freq_noun %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = 0) %>%
  mutate(p_lee = ((이명박+1)/(sum(이명박+1))), 
         p_no = ((노무현+1)/(sum(노무현+1))),
         RR = p_lee/p_no,  # 상대위험 (RR) 변수 추가
         log_RR = log(RR)) %>%
  print()

top10_logRR = freq_noun_wide %>%
  group_by(president = ifelse(log_RR > 0, "이명박", "노무현")) %>%
  slice_max(abs(log_RR), n=10, with_ties=FALSE) %>%
  arrange(-log_RR) %>%
  select(word, 이명박, 노무현, log_RR, president) %>%
  print(n=Inf)


# 2-(3). 두 연설문에서 상대적으로 중요한 단어를 나타낸 막대그래프 그리기.
top10_logRR %>% ggplot(aes(x = reorder(word, log_RR),
                           y = log_RR, fill = president)) +
  geom_col() +
  coord_flip() +
  ylab("ln(RR)") +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
