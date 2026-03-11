####Reddit mining (webscrapping)

install.packages("httr")
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("ggraph")
install.packages("igraph")

library(httr)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)

url <- "https://www.reddit.com/r/femalefashionadvice/new.json?limit=100"

res <- GET(url, user_agent("trend-research-bot"))

json_data <- fromJSON(content(res, "text"), flatten = TRUE)


res$status_code

posts <- json_data$data$children

reddit_df <- tibble(
  title = posts$data.title,
  text  = posts$data.selftext
)

names(reddit_df)


reddit_words <- reddit_df %>%
  unite(content, title, text, sep = " ") %>%
  unnest_tokens(word, content)

data("stop_words")

reddit_words <- reddit_words %>%
  anti_join(stop_words, by = "word")

aesthetic_words <- reddit_words %>%
  filter(str_detect(word, "core|aesthetic|style|girl|vibe"))

trend_candidates <- aesthetic_words %>%
  count(word, sort = TRUE)

trend_candidates

head(trend_candidates, 20)

reddit_phrases <- reddit_df %>%
  unite(content, title, text, sep = " ") %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2)

phrase_counts <- reddit_phrases %>%
  count(bigram, sort = TRUE)

head(phrase_counts, 20)


aesthetic_trends <- phrase_counts %>%
  filter(str_detect(bigram, "core|girl|aesthetic|style|vibe"))

aesthetic_trends



reddit_df <- tibble(
  title = posts$data.title,
  text  = posts$data.selftext
)

head(reddit_df)
names(reddit_df)

head(reddit_words)


##Detect New Aesthetic Phrases
aesthetic_patterns <- c(
  "core",
  "girl",
  "aesthetic",
  "style",
  "vibe",
  "look",
  "energy"
)

aesthetic_trends <- phrase_counts %>%
  filter(n >= 2) %>%   # avoid noise
  filter(str_detect(bigram, str_c(aesthetic_patterns, collapse = "|"))) %>%
  arrange(desc(n))

aesthetic_trends

candidate_trends <- phrase_counts %>%
  filter(n >= 2) %>%
  filter(str_detect(bigram, "core|aesthetic|girl|style|vibe")) %>%
  arrange(desc(n))

head(candidate_trends, 20)

core_trends <- phrase_counts %>%
  filter(str_detect(bigram, "core")) %>%
  arrange(desc(n))

core_trends

##Analytics

reddit_df <- reddit_df %>%
  mutate(doc_id = row_number())

reddit_words <- reddit_df %>%
  unite(content, title, text, sep = " ") %>%
  unnest_tokens(word, content)

reddit_words <- reddit_words %>%
  anti_join(stop_words, by = "word")

tfidf_words <- reddit_words %>%
  count(doc_id, word, sort = TRUE) %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf))

head(tfidf_words, 20)

trend_terms <- tfidf_words %>%
  filter(str_detect(word, "core|girl|style|aesthetic|vibe")) %>%
  arrange(desc(tf_idf))

trend_terms

reddit_bigrams <- reddit_df %>%
  unite(content, title, text, sep = " ") %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2)

reddit_bigrams <- reddit_bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

reddit_bigrams <- reddit_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


reddit_bigrams <- reddit_bigrams %>%
  unite(bigram, word1, word2, sep = " ")

tfidf_phrases <- reddit_bigrams %>%
  count(doc_id, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))

head(tfidf_phrases, 20)

trend_phrases <- tfidf_phrases %>%
  filter(str_detect(bigram, "core|girl|style|aesthetic|vibe|luxury|look")) %>%
  arrange(desc(tf_idf))

trend_phrases


reddit_df <- tibble(
  title = posts$data.title,
  text  = posts$data.selftext,
  created = as.POSIXct(posts$data.created_utc, origin="1970-01-01", tz="UTC")
)

reddit_df <- reddit_df %>%
  mutate(day = as.Date(created))

reddit_bigrams <- reddit_df %>%
  unite(content, title, text, sep=" ") %>%
  unnest_tokens(bigram, content, token="ngrams", n=2)

reddit_bigrams <- reddit_bigrams %>%
  separate(bigram, into=c("word1","word2"), sep=" ")

reddit_bigrams <- reddit_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

reddit_bigrams <- reddit_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

trend_time <- reddit_bigrams %>%
  count(day, bigram, sort=TRUE)

trend_growth <- trend_time %>%
  group_by(bigram) %>%
  arrange(day) %>%
  mutate(
    prev = lag(n),
    growth = n - prev
  )

emerging_trends <- trend_growth %>%
  filter(!is.na(growth)) %>%
  group_by(bigram) %>%
  summarise(score = sum(growth, na.rm=TRUE)) %>%
  arrange(desc(score))

trend_radar <- trend_time %>%
  group_by(bigram) %>%
  summarise(
    total_mentions = sum(n),
    active_days = n_distinct(day)
  ) %>%
  mutate(
    score = total_mentions * log(active_days + 1)
  ) %>%
  arrange(desc(score))


##analytics
top_trends <- trend_radar %>%
  slice_max(score, n = 15)

ggplot(top_trends, aes(x = reorder(bigram, score), y = score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Emerging Fashion Aesthetics on Reddit",
    x = "Aesthetic Phrase",
    y = "Trend Score"
  )
trend_radar <- trend_radar %>%
  filter(!str_detect(bigram, "question|help|advice|discussion"))

trend_radar <- trend_radar %>%
  filter(total_mentions >= 2)

top_trends <- trend_radar %>%
  slice_max(score, n = 15)

top_trends <- trend_radar %>%
  slice_max(score, n = 10)

ggplot(top_trends, aes(x = reorder(bigram, score), y = score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Emerging Fashion Phrases on Reddit",
    x = "Phrase",
    y = "Trend Score"
  ) +
  theme_minimal()

top_phrase <- top_trends$bigram[1]

trend_time %>%
  filter(bigram == top_phrase) %>%
  ggplot(aes(day, n)) +
  geom_line() +
  geom_point() +
  labs(
    title = paste("Trend Evolution:", top_phrase),
    x = "Date",
    y = "Mentions"
  ) +
  theme_minimal()

write.csv(trend_radar, "trend_results.csv", row.names = FALSE)

ggsave("trend_leaderboard.png")

bigram_graph <- reddit_bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigram_graph <- bigram_graph %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_graph %>%
  filter(n >= 2)

graph <- graph_from_data_frame(bigram_graph)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n)) +
  geom_node_point(color = "steelblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Fashion Phrase Network from Reddit")
