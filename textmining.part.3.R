data = read.csv("C:\\Users\\zxy\\Desktop\\5205project\\cleandata.csv")

#Pairwise analysis of Text

#1.0 Bigrams and N-grams
# Now we’d like to extend our analysis to consider pairs of words (bigrams) and more generally N-grams. 
#Moreover, if we can determine how often word X is followed by word Y, we can build a model by exploiting the relationships between them. 

#1.1Bigrams
library(dplyr)
library(tidyr)
library(tidytext)
data_bigrams <- data %>% 
  unnest_tokens(bigram, Review_Text, token = "ngrams", n=2)

head(data_bigrams,15)

#A look at count, Only first 25 lines of output is displayed
data_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))

#Remove stop words ,separate the words into columns and remove from each column
bigrams_separated <- data_bigrams %>% 
  separate(bigram, c('word1', 'word2'), sep=' ')

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) 

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE) %>% 
  arrange(desc(n))

head(bigram_counts,15)

#Analyzing Bigrams
disney_by_tf_idf <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = ' ') %>% 
  count(Review_ID, bigram) %>% 
  bind_tf_idf(bigram, Review_ID, n) %>% 
  arrange(desc(tf_idf))

head(disney_by_tf_idf,15)

docs_after_stop_words <- bigrams_filtered %>% 
  group_by(Review_ID) %>% 
  count(Review_ID) %>% 
  NROW()
log(docs_after_stop_words)#10.63578

#Revisiting sentimental analysis by bigrams 
library(ggthemes)
library(ggplot2)
library(lexicon)

bigrams_filtered %>%
  inner_join(hash_sentiment_nrc,by = c('word1'='x')) %>% 
  rename(y1=y) %>% 
  inner_join(hash_sentiment_nrc,by = c('word2'='x')) %>% 
  rename(y2=y) %>% 
  mutate(sentiment_score = ifelse((y1 == -1) & (y2 == 1), -1, y1+y2)) %>% 
  mutate(sentiment = ifelse(sentiment_score == 0, 'neutral', ifelse(sentiment_score > 0, 'positive', 'negative'))) %>% 
  group_by(Rating, sentiment) %>% 
  summarize(n = n()) %>% 
  mutate(proportion = n/sum(n)) %>% 
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()+
  coord_flip()

#2.0 Visualizing Word Relationships with Graphs
library(igraph)
bigram_graph <- bigram_counts %>% 
  # We limit to just pairs with alteast 100+ observations since the graph can become very clusters. Feel free to experiment.
  filter(n > 100) %>% 
  graph_from_data_frame()

#convert igraph to ggraph for plotting
library(ggraph)
set.seed(617)

ggraph(bigram_graph, layout = 'fr')+
  geom_edge_link()+
  geom_node_point() +
  geom_node_text(aes(label = name), vjust =1, hjust =1)

#3.0 Word embeddings
library(slider)
slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, ~.x, .after = window_size - 1, .step = 1, .complete = TRUE
  )
  safe_mutate <- safely(mutate)
  out <- map2(skipgrams, 1:length(skipgrams), ~ safe_mutate(.x, window_id = .y))
  out %>%
    transpose() %>% 
    pluck("result") %>% 
    compact() %>%
    bind_rows()
}
#start from the top again with our video game reviews tokenized into single words, with stops removed.
tidyreviews <- data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  anti_join(stop_words)%>%
  ungroup()

#Now that we have “cleaned” our dataset, we need to go backwards and reconstruct the reviews (minus the stop words).
# This is necessary so we can see the surrounding words to our targets in the window.
nested_words <- tidyreviews %>% 
  nest(words = c(word))

head(nested_words)

#4.0 PMI-How often do words occur together with other words?

#Construct word embeddings based on PMI
library(furrr)
library(tictoc)
library(irlba)
library(widyr)
tic()
plan(multisession)  # invoke parallel processing

tidy_pmi <- nested_words %>%  
  mutate(words = future_map(words, slide_windows, 4)) %>%
  unnest(words) %>%
  unite(window_id, Review_ID, window_id) %>%
  pairwise_pmi(word, window_id) 
toc(log=TRUE) #1713.54 sec(may differ)

plan(sequential) #turn off the parallel processing (free up computer resources)

#Find relationship between words 
#The code above produces PMIs for several combinations of word pairs across all reviews. 
#To make this computation useful for analysis we need to transform the PMI to vectors.

library(Matrix)
library(irlba)
plan(multisession)

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi, 
    nv = 100, maxit = 1000
  )
plan(sequential)

#Results :Nearest words
nearest_neighbors <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), 
           sort = TRUE, 
           maximum_size = NULL)(item1, dimension, value) %>%
    select(-item2)
}

#Which word are closest to "park"
tidy_word_vectors %>%
  nearest_neighbors("park")

#'queue'
tidy_word_vectors %>%
  nearest_neighbors("queue")

#"diappointed" reviews
tidy_word_vectors %>%
  nearest_neighbors("disappointed")

#'amazing' reviews
tidy_word_vectors %>%
  nearest_neighbors("amazing")

#'fee'
tidy_word_vectors %>%
  nearest_neighbors("fee")

#Principal components
library(ggplot2)
tidy_word_vectors %>%
  filter(dimension <= 8) %>%
  group_by(dimension) %>%
  top_n(10, abs(value)) %>%
  ungroup %>%
  ggplot(aes(value, item1, fill = as.factor(dimension))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4)

