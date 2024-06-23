data = read.csv("C:\\Users\\zxy\\Desktop\\5205project\\cleandata.csv")

#Recommender
#library(recommenderlab)
#data_matrix = as(data,Class = 'realRatingMatrix')

#Has character variable so not working

#Association
#library(arules); library(arulesViz)
#items = read.transactions(data, format = 'basket', sep=',',skip=1)

#Not suitable for reading transactions

#Text mining

#1.Ratings of Reviews

#Mean rating review:HongKong,California and Paris
library(dplyr)
data %>%
  group_by(City) %>%
  summarise(mean_rating = mean(Rating, na.rm = TRUE))%>%
  as.data.frame()
#Paris is lowest while California is highest

#Median rating review:HongKong,California, and Paris
library(dplyr)
data %>%
  group_by(City) %>%
  summarise(median_rating = median(Rating, na.rm = TRUE))%>%
  as.data.frame()
#California is highest among three while same for HK and paris.

#2.Distribution of Rating for each city
library(ggplot2)
ggplot(data, aes(x = Rating, fill = City)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "dodge") +
  scale_fill_manual(values = c("HongKong" = "blue", "California" = "orange", "Paris" = "red")) +
  labs(title = "Distribution of Review Ratings by City",
       x = "Review Rating", y = "Frequency")
#California has majority of highest rating:5 and Paris has majority of least rating:1


#3.Summary of characters review, words review and sentence review
library(dplyr);library(stringr)
data %>%
  select(Review_Text)%>%
  mutate(characters = nchar(Review_Text),
         words = str_count(Review_Text,pattern='\\S+'),
         sentences = str_count(Review_Text,pattern="[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))%>%
  summarize_at(c('characters','words','sentences'),.funs = mean,na.rm=T)
#MEAN FOR characters:696.8175,words:129.7938,sentences:8.251219

#4.Length
# Testing the correlation between number of characters and rating
r_characters = cor.test(nchar(data$Review_Text),data$Rating)

# Testing the correlation between number of words and rating
r_words = cor.test(str_count(string = data$Review_Text,pattern = '\\S+'),data$Rating)

# Testing the correlation between number of sentences and rating
#r_sentences = cor.test(str_count(string = data$Review_text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$Rating)
#run error for r_sentences
correlations = data.frame(r = c(r_characters$estimate, r_words$estimate),p_value=c(r_characters$p.value, r_words$p.value))
rownames(correlations) = c('Characters','Words')
correlations
#The p-values indicate there is significant evidence that the correlations are non-zero (ie there some relationship). However the r values are quite small in magnitude. There is only a weak negative relationship in both cases.


#5.Grammar
#Screaming Reviews
percentUpper = 100*str_count(data$Review_Text,pattern='[A-Z]')/nchar(data$Review_Text)
summary(percentUpper)

#Exclamation Marks
percentExclamation = 100*str_count(data$Review_Text,pattern='!')/nchar(data$Review_Text)
summary(percentExclamation)

#impact of Upper Case and Exclamation marks on rating
r_upper = cor.test(percentUpper,data$Rating)
r_exclamation = cor.test(percentExclamation,data$Rating)
correlations2 = data.frame(r = c(r_upper$estimate, r_exclamation$estimate),p_value=c(r_upper$p.value, r_exclamation$p.value))
rownames(correlations2) = c('Upper Case','Exclamation Marks')
correlations2
#P value indicate significance however with a small r, the relationship is weak.

#6.Keywords 'Disneyland"
library(stringr)
mean(str_detect(string = tolower(data$Review_Text),pattern = 'Disneyland|Disney land'))*100
#Maybe should change to another popular word exist in the review


#7.Common Words
#Top 25 words
library(dplyr); library(tidytext); library(magrittr)
data%>%
  unnest_tokens(input = Review_Text, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)
#Visualize it
data%>%
  unnest_tokens(input = Review_Text, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

#Look at the top 25 word list without the above stop words
data%>%
  unnest_tokens(input = Review_Text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)
#Visualize
data%>%
  unnest_tokens(input = Review_Text, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()
#could see that vistors are caring about:park,rides,time,day,food,kids,people,fast,pass,parade,staff and characters(maybe the cartoon one?)

#qdap method
#set of top 25 words can also be generated using freq_terms from library(qdap)
#Failed for qdap library always show error for not connect with Java while Java updated
#library(qdap)
#freq_terms(text.var = data$Review_Text, top = 25)
#plot(freq_terms(text.var = data$Review_Text, top = 25))

#Without stopwords top 25
#freq_terms(text.var=data$Review_Text,top=25,stopwords = Top200Words) 
#plot(freq_terms(text.var=data$Review_Text,top=25,stopwords = Top200Words))

#8.Tokenize

#Words in each review
library(dplyr); library(tidytext)
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word,input=Review_Text) %>% 
  ungroup()%>%
  group_by(Review_ID)%>%
  summarize(count = n()) %>%
  arrange(desc(count))
#Review_ID:110066682 most words 3951,second most 185800062 has 3550 words

library(dplyr); library(tidytext)
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word,input=Review_Text) %>% 
  ungroup()%>%
  group_by(Review_ID)%>%
  summarize(count = n()) %>%
  arrange(count)
#Review_ID:457253942 and 606997669 least words:3;third and fourth least 124459435 and 130999201 has 6 words.

#Plot the data each id review's contain how many words
library(dplyr); library(tidytext);library(ggplot2)
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word,input=Review_Text)%>%
  ungroup()%>%
  group_by(Review_ID)%>%
  summarize(count = n())%>%
  ggplot(aes(x=count))+geom_histogram(bins = 40)+xlab('Number of Words')

#9.Categorize
#9.1Using Bing sentiment
library(tidytext)
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  as.data.frame()
#Visualization based on the count of positive and negative words in reviews
library(ggthemes)
data%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+
  geom_col()+
  theme_economist()+
  guides(fill=F)+
  coord_flip()

#Whether the proportion of positive words has any impact on helpfulness
#Look at the proportion of positive and negative words for each rating
data %>%
  select(Review_ID,Review_Text,Rating)%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  as.data.frame()

#Picture
library(ggthemes)
data %>%
  select(Review_ID,Review_Text,Rating)%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()+
  coord_flip()

#Compute the proportion of positive words for each review
data%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Review_ID,Rating)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()

#See if reviews with a lot of positive words rated favorably
data%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(Review_ID,Rating)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>%
  ungroup()%>%
  summarize(correlation = cor(proportion_positive,Rating))
#The correlation is 0.464 not very high though

#9.2Using NRC sentiment
library(lexicon)
data %>%
  select(Review_ID, Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>%
  ungroup()%>%
  group_by(y)%>%
  summarize(count = n())%>%
  ungroup()
#Visualize
library(ggthemes)
data %>%
  select(Review_ID,Review_Text,Rating)%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  ungroup()%>%
  inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>%
  mutate(sentiment = ifelse(y==1,'positive','negative')) %>% 
  group_by(Rating,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=Rating,y=proportion,fill=sentiment))+
  geom_col()+
  theme_economist()+
  coord_flip()

#9.3nrc emotion
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt',
header = F,
col.names = c('word','sentiment','num'),
sep = '\t',
stringsAsFactors = F)
nrc = nrc[nrc$num!=0,]
nrc$num = NULL

#exmaine emotion expressed in the reviews
library(dplyr);library(tidytext)
data%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  arrange(desc(n))

#BY Plot
data%>%
  group_by(Review_ID)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n), y=n, fill=sentiment))+
  geom_col()+
  guides(fill=F)+
  coord_flip()+
  theme_wsj()

#Emotions in each review
library(tidyr)
data%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(nrc)%>%
  group_by(Review_ID,Rating, sentiment)%>%
  count()%>%
  pivot_wider(names_from = sentiment,values_from=n)%>%
  select(Review_ID, Rating, positive, negative, trust, anticipation, joy, fear, anger, sadness, surprise, disgust)%>%
  mutate_at(.vars = 3:12, .funs = function(x) replace_na(x,0))%>%
  ungroup()

#did review_rating tied with emotion expressed?
library(tidyr)
data%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output = word, input =Review_Text)%>%
  inner_join(nrc)%>%
  group_by(Review_ID,sentiment,Rating)%>%
  count()%>%
  group_by(Review_ID,sentiment, Rating)%>%
  pivot_wider(names_from = sentiment,values_from = n)%>%
  mutate_at(.vars = 3:12, .funs = function(x) replace_na(x,0))%>%
  ungroup()%>%
  pivot_longer(cols = anticipation: sadness, names_to = 'sentiment',values_to = 'n')%>%
  group_by(sentiment, Rating)%>%
  summarize(n = mean(n))%>%
  ggplot(aes(x=Rating,y=n,fill=Rating))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+
  coord_flip()+
  theme_bw()

#Quantify relationship by examining relatinoship between frequency of emotions expressed and rating
library(tidyr)
data%>%
  group_by(Review_ID, Rating)%>%
  unnest_tokens(output = word, input = Review_Text)%>%
  inner_join(nrc)%>%
  group_by(Review_ID,Rating, sentiment)%>%
  count()%>%
  pivot_wider(names_from = sentiment,values_from=n)%>%
  select(Review_ID, Rating, positive, negative, trust, anticipation, joy, fear, anger, sadness, surprise, disgust)%>%
  mutate_at(.vars = 3:12, .funs = function(x) replace_na(x,0))%>%
  ungroup()%>%
  pivot_longer(cols = 3:12, names_to = 'sentiment',values_to = 'n')%>%
  group_by(sentiment)%>%
  summarize('Correlation with rating' = round(cor(n,Rating),2),
            p = ifelse(cor.test(n,Rating)$p.value<0.05,'p < 0.05','not significant')) 

#9.4 afinn sentiment
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)

#All review
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))
#Distribution of Afinn sentiment
library(ggplot2);library(ggthemes)
data %>%
  select(Review_ID,Review_Text)%>%
  group_by(Review_ID)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

#10 visualizing Text
library(wordcloud)
wordcloudData = 
  data%>%
  group_by(Review_ID)%>%
  unnest_tokens(output=word,input=Review_Text)%>%
  ungroup()%>%
  select(Review_ID,word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()

library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

