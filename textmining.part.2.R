data = read.csv("C:\\Users\\zxy\\Desktop\\5205project\\cleandata.csv")

#Text mining part 2:predictive analysis

#Inspect the text of shortest review
library(stringr)
shortest_review_index = which.min(str_count(string = data$Review_Text,pattern = '\\S+'))
data$Review_Text[shortest_review_index]

#Inspect the text of longest review
longest_review_index = which.max(str_count(string = data$Review_Text,pattern = '\\S+'))
data$Review_Text[longest_review_index]

#Prepare and tokenize
#Create a corpus
library(tm)
corpus = Corpus(VectorSource(data$Review_Text))
corpus[[longest_review_index]]

#21-41行code非必须
##############################################################
#clean text
corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus[[longest_review_index]][1]

#Match pattern and replace url with a blank spcae
corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))
corpus[[longest_review_index]][1]

#Remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
corpus[[longest_review_index]][1]

#Remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus[[longest_review_index]][1]

#Strip whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
corpus[[longest_review_index]][1]
#########################################

#Create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$Review_Text))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

#Stem document
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[longest_review_index]][1]

#1.0 Create a document term matrix(tokenize)
dtm = DocumentTermMatrix(corpus)
dtm

#see how many times words appear in the longest review
inspect(dtm[longest_review_index,])

#The document term matrix contains a column for each term generated from tokenizing the corpus.
dim(dtm)

#Remove sparse terms
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm

#Complete stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

#Browse tokens
sort(colSums(xdtm),decreasing = T)

#2.0 Document Term Matrix-tfidf:term frequency inverse document frequency weighting
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

#Document term matrix : Term frequency vs Term frequency Inverse Document Frequency
xdtm[611:620,41:50]
xdtm_tfidf[611:620,41:50]

#Bar chart for Top 20 terms contrasts between
#weights of term frequency and term frequency inverse document frequency weighting
library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()

#Add review rating back to dataframe of features
Disney_data = cbind(review_rating = data$Rating,xdtm)
Disney_data_tfidf = cbind(review_rating = data$Rating,xdtm_tfidf)

#3.0 Predictive models (using TF features)
set.seed(617)
split = sample(1:nrow(Disney_data),size = 0.7*nrow(Disney_data))
train = Disney_data[split,]
test = Disney_data[-split,]

#3.1 build a basic decision tree
library(rpart); library(rpart.plot)
tree = rpart(review_rating~.,train)
rpart.plot(tree)

#tree model predictions
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree

#3.2 linear regression model
reg = lm(review_rating~.,train)
summary(reg)

#linear model predictions
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg

#4.0 predictive model (using TF-IDF features)
#split data
set.seed(617)
split = sample(1:nrow(Disney_data_tfidf),size = 0.7*nrow(Disney_data_tfidf))
train = Disney_data_tfidf[split,]
test = Disney_data_tfidf[-split,]

#4.1 decision tree model
library(rpart); library(rpart.plot)
tree = rpart(review_rating~.,train)
rpart.plot(tree)

#tree model predictions
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree

#4.3Linear regression model
# Only first 25 lines of output is displayed
reg = lm(review_rating~.,train)
summary(reg)

#linear regression model predictions
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg
