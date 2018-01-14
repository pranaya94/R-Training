
setwd("~/Dropbox/Trainings/DeloitteApril2017/DataForShare/")
messages=readLines("SMSSpamCollection.txt")

print(messages)
messages=na.omit(messages)
messages =data.frame(messages,stringsAsFactors = F)

library(tidyr)

messages=messages %>% separate(messages,into=c("target","msg"),sep="\t")



library(tm)
corpus= Corpus(VectorSource(messages$msg))

## data processing

trans=content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus =tm_map(corpus, trans, "/")
corpus =tm_map(corpus, trans, "@")
corpus =tm_map(corpus, trans, "\\|")
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(corpus)

View(as.matrix(dtm))

spdtm = removeSparseTerms(dtm, 0.99)
spdtm
View(as.matrix(spdtm))

message_feats = as.data.frame(as.matrix(spdtm))
rm(dtm,spdtm)

colnames(message_feats) = make.names(colnames(message_feats))

View(message_feats)

message_feats$target = as.factor(messages$target)

message_feats$msg_len=nchar(messages$msg)

set.seed(2)
s=sample(1:nrow(message_feats),0.7*nrow(message_feats))
message_feats_train=message_feats[s,]
message_feats_test=message_feats[-s,]

library(randomForest)

rf_model=randomForest(target~.,data = message_feats_train,do.trace=T)



rf_test_predict = predict(rf_model,newdata=message_feats_test)

table(message_feats_test$target,rf_test_predict)

varImpPlot(rf_model)



###


library(e1071)
#naivebayes
nb_model=naiveBayes(target~.,data = message_feats_train)

nb_model


nb_test_predict = predict(nb_model,dplyr::select(message_feats_test,-target))
table(message_feats_test$target,nb_test_predict)

#SVM model
svm_model= svm(target~.,data = message_feats_train,kernel='linear')

svm_test_predict = predict(svm_model,message_feats_test)

table(message_feats_test$target,svm_test_predict)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# further cleaning of data

# Remove numbers
corpus = tm_map(corpus, removeNumbers)
# Eliminate extra white spaces
corpus = tm_map(corpus, stripWhitespace) 

# you can pass your own vector in place of  stopwords("english") ,
# or modify stopwords("english") vector to add or remove stopwords
corpus = tm_map(corpus, removeWords, c("over", "term")) 

## Dr. Martin Porter's stemming algorithm
corpus = tm_map(corpus, stemDocument)

### weightTfIdf 
dtm =DocumentTermMatrix(corpus,
                        control = list(weighting = function(x) weightTfIdf(x)))

spdtm = removeSparseTerms(dtm, 0.99)
message_feats = as.data.frame(as.matrix(spdtm))

## make the column names syntactically correct. 
## you will not see any change for this particular example
colnames(message_feats) = make.names(colnames(message_feats))

message_feats$target = messages$target
message_feats$target=ifelse(message_feats$target=="spam",1,0)
message_feats$msg_len=nchar(messages$msg)
message_feats$target = as.factor(message_feats$target)
set.seed(2)
s=sample(1:nrow(message_feats),0.7*nrow(message_feats))
message_feats_train=message_feats[s,]
message_feats_test=message_feats[-s,]


rf_model=randomForest(target~.,data = message_feats_train,do.trace=T)

rf_test_predict = predict(rf_model,message_feats_test)
table(message_feats_test$target,rf_test_predict)

#naivebayes
nb_model=naiveBayes(target~.,data = message_feats_train)


nb_test_predict = predict(nb_model,dplyr::select(message_feats_test,-target))
table(message_feats_test$target,nb_test_predict)

#SVM model

svm_model= svm(target~.,data = message_feats_train,kernel='linear')


svm_test_predict = predict(svm_model,message_feats_test)
table(message_feats_test$target,svm_test_predict)

##############################################################
# knn and kmeans from other dataset
  
setwd("C:/Users/prtomar/Documents/R Training/reuters_data")

files_acq =list.files(getwd(), 
                      pattern="training_acq.*\\.txt")
acq=lapply(files_acq,readLines)

# acq,money,grain,earn,crude

files_crude =list.files(getwd(), 
                        pattern="training_crude.*\\.txt")
crude=lapply(files_crude,readLines)

files_earn =list.files(getwd(), 
                       pattern="training_earn.*\\.txt")

earn=lapply(files_earn,readLines)

files_money =list.files(getwd(), 
                        pattern="training_money.*\\.txt")
money=lapply(files_money,readLines)

files_grain =list.files(getwd(), 
                        pattern="training_grain.*\\.txt")
grain=lapply(files_grain,readLines)


file=data.frame("msg"=character(),"category"=character(),
                stringsAsFactors=FALSE)

for (i in 1:length(acq)){
  d=acq[[i]]
  k=paste(d,collapse=" ")
  file[nrow(file)+1,]=c(k,"acq")
}

for (i in 1:length(grain)){
  d=grain[[i]]
  k=paste(d,collapse=" ")
  file[nrow(file)+1,]=c(k,"grain")
}
for (i in 1:length(crude)){
  d=crude[[i]]
  k=paste(d,collapse=" ")
  file[nrow(file)+1,]=c(k,"crude")
}
for (i in 1:length(earn)){
  d=earn[[i]]
  k=paste(d,collapse = " ")
  file[nrow(file)+1,]=c(k,"earn")
}
for (i in 1:length(money)){
  d=money[[i]]
  k=paste(d,collapse = " ")
  file[nrow(file)+1,]=c(k,"money")
}

View(file)
rm(acq,earn,money,grain,crude)

train_data=file
rm(file)

files_acq =list.files(getwd(), 
                      pattern="test_acq.*\\.txt")
acq=lapply(files_acq,readLines)

files_crude =list.files(getwd(), 
                        pattern="test_crude.*\\.txt")
crude=lapply(files_crude,readLines)

files_earn =list.files(getwd(), 
                       pattern="test_earn.*\\.txt")
earn=lapply(files_earn,readLines)

files_money =list.files(getwd(), 
                        pattern="test_money.*\\.txt")
money=lapply(files_money,readLines)

files_grain =list.files(getwd(), 
                        pattern="test_grain.*\\.txt")
grain=lapply(files_grain,readLines)


file_test=data.frame("msg"=character(),"category"=character(),stringsAsFactors=FALSE)
for (i in 1:length(acq)){
  d=acq[[i]]
  k=paste(d,collapse = " ")
  file_test[nrow(file_test)+1,]=c(k,"acq")
}
for (i in 1:length(grain)){
  d=grain[[i]]
  k=paste(d,collapse = " ")
  file_test[nrow(file_test)+1,]=c(k,"grain")
}
for (i in 1:length(crude)){
  d=crude[[i]]
  k=paste(d,collapse = " ")
  file_test[nrow(file_test)+1,]=c(k,"crude")
}
for (i in 1:length(earn)){
  d=earn[[i]]
  k=paste(d,collapse = " ")
  file_test[nrow(file_test)+1,]=c(k,"earn")
}
for (i in 1:length(money)){
  d=money[[i]]
  k=paste(d,collapse = " ")
  file_test[nrow(file_test)+1,]=c(k,"money")
}
rm(acq,money,grain,crude,earn)
test_data=file_test
rm(file_test)

corpus= Corpus(VectorSource(train_data$msg))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))


dtm = DocumentTermMatrix(corpus)

spdtm = removeSparseTerms(dtm, 0.99)
msgs = as.data.frame(as.matrix(spdtm))

rm(dtm,spdtm)

colnames(msgs) = make.names(colnames(msgs))

msgs$target = train_data$category

msgs$target = as.factor(msgs$target)

set.seed(2)
s=sample(1:nrow(msgs),0.7*nrow(msgs))
msgs_train_knn=msgs[s,]
msgs_test_knn=msgs[-s,]

cl_train=msgs_train_knn$target
cl_test=msgs_test_knn$target

msgs_train_knn=msgs_train_knn %>% select(-target)

msgs_test_knn=msgs_test_knn %>% select(-target)

library(class)
knn_model= knn(msgs_train_knn,msgs_test_knn,cl_train)
table(Predictions = knn_model, Actual = cl_test)

  
########
# cos.sim = function(ix) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
# }   
# n = nrow(X) 
# cmb = expand.grid(i=1:n, j=1:n) 
# C =   matrix(apply(cmb,1,cos.sim),n,n)
#########

#Setting up working directory
getwd()
mydata=readLines("C:/Users/prtomar/Documents/R Training/amazon_reviews.txt")

mdata=str_trim(mydata)

L=mdata!=""

mdata=mdata[L]

rm(mydata)

amz_data =data.frame(mdata,stringsAsFactors = F)

rm(mdata)

even_indices=seq(2,62000,by=10)

amaz_data = data.frame(amz_data[even_indices,1])
rm(amz_data)
colnames(amaz_data)="reviews"

## Text Preprocessing Steps
myCorpus = Corpus(VectorSource(amaz_data$reviews)) 

### Removing junk values###
trans=content_transformer(function (x , pattern ) gsub(pattern, " ", x))

myCorpus =tm_map(myCorpus, trans, "&quot")
myCorpus =tm_map(myCorpus, trans, "<p>")
myCorpus =tm_map(myCorpus, trans, "<br>")
myCorpus=tm_map(myCorpus, content_transformer(tolower))
myCorpus=tm_map(myCorpus, removePunctuation)
myCorpus= tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus= tm_map(myCorpus, stripWhitespace)

### remove single letter ###

myCorpus=tm_map(myCorpus, removeWords, letters)
myCorpus = tm_map(myCorpus, stemDocument)
myCorpus = tm_map(myCorpus, removeNumbers)

dtm =TermDocumentMatrix(myCorpus) #rows are documents and columns are terms with their freq
m =as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)

library(wordcloud)

wordcloud(words = d$word, freq = d$freq, min.freq = 300,
          max.words=500, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))


dtm =TermDocumentMatrix(myCorpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
m =as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)   

set.seed(10)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
        max.words=800, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))
# 
myCorpus1= data.frame(text = sapply(myCorpus,as.character), stringsAsFactors = FALSE)
# 
View(myCorpus1)



#remove NAs in some_txt

myCorpus1 = myCorpus1[!is.na(myCorpus1)]


#Performing Sentiment Analysis #

# the classify_polarity function allows us to classify some text as positive or negative.This
#classification can be done by using a naive Bayes algorithm
## This process will classify the text data into four categories
#(pos - The absolute log likelihood of the document expressing 
#a positive sentiment, neg - The absolute log likelihood of the
#document expressing a negative sentimen, 
#pos/neg  - The ratio of absolute log likelihoods between 
#positive and negative sentiment scores where a score of 1 indicates 
#a neutral sentiment, less than 1 indicates a negative sentiment, 
#and greater than 1 indicates a positive sentiment; 
#AND best_fit - The most likely sentiment category 
#(e.g. positive, negative, neutral) for the given text)

# classify polarity
library(sentiment)
library(sentimentr)
library(Rstem)
class_pol = classify_polarity(myCorpus1, algorithm="bayes")

head(class_pol)



# get polarity best fit
polarity = class_pol[,4]
polarity

#sentiment package uses an in-built dataset "emotions", 
#which containing approximately 1500 words classified into 
#six emotion categories: anger, disgust, fear, joy, sadness,
#and surprise
## If any words outside this dataset are given, 
#the process will term the words as NA's

### classify emotion ###
class_emo = classify_emotion(myCorpus1, algorithm="bayes", prior=1.0)

head(class_emo)
# get emotion best fit
emotion = class_emo[,7]

# substitute NA's by "Neutral"
emotion[is.na(emotion)] = "neutral"
emotion


#Create data frame with the results and obtain some general statistics
# data frame with results
df = data.frame(reviews=myCorpus1, emotion=emotion, polarity=polarity, 
                stringsAsFactors=FALSE)
head(df)
View(df)
class(df)

library(dplyr)
df2= df %>%
  mutate(emotion=factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
glimpse(df2)


# plot distribution of emotions
library(ggplot2)
ggplot(df2, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories",y= "number of Reviews")

# plot distribution of polarity
ggplot(df2, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of Reviews")

#Separate the text by emotions and visualize the words 
#with a comparison cloud separating text by emotion
emos = levels(factor(df2$emotion))
n_emos=length(emos)
emo.docs = rep("", n_emos)

for (i in 1:n_emos)
{
  tmp = subset(myCorpus1,emotion == emos[i])
  emo.docs[i] = paste(tmp, collapse=" ")
}

# create corpus
corpus = Corpus(VectorSource(emo.docs))

#remove white spaces
corpus= tm_map(corpus, stripWhitespace)
corpus= tm_map(corpus, tolower)
corpus= tm_map(corpus, removeWords, stopwords("english"))
corpus= tm_map(corpus, stemDocument,language = ("english"))
corpus= tm_map(corpus, removePunctuation)
corpus= tm_map(corpus, removeNumbers)


#create Term document matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(n_emos, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, 
                 title.size = 1.5)

# Word Associations
# which words are associated with 'ipod'?
findAssocs(dtm, terms = "ipod", corlimit = 0.10)
# how does it find the correlation
#Docs word1 word2 word3 word4 word5 
#1     1     0     0     0     0 
#2     1     1     0     0     0 
#3     1     1     1     0     0 
#4     1     1     1     1     0 
#5     1     1     1     1     1 
# findAssocs(dtm, "word2", 0.1) 
#word2 word3 word4 word5 
#1.00  0.61  0.41  0.25 
# Correlation word2 with word3 
#cor(c(0,1,1,1,1),c(0,0,1,1,1)) 
#0.6123724 
# Correlation word2 with word4 
#cor(c(0,1,1,1,1),c(0,0,0,1,1)) 
#0.4082483 
# Correlation word2 with word5 
#cor(c(0,1,1,1,1),c(0,0,0,0,1)) 
#0.25 



dtm2=removeSparseTerms(dtm, sparse = 0.99) # 0.99 indicates the word is missing 98% of the time

freq.terms= findFreqTerms(dtm2, lowfreq = 50)
plot(dtm2, term = freq.terms, corThreshold = 0.25, 
     weighting = F, attrs=list(node=list(width=20, 
                                         fontsize=24, 
                                         fontcolor="blue", 
                                         color="red")))


library(topicmodels)

lda_data=myCorpus[1:15]

dtm=TermDocumentMatrix(lda_data)
dtm_topic= as.DocumentTermMatrix(dtm)
lda= LDA(dtm_topic, k = 3) 
lda # find 3 topics
summary(lda)
term =terms(lda, 5)
term# first 5 terms of every topic

term =apply(term, MARGIN = 2, paste, collapse = ",")
term

topics= topics(lda,1) # 1st topic identified for every document

topics = data.frame( topic=topics)



