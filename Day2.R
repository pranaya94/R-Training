## Ex1
df1 = data.frame(CustomerId=c(1:6),
                 Product=c(rep("Toaster",3),rep("Radio",3)))

df2 = data.frame(Id=c(3,4,7,8),
                 State=c(rep("Alabama",2),rep("Ohio",2)))
## you can use rename and use old ways or do following

d=inner_join(df1,df2,by=c("CustomerId"="Id"))

# Ex2 : left for you

# Ex3 : slice

df = data.frame(id=c(1,1,1,2,2,2,3,3,3), 
                 stopId=c("a","b","c","a","b","c","a","b","c"), 
                 stopSequence=c(1,2,3,3,1,4,3,1,2))


df %>% arrange(stopSequence) %>% 
  group_by(id) %>% 
  slice(c(1,4))

### Ex4

emails=c("a@b.c","abc.tfg_a.com","<lalit>lalit.sachan@edvancer.in",
         "qwe@op.co.in","lalit sachan @deloitte.com","LALIT@gmail.com")

pattern="\\w+@\\w+\\.\\w+\\.?\\w+"


##stringr
library(stringr)

####
paste("May", "The", "Force", NULL, "Be", "With", NULL,character(0), "You", character(0),sep="-")
str_c("May", "The", "Force", NULL, "Be", "With", NULL,character(0), "You", character(0),sep="-")


###
some_text = c("one", "two", "three", NA, "five")
nchar(some_text)
str_length(some_text)

####


some_factor = factor(c(1, 1, 1, 2, 2, 2), labels = c("good", "bad"))
nchar(some_factor)
str_length(some_factor)

#####

lorem = "Lorem Ipsum"
str_sub(lorem, start = 1, end = 4)

# equivalent to 'substring'
substring(lorem, first = 1, last = 4)

str_sub("adios", 1:3)


# it also has the ability to work with negative indices in the start
#and end positions
resto = c("brasserie", "bistrot", "creperie", "bouchon")

substring(resto, start = -4, end = -1)


str_sub(resto, start = -4, end=-1)

s1="abcdefghijklmnopqrstuvw"

str_sub(s1, start = -10, end=2)

####
s="some string"
substr(s,1,1)="@@"
s

s="some string"
str_sub(s,1,1)="@@"
s


#If you want to duplicate any string, R doesnt have any specific 
# function, but stringr package does provide that

str_dup("hola", 3)


str_dup("adios", 1:3)


#extracting a word

change = c("Be,the,change asdh", "you,want,to asjabd be")

word(change, 1)

word(change, 4,sep="[, ]")


# str detect(): Detect the presence or absence of a pattern in a string
# str extract(): Extract first piece of a string that matches a pattern
# str extract all(): Extract all pieces of a string that match a pattern
# str locate(): Locate the position of the first occurence of a pattern in a string
# str locate all(): Locate the position of all occurences of a pattern in a string
# str replace(): Replace first occurrence of a matched pattern in a string
# str replace all(): Replace all occurrences of a matched pattern in a string
# str split(): Split up a string into a variable number of pieces


# tweets about Paris
paris_tweets = c(
  "#Paris is chock-full of cultural and culinary attractions",
  "Some time in #Paris along Canal St.-Martin famous by #Amelie",
  "While you re in #Paris, stop at cafe: http://goo.gl/yaCbW",
  "Paris, the city of light")

# hashtag pattern
pattern = "#[a-zA-Z]{1,}"

# extract (first) hashtag
str_detect(paris_tweets, pattern)

str_extract(paris_tweets, pattern)

str_extract_all(paris_tweets, pattern)


str_locate(paris_tweets, pattern)

str_locate_all(paris_tweets, pattern)



### Splitting a string
# Explore strsplit Vs str_split


#Exercise
# some strings
strings = c("12 Jun 2002", "22-July-2009 ",
            "01 01 2001", "date", "02.06.2000",
            "xxx-yyy-zzzz", "$2,600"," 8 September 2004 "," 8 September 04 ")


# date pattern (month as text)



dates = "([0-9]{1,2})[- .]([a-zA-Z]+)[- .]([0-9]{4})"


str_extract(strings,dates)
# detect dates
strings[str_detect(strings, dates)]

########### Web Scraping ############

library(rvest)
library(RCurl)
library(XML)
library(stringr)
library(xml2)

# http://selectorgadget.com/


Fantastic_Beasts= read_html("http://www.imdb.com/title/tt3183660/")

cast=Fantastic_Beasts %>%
  html_nodes(".itemprop") %>% 
  html_nodes("span") %>% 
  html_text()




review=Fantastic_Beasts %>%
  html_nodes("#titleUserReviewsTeaser p") %>% # p is to wrap text in paragraph
  html_text()
review

storyline=Fantastic_Beasts %>%
  html_nodes("#titleStoryLine p") %>%
  html_text()
storyline


#Naukri

uagent="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.87 Safari/537.36"
jobs=html_session("https://www.naukri.com/premium-data-analyst-jobs-in-india")
jobs



organization = jobs %>% 
  html_nodes(".org" ) %>% 
  html_text

jobtitle = jobs %>% 
  html_nodes(".desig" ) %>% 
  html_text

experience = jobs %>% 
  html_nodes(".exp" ) %>% 
  html_text

location = jobs %>% 
  html_nodes(".loc" ) %>% 
  html_text



df =data.frame(organization,jobtitle,experience,location)
View(df)



#Exercise
sbi=read_html("https://in.finance.yahoo.com/quote/SBIN.BO?p=SBIN.BO",uagent=uagent)
sbi

mydata=sbi %>% 
  html_nodes("table")

mydata=mydata[2] %>% html_nodes("tr>td") %>% html_text()

mydata=matrix(mydata,ncol=2,byrow = T)

mydata=as.data.frame(mydata)
names(mydata)=c("Detail","Value")

View(mydata)

#### Twitter

library(twitteR)
#create a twitter app and generate the token keys
# replace "\" with "/"
key= scan("~/Dropbox/Trainings/DeloitteApril2017/riya_twit_info.txt", 
          what="character") 
key
consumer_key= key[2] 
consumer_secret= key[4] 
access_token= key[6] 
access_secret= key[8]

#to access tweets
setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_secret)

#top 10 tweets for #tesla
tweets=searchTwitter("#Tesla", n=10, lang="en")
tweets


# transform tweets to data frame
df= twListToDF(tweets)
dim(df)

tweets_bh= searchTwitter("Samsung Galaxy", n=10, lang="en")
tweets_bh[1] # will show the first tweet
#transform tweets to data frame
df2=twListToDF(tweets_bh)
dim(df2)
View(df2)

#################################################################

for(i in 1:10){
  print("hello")
}

### print mean of each column in mtcars

for(i in 1:ncol(mtcars)){
  print(mean(mtcars[,i]))
}

for(i in c("abc","delhi","gurgaon")){
  x=nchar(i)
  print(x)
}

### explore while loops

x=sample(letters,1000,replace = T)

t=table(x)

t=sort(t,decreasing = T)

names(t)[1:3]

get_top3=function(input_vec){
  t=table(input_vec)
  
  t=sort(t,decreasing = T)
  
  top3=names(t)[1:3]
  
  return(top3)
}

y=sample(letters,10000,replace = T)

get_top3(y)


myfunc=function(x=1,y=10,z=100){
  
  return(x+y+z)
}

myfunc(3,4,5)
myfunc(3,4)

myfunc(z=7,x=3)

###################

library(dplyr)

setwd("~/Dropbox/Trainings/DeloitteApril2017/DataForShare/")

bd=read.csv("bank-full.csv",sep=";",stringsAsFactors = T)

set.seed(2)
s=base::sample(1:nrow(bd),0.7*nrow(bd))
bd_train=bd[s,]
bd_test=bd[-s,]

log.fit=glm(y~.,data=bd_train,family="binomial")

# log.fit$coefficients

log.fit2=step(log.fit)

# log.fit2$coefficients

test.score=predict(log.fit2,newdata=bd_test,type="response")

library(pROC)
auc(roc(bd_test$y,test.score))

######
library(randomForest)
rf.fit=randomForest(y ~ .,data=bd_train,
                    do.trace=T)

test.score=predict(rf.fit,newdata=bd_test,type="prob")[,1]
auc(roc(bd_test$y,test.score))

### cvTools for hype parameter tuning

#################
train.score=predict(log.fit2,newdata=bd_train,type="response")

cutoff=0.2
pred=ifelse(train.score>cutoff,"yes","no")
real=bd_train$y

TP=sum(real=="yes" & pred=="yes")
FP=sum(real=="no" & pred=="yes")
FN=sum(real=="yes" & pred=="no")
TN=sum(real=="no" & pred=="no")

P=TP+FN
N=TN+FP

Accuracy=(TP+TN)/(P+N)

KS=(TP/P)-(FP/N)

precision=TP/(TP+FP)
recall=TP/P
### beta=0.1 , beta=5
f0.1=1.01*precision*recall/((.01*precision)+recall)
f5=26*precision*recall/((25*precision)+recall)


cutoffs=seq(.01,.99,by=0.01)
cutoff_data=data.frame(cutoff=999,KS=999,f0.1=999,f5=999)

for(cutoff in cutoffs){
  pred=ifelse(train.score>cutoff,"yes","no")
  real=bd_train$y
  
  TP=sum(real=="yes" & pred=="yes")
  FP=sum(real=="no" & pred=="yes")
  FN=sum(real=="yes" & pred=="no")
  TN=sum(real=="no" & pred=="no")
  
  P=TP+FN
  N=TN+FP
  
  Accuracy=(TP+TN)/(P+N)
  
  KS=(TP/P)-(FP/N)
  
  precision=TP/(TP+FP)
  recall=TP/P
  ### beta=0.1 , beta=5
  f0.1=1.01*precision*recall/((.01*precision)+recall)
  f5=26*precision*recall/((25*precision)+recall)
  
  cutoff_data=rbind(cutoff_data,c(cutoff,KS,f0.1,f5))
}

cutoff_data=cutoff_data[-1,]

KS.cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

f0.1.cutoff=cutoff_data$cutoff[which.max(cutoff_data$f0.1)]
f5.cutoff=cutoff_data$cutoff[which.max(cutoff_data$f5)]






