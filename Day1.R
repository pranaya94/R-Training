# Quick walkthrouh : R

# Text data focus on R : handling Strings

# gathering text data from external sources

# general predictive models with text data
    # -classification, regression
    # -Clustering

# Text Specific Analysis
    # -Word Cloud
    # -Sentiment Analysis
    # -Topic Modeling
    # -Query search/matching

# Overview for further exploration
    # -POS tagging
    # -Machine translations
    # -Chat Bots



##Day 1
## Quick recap of R


# for commenting  shortcut is : ctrl+shift+c

# to comment multiple lines
# at once, select those line
# and use the same short cut 


x=5
y="any word"
z=TRUE


# TRUE/FALSE : T/F

# shortcut to execute code : ctrl+enter


# Rules for naming your objects

# Allowed Special Characters : you can use dots(.) and underscore ( _ )
# Numbers : Can be used anywhere in the name, BUT not at the begining
# no spaces allowed in the object names


## ------------------------------------------------------------------------

# Shortcut to clear console use : ctrl+l
## ------------------------------------------------------------------------

class(x)
x="delhi"

## Type conversion

v1="21"
v1+2 


v2=as.numeric(v1)

v2+2

as.numeric(v1)

v1="king"

v2=as.numeric(v1)
class(v2)



## ---------------------Numeric Operations---------------------------------

x=2
y=8

x+y
x-y
x*y
x/y

x^y
x**y

## ------------------------------------------------------------------------

z=(x+y-(x/y))**(x/10)

z

## ------------------------------------------------------------------------
tan(10)
log(2^14,10)
exp(34)
abs(-98)


## ----------------------String Operations--------------------------------
x="Sachin"
y="Tendulkar"
z="Cricket"

paste(x,y,z)

paste(x,y,z,sep="$#")
paste(x,y,z,sep="")
paste0(x,y,z)


## ------------------------------------------------------------------------

address="1612-Lone Tower-Mumbai"

newAd=sub("-","/",address)
newAd

newAd1=gsub("-","/",address)
newAd1


## ------------------------------------------------------------------------
ip="192.168.23.134:219"            
abc=substr(ip,16,19)
abc


q=12345
w=substr(q,1,2)
class(w)


## ------------------------------------------------------------------------
x="asdnakdn dkjnasd 234524&%$$"
nchar(x)

## exercise
s1="sdkfnskfn47547"

substr(s1,5,nchar(s1))
substr(s1,5,10000)


## --------------------Writing Conditions-------------------------------
x=7
y=9

x>y
x<y
x==y
x!=y
x>=y
x<=y

# ! is used for negation or flipping the conditions

## ------------------------------------------------------------------------
x=10

x>=1 & x<=19

y="SAchin"
y=="Sachin" | y=="SACHIN"


## ---------------------Vectors--------------------------------
x=c(2,3,4,"a")
x
class(x)

x=c(2,4,89,-10,78,6.6)
class(x)


## ------------------------------------------------------------------------
x
x[3]


x[1,2,5]

## ------------------------------------------------------------------------
p=c(1,2,5)

x[p]

x[c(1,2,5)]

x[-3]

x[c(-1,-2,-5)]

x[-c(1,2,5)]

x[c(-1,2,5)]


## ------------------------------------------------------------------------

x[c(3,4,2,11,2,6,3,10)]


# they need not be in order
# need not be unique
# need to be less than the length of vector



## ------------------------------------------------------------------------

x
x>4
L=x>4
L

x[L]

x[!L]


x[x>4]

## exercise

x=c(3,4,5,1,2,10,5,7)

y=c(3,4,8,-9,0,12,6,99)

# extract those values from x where 
# corresponding values of y are larger than 6
x[y>6]


## ----------------------Vector Operations------------------------------
x=1:10
y=seq(1,12,length=10)   

x+y
x*y
x-y
x/y

log(x)
2**x

x=c("acs","asjdabdj","asjdhbad","dfjgheg")
nchar(x)


x=1:10
y=rep("#",10)
paste0(x,y)


## ------------------------------------------------------------------------
x=1:11
y=c(10,20,30)



x+y

## ------------------------------------------------------------------------
x=c("a","b")
y=1:10
paste0(x,y)

### sample

x=sample(letters,10)
y=c("a","b","c","d","e","f")

match(x,y)

### %in%

x %in% y

y %in% x

### which

which(c(T,F,T,F,T,T,F))

###

## create a vector which contains numbers from 1 to 100
# which are divisible by 3, but not by 5 and 7

100 %% 33



x[x%%3==0 & x%%5!=0 & x%%7!=0]
###
x=1:100
y=seq(3,100,by=3)
z=seq(5,100,by=5)
m=seq(7,100,by=7)

###
y[!(y %in% z | y %in% m)]

## ------------------------------------------------------------------------

d=mtcars
# lists of data frames in R : data()
# lalit.sachan@edvancer.in

View(d)
dim(d)
names(d)
colnames(d)
rownames(d)


## ------------------------------------------------------------------------
d[3,6]

## ------------------------------------------------------------------------
#3rd row , all columns

d[3,]
class(d[3,])

# all rows, 6 th colunmn

d[,6]
class(d[,6])


# multiple rows, multiple column

d[c(3,4,20),c(1,4,6)]


d[-(3:17),-c(1,4,6,7)]


d[,c("wt","mpg")]


# subsetting rows on condition
am

d[ d$am==0 & d$mpg>15 ,c("wt","mpg","am")]

d1=d[,c("wt","mpg","am")]

d1$v1=100

d1$v2=d$wt+d$mpg


d1$v3=ifelse(d1$mpg>20,"L","H")
d1$v4=ifelse(d1$mpg>20,10000,d1$wt)


###
install.packages("dplyr")

## ------------------------------------------------------------------------
library(dplyr)

df1 = data.frame(CustomerId=c(1:6),
                 Product=c(rep("Toaster",3),rep("Radio",3)))

df2 = data.frame(CustomerId=c(3,4,7,8),
                State=c(rep("Alabama",2),rep("Ohio",2)))


inner=inner_join(df1,df2,by="CustomerId")
left=left_join(df1,df2,by="CustomerId")
right=right_join(df1,df2,by="CustomerId")
full=full_join(df1,df2,by="CustomerId")


semi=semi_join(df1,df2,by="CustomerId")
anti=anti_join(df1,df2,by="CustomerId")

## what if the name of the key is not same
# how to use multiple keys

#####

## ----Data Processing with dplyr----------------------------------------
library(dplyr)
library(hflights)


## --------------------------filter-----------------------------------
flights=tbl_df(hflights)
flights

#note: you can use comma or ampersand to represent AND condition

?hflights

filter(flights, Month==1, DayofMonth==1)
filter(flights, Month==1 & DayofMonth==1)


# use pipe for OR condition
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

# you can also use %in% operator

filter(flights, UniqueCarrier %in% c("AA", "UA"))

## ---------------------------select-------------------------------------
select(flights, DepTime, ArrTime, FlightNum)


## ------------------------------------------------------------------------
select(flights, Year:DepTime, contains("Taxi"), contains("Delay"))

### select obs where uniquecarrier is in ("UA","AA"), then from there
### select only columns DepDelay, DayOfWeek

d1=filter(flights, UniqueCarrier %in% c("AA", "UA"))

d2=select(d1,DayOfWeek,DepDelay)

select(filter(flights, UniqueCarrier %in% c("AA", "UA")),
       DayOfWeek,DepDelay)

## shortcut is ctrl+shift+m : %>% 

x=1:10

sum(log(x))

x %>% 
  log() %>% 
  sum()


## ----------------------- %>% Operator------------------------

d1=flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)


## ----------------------- arrange------------------------

d2=flights %>%
  select(UniqueCarrier,DayOfWeek, DepDelay) %>%
  arrange(DayOfWeek,desc(DepDelay))


## --------------------------mutate-----------------------------------

flights %>%
  select(Distance, AirTime,ArrTime,TaxiIn,DepDelay,DepTime) %>%
  mutate( Deftime=DepTime-ArrTime, 
          Speed= Distance/AirTime,
          DepDelay_flag=as.numeric(DepDelay>60),
          DepDelay=ifelse(DepDelay>60,61,DepDelay),
          DepDelay_flag2=as.numeric(DepDelay>60))


## ------------------------summarise & group_by------------------------------

d4=flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay,na.rm=T))


# mean(c(1,2,3,4,NA),na.rm=T)

flights %>% 
  group_by(Dest) %>% 
  summarise(flight_count=n(),
            num_planes=n_distinct(TailNum))


##
## find out number of flights , number of cancelled flights every month
## and monthly change in number of flights [hint: use function lag]

flights %>% 
  group_by(Month) %>% 
  summarise(num_flights=n(),
            can_flights=sum(Cancelled==1)) %>% 
  mutate(lagged_count=lag(num_flights),
         change=num_flights-lagged_count) %>% 
  select(-lagged_count,-num_flights)

##

## select all the flights belonging to UA,AA,WN
## group by month and get avg delay ( separately for each unique carrier)
## sort the entire data in descending manner by avg_delay
## store this result in some data frame

d=flights %>% 
  filter(UniqueCarrier %in% c("UA","AA","WN")) %>% 
  group_by(Month,UniqueCarrier) %>% 
  summarise(avg_del=mean(DepDelay,na.rm=T)) %>% 
  arrange(desc(avg_del))


#### ungroup, slice

## Find out the first two most delayed flights for each month

# package : stringr

##Regular expression
#Till now we have seen sub(), gsub(), grep() function in base R

# 1 : we know how to match by providing exact letters or digit



myvec=c("a7bc123xyz","1define456","789sth","379tut26")
gsub("123","MATCH",myvec)

# substitute all the strings having digits
# \\d stands for a digit in regular expression

gsub("\\d","MATCH",myvec)


gsub("\\d\\d\\d","MATCH",myvec)


gsub("\\d\\d9","MATCH",myvec)

#exercise:
myvec=c("abc","ghi","xyz","$mys129")
#substitute all the letters with a single "Match"
#Hint: \\w


gsub("\\w\\w\\w","Match",myvec)
## we'll come back to it , how to target only alphabets
## \\w : means any character including numbers except special charactaers


# 2 : . means any single character, for symbol . use \\. 

myvec=c("ab@c","123#","$qwe.123","...")

gsub(".","MATCH",myvec)

gsub("\\.","MATCH",myvec)

#exercise : substitute first two members only with a single MATCH

myvec=c("896.","?Q+.","abc1")

gsub("...\\.","Match",myvec)

# 3 : replacing multiple characters at once

myvec=c("<#abc","#abc","abc%")


gsub("[<#%]","MATCH",myvec)


# 4 : excluding a pattern with metacharacter ^

myvec=c("<abc","#1abc","abc%")


gsub("[^abc]","MATCH",myvec)

# 5 : character ranges with -

myvec=c("<ab:c","#def","zghi%","a:z")

gsub("[^a-z]","MATCH",myvec)


gsub("[a-z]","MATCH",myvec)
gsub("[^a-z]","MATCH",myvec)

#exercise:
myvec=c("Ana","Bob","Cpc","aax","bby","ccz")

# Match  Ana,Bob,Cpc

# Skip	aax,bby,ccz

gsub("[A-Z]\\w\\w","MATCH",myvec)
gsub("[A-Z][a-z][a-z]","Match",myvec)




# 6 : more compact way for repetition

myvec=c("abc123xyz","define456","789sth","379tut6","3798tut6")

gsub("\\d\\d\\d","MATCH",myvec)

gsub("\\d{3}","MATCH",myvec)

gsub("\\d{3,}","MATCH",myvec)

myvec=c("abc123xy1234z12345","def4567ine456","789sth","379tut6")

gsub("\\d{3,4}","MATCH",myvec)


gsub("\\d{1,}","MATCH",myvec)


# 7 : Kleene Plus and Kleene Star

# * means it matches zero or more character
# + means it matches atleast one or more character

# people names
people = c("rori", "emmilia", "matteo", "mehmemt", "filipe", "anna", "tyler",
           "rasmus", "mt jacob", "youna", "flora", "adi mmt")

# match m zero or more times, and t
gsub("m*t","MATCH",people)


# match m atleast one or more times
gsub("m+t","MATCH",people)


# 8 : metacharacter or with ?

myvec=c("ac","abc","ab?c","12ac")

gsub("abc","MATCH",myvec)

gsub("ab?c","MATCH",myvec)

gsub("ab?\\??c","MATCH",myvec)

# 9 : \\s stands for any white space : " " , \\t , \\n

# 10 : ^ start and $ end
myvec=c("file_record_transcript.pdf","file_07241999.pdf",
        "fileabcpdf","file.pdf","fileabc.pdf","testfile_fake.pdf.tmp",
        "file_record_transcript.pdff")

gsub("^file.+\\.pdf$","MATCH",myvec)


# 11 : groups with ()

myvec=c("ac","abc","aQAc","12ac")

gsub("ab?Q?A?c","MATCH",myvec)

gsub("ab?QA?c","MATCH",myvec)

gsub("ab?(QA)?c","MATCH",myvec)


