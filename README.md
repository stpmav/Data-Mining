Data-Mining
===========

install.packages(c("tm","XML","e1071"))
install.packages("SnowballC")
install.packages("RTextTools")
install.packages("randomForest")
library(randomForest)
library(RTextTools)
library(SnowballC)
library(tm)
library(XML)
library(e1071)
library(NLP)

setwd("F:\\Data Mining\\Reuters Ex\\Files")
#setwd("/media/NITISH/Data Mining/Reuters Ex/Files")

reut000 <- ("reut2-000.xml")
rs000 <- ReutersSource(reut000)
rc0 <- Corpus(rs000)

reut001 <- ("reut2-001.xml")
rs001 <- ReutersSource(reut001)
rc1 <- Corpus(rs001)

reut002 <- ("reut2-002.xml")
rs002 <- ReutersSource(reut002)
rc2 <- Corpus(rs002)

reut003 <- ("reut2-003.xml")
rs003 <- ReutersSource(reut003)
rc3 <- Corpus(rs003)

reut004 <- ("reut2-004.xml")
rs004 <- ReutersSource(reut004)
rc4 <- Corpus(rs004)

reut005 <- ("reut2-005.xml")
rs005 <- ReutersSource(reut005)
rc5 <- Corpus(rs005)

reut006 <- ("reut2-006.xml")
rs006 <- ReutersSource(reut006)
rc6 <- Corpus(rs006)

reut007 <- ("reut2-007.xml")
rs007 <- ReutersSource(reut007)
rc7 <- Corpus(rs007)

reut008 <- ("reut2-008.xml")
rs008 <- ReutersSource(reut008)
rc8 <- Corpus(rs008)

reut009 <- ("reut2-009.xml")
rs009 <- ReutersSource(reut009)
rc9 <- Corpus(rs009)

reut010 <- ("reut2-010.xml")
rs010 <- ReutersSource(reut010)
rc10 <- Corpus(rs010)

reut011 <- ("reut2-011.xml")
rs011 <- ReutersSource(reut011)
rc11 <- Corpus(rs011)

reut012 <- ("reut2-012.xml")
rs012 <- ReutersSource(reut012)
rc12 <- Corpus(rs012)

reut013 <- ("reut2-013.xml")
rs013 <- ReutersSource(reut013)
rc13 <- Corpus(rs013)

reut014 <- ("reut2-014.xml")
rs014 <- ReutersSource(reut014)
rc14 <- Corpus(rs014)

reut015 <- ("reut2-015.xml")
rs015 <- ReutersSource(reut015)
rc15 <- Corpus(rs015)

reut016 <- ("reut2-016.xml")
rs016 <- ReutersSource(reut016)
rc16 <- Corpus(rs016)

reut017 <- ("reut2-017b.xml")
rs017 <- ReutersSource(reut017)
rc17 <- Corpus(rs017)

reut018 <- ("reut2-018.xml")
rs018 <- ReutersSource(reut018)
rc18 <- Corpus(rs018)

reut019 <- ("reut2-019.xml")
rs019 <- ReutersSource(reut019)
rc19 <- Corpus(rs019)

reut020=("reut2-020.xml")
rs020=ReutersSource(reut020)
rc20=Corpus(rs020)

reut021=("reut2-021.xml")
rs021=ReutersSource(reut021)
rc21=Corpus(rs021)

reuters=c(rc0, rc1, rc2, rc3, rc4, rc5, rc6, rc7, rc8, rc9, rc10, rc11,
       rc13, rc14, rc15, rc16, rc17, rc18, rc19, rc20, rc21)

reuters

Data=as.data.frame(matrix(0,nrow=20578,ncol=3))
colnames(Data)=c("TOPICS","Topics","LSPLIT")
for(i in 1:20578){
  Folder=reuters[[i]]
  Data[i,1]=LocalMetaData(Folder)$TOPICS
  Data[i,2]=LocalMetaData(Folder)$Topics[1]
  Data[i,3]=LocalMetaData(Folder)$LEWISSPLIT
 
}

Data1=as.integer(rownames(Data[
  is.element(Data$Topics, c("crude","earn","acq","money-fx",
  "grain","trade","interest","ship","wheat","corn")) & 
  (is.element(Data$LSPLIT, c("TEST","TRAINING-SET","TRAIN"))),]))

TestData=as.integer(rownames(Data[is.element(Data$Topics, c("crude","earn","acq",
         "money-fx","grain","trade","interest","ship","wheat","corn")) & 
          is.element(Data$LSPLIT,"TEST"),]))

TrainData=as.integer(rownames(Data[is.element(Data$Topics, c("crude","earn","acq",
          "money-fx","grain","trade","interest","ship","wheat","corn")) & 
           is.element(Data$LSPLIT,"TRAIN"),]))


Data.F=reuters[Data1]

#Data Prepocessing
reuters.p=tm_map(Data.F,as.PlainTextDocument)
reuters.p=tm_map(reuters.p,tolower)
reuters.p=tm_map(reuters.p,removeWords,stopwords("english"))
reuters.p=tm_map(reuters.p,stemDocument)
reuters.p=tm_map(reuters.p,removeNumbers)
reuters.p=tm_map(reuters.p,removePunctuation)
reuters.p=tm_map(reuters.p,stripWhitespace)

dtm=DocumentTermMatrix(reuters.p)
TfIdf=weightTfIdf(dtm) #TF*IDF
DFterms=as.data.frame(inspect(TfIdf))

Keywords=as.data.frame(inspect(removeSparseTerms(TfIdf, 0.95)))
ncol(Keywords)
#summary(Keywords)
#Keywords[1:5,]
#summary(reuters.p)



#CLASSIFIERS

P.class=as.factor(Data[TrainData,2])

NB=naiveBayes(Keywords[1:length(TrainData),], P.class)
T=table(as.factor(Data[TrainData,2]), predict(NB,Keywords[1:length(TrainData),]))
sum(diag(T))/sum(T)
A= sum(diag(T))/sum((upper.tri(T)+lower.tri(T)))

SVM=svm(Keywords[1:length(TrainData),], P.class)
TSVM=table(as.factor(Data[TrainData,2]), predict(SVM,Keywords[1:length(TrainData),]))
TestSVM=table(as.factor(Data[TestData,2]), predict(SVM,Keywords[(length(TrainData)+1):length(list),]))
sum(diag(TSVM))/sum(TSVM)

RF=randomForest(Keywords[1:length(TrainData),], P.class)
TRF=table(as.factor(Data[TrainData,2]), predict(RF,Keywords[1:length(TrainData),]))
sum(diag(R))/sum(R)
RTest=table(as.factor(Dati[Test,2]), predict(RF,words[(length(Train)+1):length(list),]))

#CLUSTERING

cluslist=as.integer(rownames(Data[
  is.element(Data$Topics, c("earn","acq","grain","trade"))
  & is.element(Data$LSPLIT,c("TRAIN","TEST")),]))

Data.F2=reuters[cluslist]

Data.F3=tm_map(Data.F2 , as.PlainTextDocument)
Data.F3=tm_map(Data.F3 , stripWhitespace)
Data.F3=tm_map(Data.F3 , tolower)  	
Data.F3=tm_map(Data.F3 , removeWords, stopwords("english"))
Data.F3=tm_map(Data.F3 , stemDocument)
Data.F3=tm_map(Data.F3 , removePunctuation)
Data.F3=tm_map(Data.F3 , removeNumbers)

DTM2=DocumentTermMatrix(Data.F3)
#DFterms0 <- as.data.frame(inspect(dtm))
Tf_IDF2=weightTfIdf(dtm)  #TF*IDF
DF2=as.data.frame(inspect(Tf_IDF2))

Keyword2=(as.matrix(inspect(removeSparseTerms(Tf_IDF2, 0.95))))
v=factor(Data[cluslist,2])

D=dist(Keywords, method="euclidean")  
