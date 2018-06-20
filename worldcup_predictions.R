data<-read.csv("fifa_ranking.csv")
year<-format(as.Date(data$rank_date),"%Y")
data$rank_year<-year
data$rank_date<-NULL
data$country_abrv<-NULL
suppressWarnings(library("caTools"))
suppressWarnings(library("caret"))
suppressWarnings(library("rpart"))
suppressWarnings(library("rpart.plot"))
suppressWarnings(library("cluster"))
suppressWarnings(library("flexclust"))
suppressWarnings(library("dplyr"))
data1<-data
test2018<-subset(data1,as.character(rank_year)=="2018")
test2018$rank_year<-NULL
conf<-test2018$confederation
test2018$confederation<-NULL
df1<-aggregate(.~ country_full, FUN = median, data=test2018)
test2018$confederation<-conf
a<-aggregate(confederation~ country_full,data = test2018, FUN=unique)
test2018<-cbind(df1,confederation=a[,2])



totaldf<-NULL
years<-unique(data$rank_year)
for(i in years){
  yeardata<-subset(data,rank_year==i)
  yeardata$rank_year<-NULL
  conf<-yeardata$confederation
  yeardata$confederation<-NULL
  df<-aggregate(.~ country_full, FUN = median, yeardata)
  yeardata$confederation<-conf
  a<-aggregate(confederation~ country_full,data = yeardata, FUN=unique)
  df<-cbind(df,confederation=a[,2])
  ##df$rank<-floor(df$rank)
  totaldf<-rbind(totaldf,df)
}




data<-totaldf
data$top5<-ifelse(data$rank<=5,1,0)
data$country_full<-NULL
totaldf1<-totaldf


totaldf<-data
totaldf$confederation<-as.numeric(totaldf$confederation)
totaldf$rank<-NULL
totaldf$top5<-NULL


set.seed(1)

preproc<-preProcess(totaldf)
totaldf<-predict(preproc,totaldf)

kmeans<-kmeans(totaldf,centers=6)
clusters<-kmeans$cluster

train1<-subset(data,clusters==1)
train2<-subset(data,clusters==2)
train3<-subset(data,clusters==3)
train4<-subset(data,clusters==4)
train5<-subset(data,clusters==5)
train6<-subset(data,clusters==6)

train1$rank<-NULL
train2$rank<-NULL
train3$rank<-NULL
train4$rank<-NULL
train5$rank<-NULL
train6$rank<-NULL

kmeans.kcca<-as.kcca(kmeans,data=totaldf)
temp<-test2018
temp$country_full<-NULL
temp$rank<-NULL
temp$confederation<-as.numeric(temp$confederation)
temp<-predict(preproc,temp)

##predicting clusters of 2018 data
predictedcluster<-predict(kmeans.kcca,newdata=temp)

suppressWarnings(model1<-glm(top5~.,data = train1,family = "binomial"))
suppressWarnings(model2<-glm(top5~.,data = train2,family = "binomial"))
suppressWarnings(model3<-glm(top5~.,data = train3,family = "binomial"))
suppressWarnings(model4<-glm(top5~.,data = train4,family = "binomial"))
suppressWarnings(model5<-glm(top5~.,data = train5,family = "binomial"))
suppressWarnings(model6<-glm(top5~.,data = train6,family = "binomial"))

##predict for 2018 test dataset

odds<-vector()
k<-1
for(i in predictedcluster){
  if(i==1){
    suppressWarnings(output<-predict(model1,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  else if(i==2){
    suppressWarnings(output<-predict(model2,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  else if(i==3){
    suppressWarnings(output<-predict(model3,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  else if(i==4){
    suppressWarnings(output<-predict(model4,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  else if(i==5){
    suppressWarnings(output<-predict(model5,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  else if(i==6){
    suppressWarnings(output<-predict(model6,type="response",newdata=test2018[k,]))
    odds <- c(odds, output)
  }
  k<-k+1
}
rankorder<-order(-odds)
odds<-sort(odds,decreasing = TRUE)
rankings<-test2018[rankorder,]
rankings$predicted_rank<-seq(1,211,1)
rankings$odds<-odds

##plotting the predicted vs actual rank graph
ggplot(rankings,aes(x=predicted_rank,y=rank))+geom_point()+geom_abline()

write.csv(rankings,"predicted_rankings.csv",row.names = FALSE)


