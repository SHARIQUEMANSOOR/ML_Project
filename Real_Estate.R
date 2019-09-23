setwd("F:\\data\\Data")

library(tidyverse)
train1= read.csv('housing_train.csv', stringsAsFactors = FALSE)
View(train1)

test1 = read.csv('housing_test.csv', stringsAsFactors = FALSE)
test1$Price <- NA

train1$Istrain1Set = TRUE
test1$Istrain1Set = FALSE

all1=rbind(train1,test1)

View(all1)
library(dplyr)
glimpse(all1)
sort(table(all1$Suburb),decreasing = T)

all1=CreateDummies(all1,'Suburb',100)

unique(all1$Suburb)
unique(all1$Address)
table(all1$Type)



CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

library(tidyr)

all1=all1 %>% 
  select(-Suburb)

all1=all1 %>% select(-Address)

View(all1)

all1=CreateDummies(all1,'Type')

table(all1$Method)
all1=CreateDummies(all1,'Method')

sort(table(all1$Postcode),decreasing = T)


all1=CreateDummies(all1,'Postcode',150)


sort(table(all1$SellerG),decreasing = T)
all1=all1 %>% select(-SellerG)
all1=CreateDummies(all1,'SellerG',150)
glimpse(all1)

sort(table(all1$CouncilArea),decreasing = T)


all1=CreateDummies(all1,'CouncilArea',400)


glimpse(all1)



for(col in names(all1)){
  if(sum(is.na(all1[,col]))>0 & !(col %in% c("Istrain1Set","Price"))){
    all1[is.na(all1[,col]),col]=mean(all1[all1$Istrain1Set=='TRUE',col],na.rm=T)
  }
}

View(all1)





lapply(all1, function(x) sum(is.na(x)))

ld_train1=all1 %>% filter(Istrain1Set=='TRUE') %>% select(-Istrain1Set)
ld_test1=all1 %>% filter(Istrain1Set=='FALSE') %>% select(-Istrain1Set,-Price)

ld_train1$random=round(runif(7536),2)

param=list(mtry=c(260),
           ntree=c(50,100,200,500),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


subset_paras=function(full_list_para,n=10){
  all1_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all1_comb),n)
  subset_para=all1_comb[s,]
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)

library(randomForest)

library(cvTools)
myerror=9999999


for(i in 1:num_trials){
   print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(randomForest,Price~.,
             data =ld_train1,
             tuning =params,
             folds = cvFolds(nrow(ld_train1), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  if(score.this<myerror){
    
     print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
     print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
   print('DONE')
  # uncomment the line above to keep track of progress
}
library(randomForest)

ld.rf.final=randomForest(Price~.,
                         mtry=60,
                         ntree=500,
                         maxnodes=20,
                         nodesize=5,
                         data=ld_train1)


d=importance(ld.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(IncNodePurity))



test1.pred=predict(ld.rf.model,newdata = ld_test1)
write.csv(test1.pred,"mysubmission.csv",row.names = F)