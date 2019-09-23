

setwd("F:\\data\\Data")

library(tidyverse)

m.train <- read.csv('product_train.csv', stringsAsFactors = FALSE)

m.test <- read.csv('product_test.csv', stringsAsFactors = FALSE)
m.test$went_on_backorder <- NA

m.train$IsTrainSet <- TRUE
m.test$IsTrainSet <- FALSE

m.full <- rbind(m.train, m.test)




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
library(dplyr)
glimpse(m.full)

table(m.full$national_inv)
table(m.full$potential_issue)
table(m.full$lead_time)
table(m.full$in_transit_qty)
m.full <- CreateDummies(m.full, 'lead_time', 2000)

m.full$potential_issue <- as.numeric(m.full$potential_issue == 'Yes')#need to drop


# data prep for perf_6_month_avg ------------------------------------------


b = sort(table(m.full$perf_6_month_avg), decreasing = T)
b


b = data.frame(b)
colnames(b)




b = b %>% mutate(Freq = ifelse(Freq < 940, "other", Freq))
b$Var1[which(b$Freq == "other")] = "other"



b = b[-c(54:101), ]

w = b$Var10


m.full$perf_6_month_avg <- as.factor(ifelse(m.full$perf_6_month_avg %in% w, m.full$perf_6_month_avg, "other"))


# data prep ends ----------------------------------------------------------



# perf_12_month_avg -------------------------------------------------------


b = sort(table(m.full$perf_12_month_avg), decreasing = T)
View(b)


b = data.frame(b)
colnames(b)



b = b %>% mutate(Freq = ifelse(Freq < 845, "other", Freq))
b$Var1[which(b$Freq == "other")] = "other"



b = b[-c(54:101), ]

w = b$Var1

m.full$perf_12_month_avg <- as.factor(ifelse(m.full$perf_12_month_avg %in% w, m.full$perf_12_month_avg, "other"))


# data prep ends ----------------------------------------------------------
table(m.full$local_bo_qty)

m.full <- CreateDummies(m.full, 'local_bo_qty', 160)


m.full$deck_risk <- as.numeric(m.full$deck_risk == 'Yes')

m.full$oe_constraint <- as.numeric(m.full$oe_constraint == 'Yes') #need to delete


m.full$ppap_risk <- as.numeric(m.full$ppap_risk == 'Yes') 


m.full$stop_auto_buy <- as.numeric(m.full$stop_auto_buy == 'Yes') 

m.full$rev_stop <- as.numeric(m.full$rev_stop == 'Yes') #need to delete

m.full$went_on_backorder <- as.numeric(m.full$went_on_backorder == 'Yes') 


glimpse(m.full)

sort(table(m.full$went_on_backorder), decreasing = T)

sum(m.full$went_on_backorder == "", na.rm = T)
sum(is.na(m.full$went_on_backorder))


# data splitting ----------------------------------------------------------


m.full <- m.full %>%
  select(-rev_stop)


m.full <- m.full %>%
  select(-oe_constraint)

m.full <- m.full %>%
  select(-sku)

m.full <- m.full %>%
  select(-potential_issue)


train = m.full[m.full$IsTrainSet == TRUE, ]

test = m.full[m.full$IsTrainSet == FALSE, ]



train <- train %>%
  select(-IsTrainSet)


glimpse(train)




test <- test %>%
  select(-went_on_backorder)


test <- test %>%
  select(-IsTrainSet)


glimpse(test)



# GBM model start ---------------------------------------------------------


library(gbm)
library(cvTools)

param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))



subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}



num_trials=10
my_params=subset_paras(param,num_trials)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

myauc=0



# parameter tuning --------------------------------------------------------


for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(gbm, went_on_backorder~.,
             data =train,
             tuning =params,
             args=list(distribution="bernoulli"),
             folds = cvFolds(nrow(train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="response",n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  if(score.this>myauc){
    # print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  print('DONE')
  # uncomment the line above to keep track of progress
}


myauc

best_params

ci.gbm.final=gbm(went_on_backorder~.,data=train,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsinnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "bernoulli")
# just checking

ci.gbm.final=gbm(went_on_backorder~.,data=train,
                 n.trees = 500,
                 n.minobsinnode = 1,
                 shrinkage = 0.1,
                 interaction.depth = 3,
                 distribution = "bernoulli")



train$score<- predict(ci.gbm.final,newdata=train,type ='response', n.trees = best_params$n.trees)
train$score<- predict(ci.gbm.final,newdata=train,type ='response', n.trees = 500)


ggplot(train,aes(x=score,y=went_on_backorder,color=factor(went_on_backorder)))+geom_point()+geom_jitter()


cutoff=0.2
predicted=as.numeric(train$score>cutoff)
TP=sum(predicted==1 & train$went_on_backorder==1)
FP=sum(predicted==1 & train$went_on_backorder==10)
FN=sum(predicted==0 & train$went_on_backorder==1)
TN=sum(predicted==0 & train$went_on_backorder==0)


P=TP+FN
N=TN+FP

total=P+N


cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)


for (cutoff in cutoffs){
  predicted=as.numeric(train$score>cutoff)
  TP=sum(predicted==1 & train$went_on_backorder==1)
  FP=sum(predicted==1 & train$went_on_backorder==0)
  FN=sum(predicted==0 & train$went_on_backorder==1)
  TN=sum(predicted==0 & train$went_on_backorder==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  select(-P,-N)


library(tidyr)

cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift) %>%
  gather(Criterion,Value,Sn:Lift)


ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()


cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()

library(dplyr)
view(cutoff_data)
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]


KS_cutoff
KS = max(cutoff_data$KS)

1-(0.025/KS)

test.score<- predict(ci.gbm.final,newdata=test,type ='response', n.trees = best_params$n.trees)
test.score<- predict(ci.gbm.final,newdata=test,type ='response', n.trees = 500)



#went_on_backorder <- as.numeric(test.score>KS_cutoff)

#output.df <- as.data.frame(went_on_backorder)

#write.csv(output.df, file="SHARIQUE_MANSOOR_MANUFACTURING_part2.csv", row.names = F)




test.predicted=as.numeric(test.score>KS_cutoff)



test.predicted=ifelse(test.predicted==1,'Yes','No')

write.csv(test.predicted, file="SHARIQUE_MANSOOR_P3_part2.csv", row.names = F)

  