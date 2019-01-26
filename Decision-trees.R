#Computing id: nb7ug
#Name: Niharika Reddy

library(datasets)
library(tidyverse)
library(ggplot2)


# Defining classification probability function
classify<-function(x){
  versicolor.count<-nrow(x %>% filter(Species=='versicolor'))
  versicolor.prob<-versicolor.count/nrow(x)
  return (versicolor.prob)
}


# Defining purity of region function
purity<-function(x){
  if(length(unique(x$Species))==1){
    return ("True")}
else{
  return("False")
}
}



#Defining function to get potential splits

potential.splits<-function(x){
  potential_split_ls <- list()
  for(col in 1:(ncol(x)-1) ) {
    levels<-(max(x[,col])-min(x[,col]))/10
    counter =1
    potential_split_ls[[col]] <- numeric(0)
    while((min(x[,col])+levels*counter)<(max(x[,col]))){
      split = min(x[,col])+levels*counter
      potential_split_ls[[col]] <- c(potential_split_ls[[col]],split)
      counter = counter+1
    }
  }
  return(potential_split_ls)
}


# Defining the function to split into 2 regions and give indices of those regions
region<-function(x,col,split){
  R1<-which(x[,col]<split)
  R2<-which(x[,col]>=split)
  return(list(R1,R2))
}

#Defining function for weighted gini index
gini<-function(df,x){
  r1<-df[x[[1]],]
  r2<-df[x[[2]],]
  gini<-(nrow(r1)/((nrow(r1)+nrow(r2)))*((classify(r1)^2)+(1-classify(r1))^2))+(nrow(r2)/((nrow(r1)+nrow(r2)))*((classify(r2)^2)+(1-classify(r2))^2))
  return(gini)
}


# Defining the function to get the best split
best_split<-function(df,pot.split){
  overall.gini=0
  for(i in 1:length(pot.split)){
    for(x in pot.split[[i]]){
      r=region(df, i, x)
      current.gini<-gini(df,r)
      if(current.gini>overall.gini){
        overall.gini<-current.gini
        best_split_col<-i
        best_split_val<-x
        }
    }
    
  }
  return(list(best_split_col, best_split_val))
}


#Defining the function to fit decision tree

fit.decision.tree<-function(df,m){
  if((purity(df)!="True")&(nrow(df)>m))# Setting the stopping criteria: Stop if node is pure or no. of obersvations<=x
 {
    potential_splits<-potential.splits(df)
    best.split<-best_split(df,potential_splits)
    reg<-region(df, best.split[[1]],best.split[[2]])
    true_ans <- fit.decision.tree(df[reg[[1]],],m)
    false_ans<-fit.decision.tree(df[reg[[2]],],m)
    tree<-list(split_col=best.split[[1]],split_val = best.split[[2]], left.subtree=true_ans, right.subtree=false_ans)
  }
  else{
    
    return(list(data=df, probability = classify(df)))
  }
  return(tree)
}

#Creating the prediction function
pred<-function(x,y){
tree.test<-y
prediction<-c()
for( i in 1: nrow(x)){
  while(is.null(y$data)){
    if(x[i,][y$split_col]<y$split_val){
      y<-y$left.subtree
    }
    else{
      y<-y$right.subtree
    }
  }
  prediction<- c(prediction,y$probability)
  y<-tree.test
}
return(prediction)
}


#Subsetting the dataset to exclude setoisa
iris.df<- filter(iris, Species!="setosa")
head(iris.df)


for(p in 1:6){
  # Shuffling the dataset and creating the training and testing dataset

  set.seed(p+100)
  train<-sample(nrow(iris.df), 2/3*nrow(iris.df))
  iris.train<-iris.df[train,]
  iris.test<-iris.df[-train,]
  
  #Fitting the decision tree
  tree<-fit.decision.tree(iris.train,p+1)#Fitting the tree with different stopping criteria
  

  
  #Predicting the probabilities in the test dataset
  prediction<-pred(iris.test, tree)
  
  iris.test$Species<- ifelse(iris.test$Species=="versicolor", 1,0)
  
  #ROC
  
  iris.test.prediction<-cbind(iris.test, prediction)
  
  iris.test.prediction$Classification<-NA
  unique<-c(unique(iris.test.prediction$prediction),-1)
  TPR_FPR<-data.frame(matrix(NA,nrow=length(unique), ncol = 3))
  colnames(TPR_FPR)<-c("Threshold","TPR", "FPR")
  counter=1
  
  for (t in unique){
    t_count<-0
    f_count<-0
    for(i in 1:nrow(iris.test.prediction)){
      iris.test.prediction$Classification[i]<-ifelse(iris.test.prediction$prediction[i]>t,1,0)
      
      if((iris.test.prediction$Classification[i]==1)&(iris.test.prediction$Species[i]==1)){
        t_count = t_count+1   
      }
      if((iris.test.prediction$Classification[i]==1)&(iris.test.prediction$Species[i]==0)){
        f_count = f_count+1   
      }
    }
    TPR_FPR$Threshold[counter]<-t
    TPR_FPR$TPR[counter]<-t_count
    TPR_FPR$FPR[counter]<-f_count
    counter =  counter+1
  }
  
  #calculation of true positive and true negative rate sfor each threshold
  TPR_FPR$TPR <- TPR_FPR$TPR/sum(iris.test.prediction$Species==1)
  TPR_FPR$FPR <- TPR_FPR$FPR/sum(iris.test.prediction$Species==0)
  
  ROC_plot<-ggplot(data = arrange(TPR_FPR,desc(Threshold)),
         aes(x=FPR, y= TPR))+geom_point() + geom_line() + ylim(c(0,1)) + xlim(c(0,1))+ggtitle(paste("ROC",p))
  #print tree for each iteration
  print(tree)
  #Print ROC plot for each iteration
  print(ROC_plot)
  
}








