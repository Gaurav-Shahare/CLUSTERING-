setwd("C:/Users/Hp/Desktop/BISCUITS 2")

data1=read.csv("sku_flatfile_updated.csv",stringsAsFactors=T)

#initial outliers are not removed 
data1=subset(data1,!data1$Revenue_Biscuit_Feb==0.0)
data1=subset(data1,!data1$Revenue_Biscuit_Mar==0.0)
colnames(data1)

z=c(2,3,7,8,9,10,14,15,16,17,18,19,20,24,28,30,31,33,34,35,37,38,40,41,43,44,46,47,50,51,52,53,56,57,70,71)
#only cookies revnue mom magic cashew and almomnds 200 gms
x1=data1[1]
x2=data1[z]
x3=data1[72:79]
FLAG=data1[80]
x2=scale(x2)


#dummies 

library(dummies)
x3<- dummy.data.frame(x3)
#x3=scale(x3) there is no need to scale dummies 
raw=cbind(x2,x3)

#PRINCIPLE COMPONENT ANALYSIS 
prin_comp <- prcomp(raw)
#CALCULATION OF VARIENCE 
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#select first 40 variables 
train.data <- data.frame(SE_HUL_CODE =data1$SE_HUL_CODE, prin_comp$x)
train.data=train.data[1:41]
train.data=cbind(train.data,FLAG)
rm(FLAG)
library(NbClust)
nc <- NbClust(train.data[2:41], min.nc=2, max.nc=15, method="kmeans")
#table(nc$Best.n[1,])
set.seed(42)
clu <- kmeans(train.data[2:41], 8, nstart=25)

clu$cluster=as.factor(clu$cluster)
library(ggplot2)
ggplot(train.data,aes(PC1,PC2,shape=as.factor(FLAG),color=clu$cluster,size=as.factor(FLAG)))+geom_point()
pairs(train.data[2:10])
plot(train.data$PC1,train.data$PC2)
table(clu$cluster)

#since cluster 4 and 6  contain 3,1 data points respectivly they must outliers
#removed the oulier lying in cluster no 4 :p  ITS A CONTROL STORE :D
train.data=train.data=subset(train.data,!clu$cluster==4)

nc <- NbClust(train.data[2:41], min.nc=2, max.nc=15, method="kmeans")


#best cluster in nbclust  is at 3
set.seed(42)

clu <- kmeans(train.data[2:41], 3, nstart=25)

clu$cluster=as.factor(clu$cluster)

ggplot(train.data,aes(PC1,PC2,shape=as.factor(FLAG),color=clu$cluster,size=as.factor(FLAG)))+geom_point()
table(clu$cluster)

#N0 YES
#1 257  19
#2  63  22
#3  54  10


train.data$Cluster=clu$cluster
z=subset(train.data,train.data$Cluster==1)
m=subset(train.data,train.data$Cluster==2)
k=subset(train.data,train.data$Cluster==3)

#ggplot(z,aes(PC1,PC2,shape=as.factor(FLAG),size=as.factor(FLAG),color=as.factor(FLAG)))+geom_point()


#function returning the nearest control stores to test stores :D

Near_control=function(z)  
{
  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  a2=which(z$FLAG=="YES")
  a3=which(z$FLAG=="N0")
  z2=z[a3,1:21] #control store 
  z3=z[a2,1:21] #test stores 
  a1=length(a2)
  
  
  
  for(i in 1:a1)
  { 
    a=array()
    for(ii in 1:nrow(z2))
      
    {
      a[ii]=euc.dist(as.vector(z3[i,2:21]),as.vector(z2[ii,2:21]))
    }
    
    a=order(a)
    a=z2$SE_HUL_CODE[a]
    
    if(i==1)
    {
      #name=paste("TEST STORE N0",i)
      near=data.frame(name =a)
    }
    
    else
    {
      #name=paste("TEST STORE N0",i)
      near=cbind(near,data.frame(name =a))
    }
    
  }
  for(i in 1:a1)
  {
    colnames(near)[i] <- paste("Test store no",i)
  }
  
  return(near)
}

#one =Near_control(z)
#two=Near_control(m)
#three=Near_control(k)


