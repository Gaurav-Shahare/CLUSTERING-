setwd("C:/Users/Hp/Desktop/BISCUITS 2/NEW FILES")

data1=read.csv("FINAL_FLAT_FILE .csv",stringsAsFactors=T)

names(data1)[names(data1)== "X0"]="Qty_COOKIES_Mar"
x1=data1[1]
s=c(3,6,10,13,20,40,41,43,44,49,51,55,57,73)
x2=data1[s]
FLAG=data1[84]
names(data1)[names(data1)== "X0"]="Qty_COOKIES_Mar"
names(data1)
x2=as.data.frame(scale(x2))
raw=x2

#dummies
#library(dummies)
#x3<- dummy.data.frame(x3)
#raw=cbind(x2,x3)

#PRINCIPLE COMPONENT ANALYSIS 
prin_comp <- prcomp(raw,scale = F)
#library("factoextra")
fviz_pca_contrib(prin_comp, choice = "var", axes = 1:2)
#CALCULATION OF VARIENCE 
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prin_comp$rotation[,1]
prin_comp$center
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#FIRST 7 PCA'S CONTRIBUTE TO APPROX 99 VARIANCE OF DATA SO CHOOSE THEM

train.data <- data.frame(SE_HUL_CODE =data1$SE_HUL_CODE, prin_comp$x)

train.data=train.data[1:8]
train.data=cbind(train.data,FLAG)
#boxplot(train.data$PC3,horizontal = T)
rm(FLAG)
#kmeans algorithm best value of k using elbow method 


#install.packages("NbClust")

library(NbClust)
nc <- NbClust(train.data[2:8], min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}





##forward selection kmean


opt=array()
#forward selection 
minimum=function(data)
  
{
  
  min_k=array()
  
  for(i in 2:15)
  {
    
    min=kmeans(data,i,nstart = 25)
    min_k[i-1]=min$withinss
    
  }
  abrupt=array()
  for(i in 1:13)
  {
    abrupt=min_k[i]-min_k[i+1]
  }
  
  return(which.max(abrupt)+2)
  
  
  
}

###############
no_var=8
for(i in 2:8)
{
  a=minimum(train.data[2:i])
  fit=kmeans(train.data[2:i],a,nstart = 25)
  
  opt[i-1]=fit$tot.withinss
}

#we are selectin
plot(2:no_var, opt, type="b", xlab="VARIABLE CHOOSEN FROM START",ylab="total groups sum of squares")


#w=minimum(train.data[2:21])
set.seed(42)
clu=kmeans(train.data[2:8],3,nstart = 25)
#clu=kmeans(train.data[2:51],w,nstart = 25)

clu$cluster=as.factor(clu$cluster)
library(ggplot2)

#ggplot(train.data,aes(PC1,PC2,color=clu$cluster))+geom_point()

#ggplot of the whole data distinguishing the test and control stores 
ggplot(train.data,aes(PC1,PC2,shape=as.factor(FLAG),color=clu$cluster,size=as.factor(FLAG)))+geom_point()

table(clu$cluster)



#hence cluster 2looks like a outlier hence removing the cluster 3 

train.data=subset(train.data,!clu$cluster==2)
clu=kmeans(train.data[2:8],3,nstart = 25)

ggplot(train.data,aes(PC1,PC2,shape=as.factor(FLAG),color=as.factor(clu$cluster),size=as.factor(FLAG)))+geom_point()

#finding value of k 

table(clu$cluster)



#there distribution
table(train.data$FLAG,clu$cluster)
clu$cluster==as.factor(clu$cluster)
ggplot(train.data,aes(PC1,PC2,shape=as.factor(FLAG),color=as.factor(clu$cluster),size=as.factor(FLAG)))+geom_point()



train.data$Cluster=clu$cluster  #adding new variable to dataframe

#write.csv(train.data,file = "train.csv",row.names = F)
z=subset(train.data,train.data$Cluster==1)
m=subset(train.data,train.data$Cluster==2)
k=subset(train.data,train.data$Cluster==3)


ggplot(z,aes(PC1,PC2,shape=as.factor(FLAG),size=as.factor(FLAG),color=as.factor(FLAG)))+geom_point()
#change the index plot will change

# Near Function
# Near Function
Near_control=function(z)  
{
  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  a2=which(z$FLAG=="YES")
  a3=which(z$FLAG=="NO")
  z2=z[a3,] #control store 
  z3=z[a2,] #test stores 
  a1=length(a2)
  
  
  
  for(i in 1:a1)
  { 
    a=array()
    for(ii in 1:nrow(z2))
      
    {
      a[ii]=euc.dist(as.vector(z3[i,2:8]),as.vector(z2[ii,2:8]))
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

#calling the function


#one =Near_control(z)
#two=Near_control(m)
#three=Near_control(k)





#euc.dist(as.vector(z[a2[1],2:21]),as.vector(z[a3[1],2:21]))
#train.data$CLUSTER=clu$cluster
#write.csv(train.data,file = "withcluster.csv",row.names = F)
##
#best fit model selection method 



#DBSCAN ALGORITHMS 
install.packages("dbscan")
library(dbscan)
kNNdistplot(train.data[2:51], k = 52)
abline(h=.5, col = "red", lty=2)
dbclust=dbscan(train.data[2:51],eps=0.5,minPts =5)

dbclust$cluster <- as.factor(dbclust$cluster)

library(ggplot2)
ggplot(train.data, aes(x = PC1, y = PC2,color=dbclust$cluster)) +geom_point()

table(dbclust$cluster)
ggplot(train.data,aes(PC1,PC2,color=as.factor(train.data$FLAG)))+geom_point()
