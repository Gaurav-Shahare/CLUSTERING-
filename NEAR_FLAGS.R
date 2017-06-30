setwd("C:/Users/Hp/Desktop/BISCUITS 2/NEW FILES")

data1=read.csv("FINAL_FLAT_FILE .csv",stringsAsFactors=T)

x1=data1[1]
s=c(3,6,10,13,20,40,41,43,44,49,51,55,57,73)
x2=data1[s]
FLAG=data1[84]

#names(data1)

x2=as.data.frame(scale(x2))
raw=x2
#PRINCIPLE COMPONENT ANALYSIS 
prin_comp <- prcomp(raw,scale = F)
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

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
yes=subset(train.data,train.data$FLAG=="YES")
no=subset(train.data,train.data$FLAG=="NO")


for(i in 1:nrow(yes))
{  a=array()
  for(j in 1:nrow(no))
  {
    a[j]=euc.dist(yes[i,2:8],no[j,2:8])
  }
    a=order(a)
    a=no$SE_HUL_CODE[a]
    if(i==1)
    {
      near=data.frame(name =a)
    }
    else
    {
      near=cbind(near,data.frame(name =a))
    }

}

for(i in 1:nrow(yes))
{
  colnames(near)[i] <- paste("FLAG",yes[i,1])
}

write.csv(near,file="NEAR_FLAGS.csv",row.names = F)


