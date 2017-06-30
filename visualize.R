setwd("C:/Users/Hp/Desktop/BISCUITS 2/NEW FILES/near_distance_visualise")
dir()
nr=read.csv("pca_NEAR.csv")
library(ggplot2)
ggplot(nr,aes(PC1,PC2,))
ggplot(nr,aes(PC1,PC2,shape=as.factor(FLAG),color=as.factor(ORDER)))+geom_point()

##NEAR FLAG VISUALIZATION

data1=read.csv("pca.csv")
data2=read.csv("HUNDREAD.csv")

a=data2[,1]
data3=data1[match(a,data1$SE_HUL_CODE),]
write.csv(data3,file="hundered.csv",row.names = F)

data4=read.csv("VISUALIZE_FINAL.csv")
ggplot(data4,aes(PC1,PC2,shape=as.factor(FLAG),color=as.factor(STORE_TYPE),size=as.factor(FLAG)))+geom_point()
which.min(data4$PC1)
data4=data4[-62,]
