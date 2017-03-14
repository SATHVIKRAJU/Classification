
library(textir) 
library(class)
library(dplyr)
library(recommenderlab)

#Read the CSV file from the directory 

creditdata <- read.csv("C:/R_2017/germancredit.csv")
creditdata$Default <- factor(credit$Default)

#The credit data has to be factored and and renamed in different levels as shown in the figure 

creditdata$history = factor(creditdata$history,levels=c("A30","A31","A32","A33","A34"))
levels(creditdata$history) = c("good","good","poor","poor","terrible")
creditdata$foreign <- factor(creditdata$foreign, levels=c("A201","A202"),labels=c("foreign","german"))
creditdata$rent <- factor(creditdata$housing=="A151")
creditdata$purpose <- factor(creditdata$purpose,
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(creditdata$purpose) <-c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

creditdata <- creditdata[,c("Default","duration","amount","installment","age",
                    "history", "purpose","foreign","rent")]
creditdata[1:3,]

#In order to standardize the data across all the three loan charactersitics we use the normaloze function.

x <- normalize(creditdata[,c(2,3,4)])
x[1:3,]

#Split the data into training and testing data. 900 training and 100 testing.
set.seed(1)
train <- sample(1:1000,900) 
xtrain <- x[train,]
xnew <- x[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]

# k-nearest neighbor method
library(class)
nearest1 <- knn(train=xtrain, test=xnew, cl=ytrain, k=1)
nearest3 <- knn(train=xtrain, test=xnew, cl=ytrain, k=3)
data.frame(ynew,nearest1,nearest3)[1:10,]
37

#Calculate the correct classifications.
cor_class1=100*sum(ynew==nearest1)/100
cor_class3=100*sum(ynew==nearest3)/100
cor_class1
cor_class3

#Plot of 3 nearest neighbours
plot(xtrain[,c("amount","duration")],col=c(4,3,6,2)[credit[train,"installment
                                                           "]],pch=c(1,2)[as.numeric(ytrain)],main="Predicted default, by 3 nearest
     neighbors",cex.main=.95)
points(xnew[,c("amount","duration")],bg=c(4,3,6,2)[credit[train,"installment"
                                                          ]],pch=c(21,24)[as.numeric(nearest3)],cex=1.2,col=grey(.7))
legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred
                                                             0","data 1","pred 1"),title="default",bty="n",cex=.8)
legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment
       %",horiz=TRUE,bty="n",col=grey(.7),cex=.8)

#Performing leave one out cross validation
cor_class=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,cl=creditdata$Default,k)
  cor_class[k]=100*sum(creditdata$Default==pred)/1000
}

cor_class
Avg_cor_class=sum(cor_class)/length(cor_class)
Avg_cor_class

