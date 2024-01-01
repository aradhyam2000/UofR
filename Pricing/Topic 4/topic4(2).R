
#Load packages
library("mlogit")
library("gmnl")
library("data.table")

library("Rfast") #Package for "rowMaxs" function

rm(list = ls());
#Change folder name to yours
setwd("YOUR FOLDER NAME")
#setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2023 Spring A pricing analytics/Topic 4")


#This file contains codes for both topic 4 and topic A4


##
rm(list = ls());
set.seed(0)
# estimate multinomial logit segment-by-segment, where segmentation is defined by demographics
#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


#Estimate single segment logit as a point of comparison
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")
mle_noseg= gmnl(choice ~  price, data = mlogitdata)
summary(mle_noseg)

coef_noseg=mle_noseg$coefficients




#Load demographic data
demo=fread("demo.csv",stringsAsFactors = F)

#Number of individuals
N = 100
#Clustering
demo_cluster = kmeans(x=demo[, 2:9], centers = 5, nstart = 1000)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)

# for those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = 6

# segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N	

# just store the coefficients (you can store many other things)
coef.est = data.frame(segment = 1:6, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

#Write a for-loop. 
for (seg in 1:6) {
  # During each loop, pick subset of data of consumers from each segment.
  data.sub = subset(data, cluster == seg)
  
  #Using that data, the rest remains the same.
  mlogitdata=mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
  
  #Run MLE.
  mle= gmnl(choice ~  price, data = mlogitdata)
  mle
  #Store the outcome in the coef.est matrix.
  coef.est[seg, 2:5] = mle$coefficients
}




#Plot results
demand=function(priceKB,priceKR,priceMB,para){
  prob=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob)
}
pricespace=seq(0.5,1.8,0.01)
plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="blue",lwd=20*seg.share[2])


#Biggest segment
plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="blue",lwd=20*seg.share[2])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="orange",lwd=20*seg.share[3])

#Least elastic segment
plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="orange",lwd=20*seg.share[2])





plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[2],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),col="orange",lwd=20*seg.share[1])


plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="orange",lwd=20*seg.share[2])



plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="blue",lwd=20*seg.share[2])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="blue",lwd=20*seg.share[6])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="orange",lwd=20*seg.share[4])

plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[1],
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="blue",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5])),col="blue",lwd=20*seg.share[5])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5])),col="blue",lwd=20*seg.share[2])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5])),col="orange",lwd=20*seg.share[6])


#Response to price change - segment 1
pricespace=seq(0,2,0.01)
plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5])),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=20*seg.share[3]
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5])),col="orange",lwd=20*seg.share[4])
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB)+1,as.numeric(coef.est[3,2:5])),col="blue",lwd=20*seg.share[3],lty=2)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB)+1,as.numeric(coef.est[4,2:5])),col="orange",lwd=20*seg.share[4],lty=2)



#Scatterplot of parameters - beta_0^{KB}-beta_0^{KR} against beta_1.
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,5],cex=20*seg.share[1],xlim=c(-3,3),ylim=c(-9,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^KR",ylab=("beta_1"))
points(coef.est[2,2]-coef.est[2,3],coef.est[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef.est[3,2]-coef.est[3,3],coef.est[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef.est[4,2]-coef.est[4,3],coef.est[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)
points(coef.est[5,2]-coef.est[5,3],coef.est[5,5],cex=20*seg.share[5],col = "chocolate",pch=16)
points(coef.est[6,2]-coef.est[6,3],coef.est[6,5],cex=20*seg.share[6],col = "chocolate",pch=16)


#Calculate profit
agg_choice=function(priceKB,priceKR,priceMB) {
  
  agg_choice=seg.share[1]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))+ 
    seg.share[6]*demand(priceKB,priceKR,priceMB,as.numeric(coef.est[6,2:5]))
  
  return(agg_choice)
  }


#Aggregate choice probabilities
plot(pricespace,agg_choice(pricespace,mean(data$price.KR),mean(data$price.MB)),type='l',xlab='Prices',
     ylab='Choice probability',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),coef_noseg),col="orange",lwd=20*seg.share[4],lty=2)
legend(1.1, 0.9, legend=c("With segmentation", "Without segmentation"),
       col=c( "blue","orange"), lty=1, cex=1.1)

aggchoicemat_cluster=agg_choice(pricespace,mean(data$price.KR),mean(data$price.MB))

#Profit
uc=1
profit=1000*agg_choice(pricespace,mean(data$price.KR),mean(data$price.MB))*(pricespace-uc)
profit_noseg=1000*demand(pricespace,mean(data$price.KR),mean(data$price.MB),coef_noseg)*(pricespace-uc)
plot(pricespace,profit,
     type='l',xlab='Prices',ylab='Profit',col="blue",lwd=2,xlim=c(1,2),ylim=c(0,100)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit_noseg,col="orange",lwd=20*seg.share[4],lty=2)
legend(1.5, 100, legend=c("With segmentation", "Without segmentation"),
       col=c( "blue","orange"), lty=1, cex=1.1)
pricespace[profit==max(profit)]
pricespace[profit_noseg==max(profit_noseg)]
#Maximum profit 91.05 if we set p=1.28. 6% increase compared to 86.56 under no segmentation.

profit_cluster=profit


#Market segmentation and targeted pricing
#Profit from each segment
profit1=1000*seg.share[1]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[1,2:5]))*(pricespace-uc)
profit2=1000*seg.share[2]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[2,2:5]))*(pricespace-uc)
profit3=1000*seg.share[3]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[3,2:5]))*(pricespace-uc)
profit4=1000*seg.share[4]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[4,2:5]))*(pricespace-uc)
profit5=1000*seg.share[5]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[5,2:5]))*(pricespace-uc)
profit6=1000*seg.share[6]*demand(pricespace,mean(data$price.KR),mean(data$price.MB),as.numeric(coef.est[6,2:5]))*(pricespace-uc)



plot(pricespace,profit1,
     type='l',xlab='Prices',ylab='Profit per segment',col="blue",lwd=2,xlim=c(1,2),ylim=c(1.5,40)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit2,col="blue",lwd=2)
lines(pricespace,profit3,col="blue",lwd=2)
lines(pricespace,profit4,col="blue",lwd=2)
lines(pricespace,profit5,col="blue",lwd=2)
lines(pricespace,profit6,col="blue",lwd=2)

plot(pricespace,profit1,
     type='l',xlab='Prices',ylab='Profit per segment',col="blue",lwd=2,xlim=c(1,2),ylim=c(1.5,40)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit2,col="orange",lwd=2)
lines(pricespace,profit3,col="orange",lwd=2)
lines(pricespace,profit4,col="orange",lwd=2)
lines(pricespace,profit5,col="blue",lwd=2)
lines(pricespace,profit6,col="orange",lwd=2)



#Profit max prices
#Seg 1 
pricespace[profit1==max(profit1)]
pricespace[profit2==max(profit2)]
pricespace[profit3==max(profit3)]
pricespace[profit4==max(profit4)]
pricespace[profit5==max(profit5)]
pricespace[profit6==max(profit6)]


#What if we set 1.30 as a regular price and offer a coupon of 10 cents to segments 1 and 5?
profitseg=profit1[pricespace==1.2]+profit2[pricespace==1.5]+profit3[pricespace==1.3]+
  profit4[pricespace==1.3]+profit5[pricespace==1.2]+profit6[pricespace==1.3]
#Profit=92.767





## 
rm(list=setdiff(ls(), c("profit_cluster","aggchoicemat_cluster")))
set.seed(0)
# estimate multinomial logit with segments included in the model
#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)

#Load demographic data
demo=fread("demo.csv",stringsAsFactors = F)
#Merge with original data
data=merge(data,demo,by="id",all=T)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


#Estimation data = those with demographic info
data.est=data[data$fam_size!="NA",]
mlogitdata=mlogit.data(data.est,id="id",varying=4:7,choice="choice",shape="wide")

#No demographics - benchmark
mle0=gmnl(choice~price,data=mlogitdata)
summary(mle0)
BIC0=log(length(data.est$id))*length(mle0$coefficients)-2*mle0$logLik$maximum[1]
BIC0
#BIC(mle0)

#Run mlogit-gmnl
mle1=gmnl(choice~price|
        fam_size+fem_age+fem_educ+fem_smoke+male_age+male_educ+male_smoke+dogs,
                                                                data=mlogitdata)
summary(mle1)
BIC1=log(length(data.est$id))*length(mle1$coefficients)-2*mle1$logLik$maximum[1]
BIC1


#Drop male side
mle2=gmnl(choice~price|
           fam_size+fem_age+fem_educ+fem_smoke+male_smoke+dogs,
         data=mlogitdata)
summary(mle2)
BIC2=log(length(data.est$id))*length(mle2$coefficients)-2*mle2$logLik$maximum[1]
BIC2


#Inside log
mle3=gmnl(choice~price|
             log(fam_size)+log(fem_age)+log(fem_educ)+fem_smoke+male_smoke+dogs,
           data=mlogitdata)
summary(mle3)
BIC3=log(length(data.est$id))*length(mle3$coefficients)-2*mle3$logLik$maximum[1]
BIC3


#Fixed effects (slow)
mle5=gmnl(choice~price|
            factor(fem_educ)+fam_size+fem_smoke+dogs,
                                              data=mlogitdata)
summary(mle5)
BIC5=log(length(data.est$id))*length(mle5$coefficients)-2*mle5$logLik$maximum[1]
BIC5




#Demographics in beta_0
mle=gmnl(choice~price|fam_size+fem_age,data=mlogitdata)

#Demographics in beta_1 
mle=gmnl(choice~price+price:fam_size+price:fem_age,data=mlogitdata)

#Demographics in both beta_0 and beta_1
mle=gmnl(choice~price+price:fam_size+price:fem_age|
                                fam_size+fem_age,data=mlogitdata)



summary(mle)
BIC=log(length(data.est$id))*length(mle$coefficients)-2*mle$logLik$maximum[1]
BIC




##
#Report estimated demand and maximize profit

#Inside log (again)
mle=gmnl(choice~price|
             log(fam_size)+log(fem_age)+log(fem_educ)+fem_smoke+male_smoke+dogs,
           data=mlogitdata)
summary(mle)
para=mle$coefficients
#Simulate demand based on log-formulation
demand=function(priceKB,priceKR,priceMB,fam_size,fem_age,fem_educ,fem_smoke,male_smoke,dogs,para){

  #Define beta_0's for each product as a function of consumer characteristics
  beta0KB=para[1]+para[5]*log(fam_size)+para[8]*log(fem_age)+para[11]*log(fem_educ)+para[14]*fem_smoke+
    para[17]*male_smoke+para[20]*dogs
  
  beta0KR=para[2]+para[6]*log(fam_size)+para[9]*log(fem_age)+para[12]*log(fem_educ)+para[15]*fem_smoke+
    para[18]*male_smoke+para[21]*dogs  
  
  beta0MB=para[3]+para[7]*log(fam_size)+para[10]*log(fem_age)+para[13]*log(fem_educ)+para[16]*fem_smoke+
    para[19]*male_smoke+para[22]*dogs  
  
  beta1=para[4]
  
  #Define choice probability of KB
  prob=exp(beta0KB+beta1*priceKB)/
    (1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
  return(prob)
}

#Plot demand for consumers with different demographics
pricespace=seq(0.5,1.8,0.01)
plot(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),2,5,4,1,0,1,para),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),4,2,3,1,0,1,para),col="orange",lwd=2)
lines(pricespace,demand(pricespace,mean(data$price.KR),mean(data$price.MB),2,2,3,0,0,0,para),col="red",lwd=2)


#Calculate aggregate choice probability
#Two ways to calculate consumer distribution in the data

#(1)
#Assuming that all demographic visit the store equally
seg.demo=demo[,c(2:5,8,9)]

#(2)
#Assuming that different consumers with different demographics
#come to store at different propensity, according to data observations
aux=cbind(data$fam_size,data$fem_age,data$fem_educ,data$fem_smoke,data$male_smoke,data$dogs)
#Create a matrix of each segment's demo to plug in to the demand
seg.demo2=aux[is.na(aux[,1])==0,]
#In order to use this weight, please replace "seg.demo" with "seg.demo2" in the following code.



#Calculate aggregate demand - for loop version
agg_choice_loop=function(priceKB,priceKR,priceMB,seg.demo){
  #Create a matrix to store choice probabilities
  aux = data.frame(ID=1:nrow(seg.demo), choiceprob = NA) 
  #For each row of consumer, calculate choice probability and store it
  for (i in 1:nrow(seg.demo)){
        aux[i,2]=demand(priceKB,priceKR,priceMB,seg.demo[i,1],seg.demo[i,2],seg.demo[i,3],
                      seg.demo[i,4],seg.demo[i,5],seg.demo[i,6],para)
  }
  agg_choice=mean(aux[,2])
}

#In fact there's a faster way - called "vectorization". Since the vectorized function
#is beyond the scope of this course level I'd just leave the code here. If interested,
#google "vectorization in R".
agg_choice=function(priceKB,priceKR,priceMB,seg.demo){
  agg_choice=1/nrow(seg.demo)*sum(
    demand(priceKB,priceKR,priceMB,seg.demo[,1],seg.demo[,2],seg.demo[,3],
           seg.demo[,4],seg.demo[,5],seg.demo[,6],para))
  
}

pricespace=seq(0,2,0.01)
aggchoicemat=matrix(0L,length(pricespace),1)
aggchoicemat2=matrix(0L,length(pricespace),1)
for (i in 1:length(pricespace)){
  aggchoicemat[i]=agg_choice(pricespace[i],mean(data$price.KR),mean(data$price.MB),seg.demo) 
  aggchoicemat2[i]=agg_choice(pricespace[i],mean(data$price.KR),mean(data$price.MB),seg.demo2) 
}

#Aggregate choice probabilities
plot(pricespace,aggchoicemat,type='l',xlab='Prices',
     ylab='Choice probability',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,aggchoicemat_cluster,col="orange",lwd=2,lty=2)
legend(1.1, 0.9, legend=c("Regression in logit", "K-mean clustering"),
       col=c( "blue","orange"), lty=1, cex=1.1)

plot(pricespace,aggchoicemat,type='l',xlab='Prices',
     ylab='Choice probability',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,aggchoicemat2,col="red",lwd=2,lty=2)
legend(1.0, 0.9, legend=c("Equal weight", "Weight by visit freq"),
       col=c( "blue","red"), lty=1, cex=1.1)


#Profit
uc=1
profit=1000*aggchoicemat*(pricespace-uc)
plot(pricespace,profit,
     type='l',xlab='Prices',ylab='Profit',col="blue",lwd=2,xlim=c(1,2),ylim=c(0,100)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit_cluster,col="orange",lwd=2,lty=2)
legend(1.5, 100, legend=c("Regression in logit", "K-mean clustering"),
       col=c( "blue","orange"), lty=1, cex=1.1)
pricespace[profit==max(profit)]
#Maximum profit 72.09 if we set p=1.3.

#Profit
uc=1
profit2=1000*aggchoicemat2*(pricespace-uc)
plot(pricespace,profit,
     type='l',xlab='Prices',ylab='Profit',col="blue",lwd=2,xlim=c(1,2),ylim=c(0,100)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit2,col="red",lwd=2,lty=2)
legend(1.5, 100, legend=c("Equal weight", "Weight by visit freq"),
       col=c( "blue","red"), lty=1, cex=1.1)
pricespace[profit2==max(profit2)]
#Maximum profit 77.77 if we set p=1.3.


beta0=function(fam_size,fem_age,fem_educ,fem_smoke,male_smoke,dogs,para){
  
  #Define beta_0's for each product as a function of consumer characteristics
  beta0KB=para[1]+para[5]*log(fam_size)+para[8]*log(fem_age)+para[11]*log(fem_educ)+para[14]*fem_smoke+
    para[17]*male_smoke+para[20]*dogs
  
  beta0KR=para[2]+para[6]*log(fam_size)+para[9]*log(fem_age)+para[12]*log(fem_educ)+para[15]*fem_smoke+
    para[18]*male_smoke+para[21]*dogs  
  
  beta0MB=para[3]+para[7]*log(fam_size)+para[10]*log(fem_age)+para[13]*log(fem_educ)+para[16]*fem_smoke+
    para[19]*male_smoke+para[22]*dogs  
  
  return(cbind(beta0KB,beta0KR,beta0MB))
}



#Willingness to pay for KB 
#If we take into account rival products - calculate surplus
#First, surplus table
beta0mat=beta0(seg.demo[,1],seg.demo[,2],seg.demo[,3],seg.demo[,4],seg.demo[,5],seg.demo[,6],para)

#Maximum surplus if one doesn't purchase KB
maxother=rowMaxs(as.matrix(cbind(beta0mat[,2]+para[4]*mean(data$price.MB),beta0mat[,3]+para[4]*mean(data$price.MB),matrix(0L,nrow(beta0mat),1))),value=TRUE)
WTPKB=(beta0mat[,1]-maxother)/(-para[4])

#If we don't, then just ratio between beta0 and beta1
WTPKB2=(beta0mat[,1])/(-para[4])
WTPKR2=(beta0mat[,2])/(-para[4])
WTPMB2=(beta0mat[,3])/(-para[4])

#Histogram
hist(as.matrix(WTPKB2),
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, breaks=seq(0.2,1.6,length.out=25),
     xlab="Willingness to pay (KB)",ylab=("Density"),main="")

#Histogram
hist(as.matrix(WTPKR2),
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, breaks=seq(0.2,1.6,length.out=25),
     xlab="Willingness to pay (KR)",ylab=("Density"),main="")

#Plot WTPKB against WTPKR
#Downward sloping relationship - KB and KR are substitutes
plot(as.matrix(WTPKB2),as.matrix(WTPKR2),cex=2,xlim=c(0,2),ylim=c(0,2),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay KB",ylab="Willingness to pay KR")
lines(c(-10,8),c(-10,8),)

#Scatterplot of parameters KB against KR
plot(as.matrix(beta0mat[,1]-beta0mat[,2]),as.matrix(beta0mat[,1]-beta0mat[,3]),cex=2,xlim=c(-3.5,6),ylim=c(-3.5,6),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab="beta0KB-beta0MB")
lines(c(-10,8),c(-10,8),)





##
#Illustrative code for lecture slides - not real
# 
# #Recall our "demand" takes each consumer's demographic as an input
# demand=function(priceKB,priceKR,priceMB,fam_size,fem_age,fem_educ,fem_smoke,male_smoke,dogs,para)
# 
#   #Compute choice probability of everyone
#   #For loop version
#   #For each row of consumer, calculate choice probability and store it
#   for (i in 1:nrow(demo)){
#     demandmat[i]=demand(priceKB,priceKR,priceMB,seg.demo[i,1],seg.demo[i,2],
#                         seg.demo[i,3],seg.demo[i,4],seg.demo[i,5],seg.demo[i,6],para)
#   }
# 
# #Vectorized version
# demandmat=demand(priceKB,priceKR,priceMB,seg.demo[,1],seg.demo[,2],
#                  seg.demo[,3],seg.demo[,4],seg.demo[,5],seg.demo[,6],para)
# names(demandmat)="Pr(y=KB)"




## Topic A4: run Kmeans in a regression-in-logit framework
rm(list = ls());
set.seed(0)
# estimate multinomial logit segment-by-segment, where segmentation is defined by demographics
#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)


#Load demographic data
demo=fread("demo.csv",stringsAsFactors = F)

#Number of individuals
N=  100
NC=5

set.seed(0)
#Clustering
demo_cluster = kmeans(x=demo[, 2:9], centers = NC, nstart = 1500)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)


#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


# for those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = NC+1
# segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N


#Run gmnl just once, with factor variable of ID
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

mle=gmnl(choice~price+price:factor(cluster)|factor(cluster),data=mlogitdata)
summary(mle)

BIC(mle)



#Compare BIC with linear regression - we need to use the same set of people as in the regression.
#Drop those with no demographic info
data=data[!(data$cluster==NC+1),]
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

mle=gmnl(choice~price+price|factor(cluster),data=mlogitdata)
BIC(mle)
#MLE worse. Linear model may perform better here. However, "6 segment" Kmeans "with all demographics"
#is an arbitrary choice. We may be able to improve Kmean model fit buy configuring model differently.



#Any way to combine cluster and linear models?
rm(list = ls());
set.seed(0)
# estimate multinomial logit segment-by-segment, where segmentation is defined by demographics
#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)
#Load demographic data
demo=fread("demo.csv",stringsAsFactors = F)

#Number of individuals
N = 100
NC=2

set.seed(0)
#Clustering
demo_cluster = kmeans(x=demo[, 2:9], centers = NC, nstart = 1000)

# now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)


#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

#Merge demographics
data=merge(data,demo,by="id")
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

mle=gmnl(choice~price+factor(cluster):price|
           log(fam_size)+log(fem_age)+log(fem_educ)+fem_smoke+male_smoke+dogs,
         data=mlogitdata)
summary(mle)

BIC(mle)
#BIC=1034.7. Fit improves.


demand=function(priceKB,priceKR,priceMB,fam_size,fem_age,fem_educ,fem_smoke,male_smoke,dogs,cluster,para){
  
  #Define beta_0's for each product as a function of consumer characteristics
  beta0KB=para[1]+para[6]*log(fam_size)+para[9]*log(fem_age)+para[12]*log(fem_educ)+para[15]*fem_smoke+
    para[18]*male_smoke+para[21]*dogs
  
  beta0KR=para[2]+para[7]*log(fam_size)+para[10]*log(fem_age)+para[13]*log(fem_educ)+para[16]*fem_smoke+
    para[19]*male_smoke+para[22]*dogs  
  
  beta0MB=para[3]+para[8]*log(fam_size)+para[11]*log(fem_age)+para[14]*log(fem_educ)+para[17]*fem_smoke+
    para[20]*male_smoke+para[23]*dogs  
  
  beta1=para[4]*cluster+para[5]*(1-cluster)
  
  #Define choice probability of KB
  prob=exp(beta0KB+beta1*priceKB)/
    (1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
  return(prob)
}




##NOT USED
# #Calculate aggregate choice probability
# #Count how many consumers exist for each demographic type
# data[,demoid:=fam_size+10*fem_age+10^2*fem_educ+10^3*fem_smoke+10^4*male_smoke+10^5*dogs]
# 
# #Two ways to calculate proportion of each segment
# #(1)
# #Assuming that all demographic visit the store equally
# aux=unique(cbind(data$id,data$demoid,data$fam_size,data$fem_age,data$fem_educ,data$fem_smoke,data$male_smoke,data$dogs))
# #Create a matrix of each segment's demo to plug in to the demand
# seg.demo=unique((aux[order(aux[,2]),2:8]))
# #The demographic for the last segment (no demo info) = average of the observed demo
# seg.demo[nrow(seg.demo),]=colMeans(seg.demo[1:(nrow(seg.demo)-1),])
# #Calculate share of each segment
# seg.share=c(table(aux[,2])/nrow(aux),sum(is.na(aux[,2]))/nrow(aux))
# 
# #(2)
# #Allowing for different demographics with different propensity of visit
# aux=cbind(data$demoid,data$fam_size,data$fem_age,data$fem_educ,data$fem_smoke,data$male_smoke,data$dogs)
# #Create a matrix of each segment's demo to plug in to the demand
# seg.demo2=unique(aux[order(aux[,1]),])
# #The demographic for the last segment (no demo info) = average of the observed demo
# seg.demo2[nrow(seg.demo2),]=colMeans(seg.demo2[1:(nrow(seg.demo2)-1),])
# seg.share2=c(table(data$demoid)/nrow(data),1-sum(table(data$demoid)/nrow(data)))
# 
# 


