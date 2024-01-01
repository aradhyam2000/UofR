
rm(list = ls());
setwd("your folder")



#Q1
data=t(rbind(c(0.8,0.9,1,1.1,1.2,1.3),c(0.515,0.29,0.171,0.085,0.079,0.05),c(0.819,0.756,0.68,0.505,0.477,0.463)))
colnames(data)=c("price","share1","share2")
data=data.frame(data)

agg.share=0.7*data$share1+0.3*data$share2
          
data=cbind(data,agg.share)

reg=lm(log(1000*agg.share)~log(price),data)
summary(reg)
#Agg elasticity = 2.68


#I don't have the code for the single-segment profit maximization. 
#Rather I reused the code for two-segment case shown below.


#Seg-by-seg share regression
reg1=lm(log(700*share1)~log(price),data)
summary(reg1)
#Elasticity for segment 1 = 4.82

coef1=reg1$coefficients

reg2=lm(log(300*share2)~log(price),data)
summary(reg2)
#Elasticity seg 2 = 1.34

coef2=reg2$coefficients
uc=0.7
profit=function(price){
        exp(coef1[1]+coef1[2]*log(price))*(price-uc)+exp(coef2[1]+coef2[2]*log(price))*(price-uc)
}

#Plot profit
pricespace=seq(0.7,3,0.01)
plot(pricespace,profit(pricespace), main="Price vs Profit",
     type='l',xlab="Price",ylab="Profit",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5
     ,ylim=c(0,120))

profitmax=pricespace[profit(pricespace)==max(profit(pricespace))]





