
#Load packages
library("data.table")
library("mlogit")
library("gmnl")
library("plotly") #Package for three dimensional plot



rm(list = ls());

#Change folder name to yours
setwd("YOUR FOLDER NAME")
#setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2023 Spring A pricing analytics/Topic 3")

##
#Multinomial logit: illustration
#Write choice probability as a function
demand=function(priceKB,priceKR,beta0KB,beta0KR,beta1){
  prob=exp(beta0KB+beta1*priceKB)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR))
  return(prob)
}

#Unit cost
uc=1;

#Demand case 1
pricespace=seq(0,3,0.001)
plot(pricespace,demand(pricespace,1,3,2,-2),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Profit case 1
#"demand" function represents each individual consumer's choice probability.
#In order to calculate profit, we multiply the "demand" by the number of consumers.
profit=1000*(demand(pricespace,1,3,2,-2)*pricespace-demand(pricespace,1,3,2,-2)*uc)

plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,300),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
pricespace[profit==max(profit)]
#Optimal price is 1.68


#Demand case 2
plot(pricespace,demand(pricespace,1,3,2,-2),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,3,3,2,-2),col="orange",lwd=2)


#Profit case 2
profit2=1000*(demand(pricespace,3,3,2,-2)*pricespace-demand(pricespace,3,3,2,-2)*uc)
plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,300),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit2,col="orange",lwd=2)
pricespace[profit2==max(profit2)]
#Optimal price is 1.78

#Price case 3
plot(pricespace,demand(pricespace,1,3,2,-2),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,1,3,5,-2),col="orange",lwd=2)

profit3=1000*(demand(pricespace,1,3,5,-2)*pricespace-demand(pricespace,1,3,5,-2)*uc)
plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,300),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit3,col="orange",lwd=2)
pricespace[profit3==max(profit3)]
#Optimal price is 1.52

#Price case 4
plot(pricespace,demand(pricespace,1,3,2,-2),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,3,3,2,-2),col="orange",lwd=2,lty=2)
lines(pricespace,demand(pricespace,1,3,5,-2),col="orange",lwd=2,lty=2)
lines(pricespace,demand(pricespace,3,3,5,-2),col="red",lwd=2)

profit4=1000*(demand(pricespace,3,3,5,-2)*pricespace-demand(pricespace,3,3,5,-2)*uc)
plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,300),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,profit2,col="orange",lwd=2,lty=2)
lines(pricespace,profit3,col="orange",lwd=2,lty=2)
lines(pricespace,profit4,col="red",lwd=2)
pricespace[profit4==max(profit4)]
#Optimal price is 1.73


#Different values of beta_0 and beta_1
#"demand" function represents each individual consumer's choice probability.
#In order to calculate profit, we multiply the "demand" by the number of consumers.
para=c(10,8,-2.95)
plot(pricespace,demand(pricespace,1,para[1],para[2],para[3]),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

profit=1000*(demand(pricespace,1,para[1],para[2],para[3])*pricespace-demand(pricespace,1,para[1],para[2],para[3])*uc)

plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,400),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
pricespace[profit==max(profit)]
#Optimal price is 1.68


#Demand case 2
pricespace2=seq(0,5,0.1)
plot(pricespace2,demand(pricespace2,1,para[1],para[2],para[3]),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2, xlim=c(0,5)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace2,demand(pricespace2,3,para[1],para[2],para[3]),col="orange",lwd=2)

#Profit case 2
profit2=1000*(demand(pricespace2,3,para[1],para[2],para[3])*pricespace2-demand(pricespace2,3,para[1],para[2],para[3])*uc)
plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(10,1500),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace2,profit2,col="orange",lwd=2)
pricespace2[profit2==max(profit2)]
#Optimal price is 2.8



#Product-line pricing
#Solve a profit maximization problem over two products

#Write choice probability for both KB and KR as a function
#Notational change - I use "para" to represent all parameter inputs,
#instead of separately defining them as "beta0KB,beta0KR,beta1".

demand=function(priceKB,priceKR,para){
  probKB=exp(para[1]+para[3]*priceKB)/(1+exp(para[1]+para[3]*priceKB)+exp(para[2]+para[3]*priceKR))
  probKR=exp(para[2]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKB)+exp(para[2]+para[3]*priceKR))
  return(cbind(probKB,probKR))
}

#Write profit as a function of prices we set and model parameters
profit=function(priceKB,priceKR,para){
  profitKB=demand(priceKB,priceKR,para)[,1]*(priceKB-1)
  profitKR=demand(priceKB,priceKR,para)[,2]*(priceKR-1)
  return(cbind(profitKB,profitKR))
}


#Set parameter
#The first element of "para" is beta0KB, the second element is beta0KR and the third beta1.
para=c(1,3,-2)

#Choose space of prices to search for the optimal price over
aux=seq(1,3,0.1)
#Because we search over two dimensions, create complete combination 
#of the two prices
pricespace=expand.grid(aux,aux)


#Compute profit at each realization of this price space.
#I write for-loop. While there are ways to get this done in a vectorized way,
#this for-loop code probably helps some in project 2.

#At each iteration of the loop, I take one realization of [P^KB,P^KR] pair and evaluate
#profit at that realization.
profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit(pricespace[i,1],pricespace[i,2],para))  
}

#Draw figure
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
                type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
            layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
            config(mathjax = 'cdn')
p
          
#If running this in a vectorized manner, simply
#profitmat=rowSums(profit(pricespace[,1],pricespace[,2],para))





##
#Estimate a multinomial logit model using gmnl and mlogit
#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)

#Drop observations with stockout
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

#Now columns 4 through 7 contains "Price.something" info.
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

coef=mle$coefficients




#NOT USED
# ##
# #Run MLE by hand
# #Read data
# data=read.csv("kiwi_bubbles.csv",head=T)
# data=data.frame(data)
# 
# data=data[!(data$price.KB==99),]
# data=data[!(data$price.KR==99),]
# data=data[!(data$price.MB==99),]
# 
# #Define variables
# id=data$id
# choice=data$choice #This is our outcome variable now
# priceKB=data$price.KB
# priceKR=data$price.KR
# priceMB=data$price.MB
# 
# #Define choice probability for each product as a function of parameters.
# #Denominators are all the same.
# qfunKB=function(para) exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)
#                                                     +exp(para[2]+para[4]*priceKR)
#                                                     +exp(para[3]+para[4]*priceMB))
# 
# qfunKR=function(para) exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)
#                                                     +exp(para[2]+para[4]*priceKR)
#                                                     +exp(para[3]+para[4]*priceMB))
# 
# qfunMB=function(para) exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)
#                                                     +exp(para[2]+para[4]*priceKR)
#                                                     +exp(para[3]+para[4]*priceMB))
# 
# #Calculate log-likelihood
# loglike=function(para) -1*sum((choice=="KB")*log(qfunKB(para)) 
#                               +(choice=="KR")*log(qfunKR(para))
#                               +(choice=="MB")*log(qfunMB(para))
#                               + (choice==0)*log(1-qfunKB(para)-qfunKR(para)-qfunMB(para)))
# 
# 
# 
# #Maximize likelihood
# mle = optim(par = c(0,0,0,0), fn= loglike)
# mle

