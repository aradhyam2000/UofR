library("scatterplot3d")


rm(list = ls());

#Change folder name to yours
setwd("G:/Takeaki/Google drive/Past classes/Rochester Simon/2021 spring pricing analytics/Topic A2")


#Calculate optimal price associated with demand = 12 - 8.7*price

#Approach 1: brute force
#Take a lot of candidate prices
pricespace=seq(1,1.8,0.001)

#Evaluate demand at each point
logdemand=12-8.7*log(pricespace)

#Evaluate profit at each point
profit=(pricespace-1)*exp(logdemand)

#Find the maximum
pricespace[profit==max(profit)]


#Plot demand and sales
plot(pricespace,exp(logdemand), main="Price vs Quantity sold",
     type='l',xlab="Price",ylab="Quantity",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(pricespace,profit, main="Price vs Profit",
     type='l',xlab="Price",ylab="Profit",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5
     ,ylim=c(0,8000))



#Brute force with two dimensional prices
#Create two-dimensional price space
aux=seq(1,1.8,0.001)
pricespace=expand.grid(aux,aux)

#Evaluate demand at each point
logdemand1=12-8.7*log(pricespace[,1])+0.1*log(pricespace[,2])
logdemand2=10-7.9*log(pricespace[,2])+0.6*log(pricespace[,1])

#Calculate profit
profit=(pricespace[,1]-1)*exp(logdemand1)+(pricespace[,2]-1)*exp(logdemand2)

#Find profit-maximizing price
pricespace[profit==max(profit),]




#Use of "optim" function

#Define profit as a function of prices
#The input "price" is a 2x1 object. The first argument is P1, and the second is P2.
profit=function(price){
        profit=-1*((price[1]-1)*exp(12-8.7*log(price[1])+0.1*log(price[2]))
                   +(price[2]-1)*exp(10-7.9*log(price[2])+0.6*log(price[1])))
        return(profit)
}

#Profit saved by the "function" command is considered as a function.
#If we plug in some value of price, it returns the value of the profit.
profit(c(1,2))
profit(c(1,1.5))

#Find the optimal price, starting from the initial guess of P1=1, P2=1.
optim(c(1,1),profit)


