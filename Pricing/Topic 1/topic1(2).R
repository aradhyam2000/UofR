

#Load packages
library("lfe") #Absolutely needed
library("data.table") #Absolutely needed

library("fastDummies") #fastDummies package used for creation of dummy variables
library("AER") #AER package used for "scatterplot" function
library("psych") #psych package used for "describeBy" function


#Set working space
rm(list = ls());
#setwd("YOUR FOLDER NAME")
#setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2023 Spring A pricing analytics/Topic 1")


#Example 1
#Read data and run a regression
#We use "fread" function from "data.table" package
simdata=fread("simulate_example_data.csv",stringsAsFactors = T)

reg=lm(sales~price,data=simdata)


#Scatterplot of prices and quantities
plot(simdata$price,simdata$sales, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Add the regression line to the plot
pricespace=seq(0.5,2,0.1)
coef=reg$coefficients
fitted=(coef[1]+coef[2]*pricespace)
lines(pricespace,fitted,col="blue",lwd=2)



#Run a Log-log regression
simdata=fread("simulate_example_data.csv",stringsAsFactors = T)

#Both X and Y are now in log.
reg=lm(log(sales)~log(price),data=simdata)

#Scatterplot of prices and quantities
plot(simdata$price,simdata$sales, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Add the regression line
pricespace=seq(0.5,2,0.1)
coef=reg$coefficients
fitted=(exp(coef[1]+coef[2]*log(pricespace)))
lines(pricespace,fitted,col="blue",lwd=2)




#Example 2: Adding dummy variables
simdata=fread("simulate_example_data2.csv",stringsAsFactors = T)

#Regression line without dummy
reg=lm(log(sales)~log(price),data=simdata)

#Scatterplot
plot(simdata$price,simdata$sales, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Add the regression line
coef=reg$coefficients
pricespace=seq(130,265,0.5)
fitted=(exp(coef[1]+coef[2]*log(pricespace)))
lines(pricespace,fitted,col="blue",lwd=2)


#Add a dummy variable representing August
#We use "dummy_cols" function in "fastdummy" package. 
#"dummy_cols" takes categorical variables as an input and
#create corresponding dummies.

#In our case, we use "month" as category.
august=dummy_cols(simdata$month)[,3]
#When we create dummies like this, drop the first two columns 
#of the output. The first column is the original simdata$month
#so no longer needed, and the second column is the dummy for 
#February, which we need to drop for level normalization.

reg2=lm(log(sales)~log(price)+august,data=simdata)

#Plot regression lines
coef2=reg2$coefficients
pricespace=seq(130,265,0.5)
fittedaug=(exp(coef2[1]+coef2[2]*log(pricespace)+coef2[3]))
fittedfeb=(exp(coef2[1]+coef2[2]*log(pricespace)))

plot(simdata$price,simdata$sales, col=rep(1:2,each=8),pch=19, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("bottomright", legend = c("February", "August"), col = 1:2, pch = 19, bty = "n",cex=1.5)
lines(pricespace,fittedaug,col="blue",lwd=2)
lines(pricespace,fittedfeb,col="blue",lwd=2)


#Extension: Add interaction term between dummies and prices
logpaug=log(simdata$price)*august
reg3=lm(log(sales)~log(price)+august+logpaug,data=simdata)
coef3=reg3$coefficients


#Plot results
pricespace=seq(130,265,0.5)
fittedaug2=(exp(coef3[1]+(coef3[2]+coef3[4])*log(pricespace)+coef3[3]))
fittedfeb2=(exp(coef3[1]+coef3[2]*log(pricespace)))

plot(simdata$price,simdata$sales, col=rep(1:2,each=8),pch=19, main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
legend("bottomright", legend = c("February", "August"), col = 1:2, pch = 19, bty = "n",cex=1.5)
lines(pricespace,fittedaug,col="blue",lwd=2,lty=2)
lines(pricespace,fittedfeb,col="blue",lwd=2,lty=2)
lines(pricespace,fittedaug2,col="blue",lwd=2)
lines(pricespace,fittedfeb2,col="blue",lwd=2)


#----------------------------------------------------------

#IV regression example
library("lfe")
library("data.table")

#Read data
simdata=fread("simulate_example_data3.csv",stringsAsFactors = F)

#Standard OLS
reg=lm(log(sales)~log(price),data=simdata)

#IV regression using "fuel_price" as an instrument for log(price)
regiv=felm(log(sales)~ 1| 0|(log(price)~fuel_price),data=simdata)


#Plot results
coef=reg$coefficients
coefiv=regiv$coefficients

pricespace=seq(100,220,1)
fitted=(exp(coef[1]+coef[2]*log(pricespace)))
fittediv=(exp(coefiv[1]+coefiv[2]*log(pricespace)))

plot(simdata$price,simdata$sales,  main="Price vs Sales",
     xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

lines(pricespace,fitted,col="blue",lwd=2,lty=2)
lines(pricespace,fittediv,col="blue",lwd=2)


#Test the relevance of IV.
reg=lm(log(price)~fuel_price,data=simdata)
summary(reg)

cor(log(simdata$price),log(simdata$fuel_price))





#Cross-elasticity example
simdata=fread("simulate_example_data4.csv",stringsAsFactors = F)
simdata=simdata[1:70,]

#Add rival prices to the estimation
reg1 <- felm(log(quantity) ~ log(price)| month+location  , data=simdata)
summary(reg1)
reg2 <- felm(log(quantity) ~ log(price)+log(price2)| month+location  , data=simdata)
summary(reg2)
reg3 <- felm(log(quantity) ~ log(price)+log(price2)+log(price3)| month+location  , data=simdata)
summary(reg3)


#Plot demand prediction
#Get values of fixed effects from reg1 and reg3.
#I evaluate profit at market 101 in January
pricespace=seq(5000,10000,10)

#Use "getfe" function to get the values of fixed effect 
#estimates from the regression 1.
#In this case, I have month FE and location FE.
#So to evaluate profit at market 101 in Jan,
#we need "the value of FE for market 101"
#and "the value of FE for Jan".
fe1=getfe(reg1)

#Find location of Jan fixed effect.
idc <- match('1',fe1$idx)
#Find location of market 101 fixed effect.
idm <- match('101',fe1$idx)

#Pick up the value of FEs. Plug this thing in later in my plot.
subfe1=fe1[c(idc,idm),1]

#Do the same for regression 3
fe3=getfe(reg3)
idc <- match('1',fe3$idx)
idm <- match('101',fe3$idx)
subfe3=fe3[c(idc,idm),1]



#Compute demand line with fixed effects obtained above, at average rival prices
demand=sum(subfe3)+reg3$coefficients[1]*log(pricespace)+
       reg3$coefficients[2]*log(mean(simdata$price2))+reg3$coefficients[3]*log(mean(simdata$price3))
demand2=sum(subfe3)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(mean(simdata$price2)*1.1)+reg3$coefficients[3]*log(mean(simdata$price3))
demand3=sum(subfe3)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(mean(simdata$price2))+reg3$coefficients[3]*log(mean(simdata$price3)*1.1)

#Plot it
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(pricespace,exp(demand), main="Price vs Quantity sold",
     type='l',xlab="Price",ylab="Quantity",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,exp(demand2),col="red",lwd=2,lty=2)
lines(pricespace,exp(demand3),col="orange",lwd=2)
legend("topright", legend = c("Benchmark", "P2 + 10%", "P3 + 10%"), col = c("blue","red","orange"), lty = c(1,2,1))



#Consider month FE in the analysis
#Check distribution of month FE and rival prices at each month
boxplot(simdata$price2~simdata$month,xlab="Month",ylab="Prices",main="Boxplot of price 2 / month")
plot(c(1,2,3,4,5,6,7,8,9,10,11,12),exp(fe3[1:12,1]),col="red",lwd=2,lty=2,xlab="Month",ylab="exp(Fixed effects)"
     ,main="Month fixed effects")

par(mar=c(5, 4, 4, 6))
boxplot(simdata$price2~simdata$month,xlab="Month",ylab="Prices",main="Boxplot of price 2 and FE / month")
box()
par(new=TRUE)
plot(c(1,2,3,4,5,6,7,8,9,10,11,12),exp(fe3[1:12,1]),axes = FALSE, xlab = "", ylab = "",col="red",lwd=2,lty=2)
axis(side=4, at = pretty(range(exp(fe3[1:12,1]))))
mtext("exp(Fixed effect)", side=4, line=2)

par(mar=c(5.1, 4.1, 4.1, 2.1))
#"describeBy" function in "psych" package is pretty good for category-by-category tabulation.
describeBy(simdata$quantity, simdata$month)



#Obtain values of FE in October
idc <- match('10',fe3$idx)
idm <- match('101',fe3$idx)
subfeoct=fe3[c(idc,idm),1]

#Obtain average rival prices for each month
pricemonth=aggregate(simdata[, 5:6], list(simdata$month), mean)

#Plot demand prediction in January and October
demandjan=sum(subfe3)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(pricemonth[1,2])+reg3$coefficients[3]*log(pricemonth[1,3])
demandfeoct=sum(subfeoct)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(pricemonth[1,2])+reg3$coefficients[3]*log(pricemonth[1,3])
demandoct=sum(subfeoct)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(pricemonth[10,2])+reg3$coefficients[3]*log(pricemonth[10,3])

#Compare Jan vs Oct
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(pricespace,exp(demandjan), main="Jan - Oct comparison",
     type='l',xlab="Price",ylab="Quantity",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,exp(demandoct),col="red",lwd=2,lty=2)
legend("topright", legend = c("January", "October"), col = c("blue","red"), lty = c(1,2))


#Decompose effects
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(pricespace,exp(demandjan), main="Jan - Oct comparison",
     type='l',xlab="Price",ylab="Quantity",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,exp(demandoct),col="red",lwd=2,lty=2)
lines(pricespace,exp(demandfeoct),col="orange",lwd=2,lty=1)
legend("topright", legend = c("January", "October FE", "October FE + Rival prices"), col = c("blue","orange","red"), lty = c(1,1,2))





#Compare between no rival price vs with rival price
demand0=sum(subfe1)+reg1$coefficients[1]*log(pricespace)
demand01=sum(subfe3)+reg3$coefficients[1]*log(pricespace)+
        reg3$coefficients[2]*log(mean(simdata$price2)*0.9)+reg3$coefficients[3]*log(mean(simdata$price3)*0.9)
plot(pricespace,exp(demand01), main="Price vs Quantity sold",
     type='l',xlab="Price",ylab="Quantity",col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,exp(demand0),col="red",lwd=2,lty=2)
legend("topright", legend = c("With cross effect", "Without cross effect"), col = c("blue","red"), lty = c(1,2))

#Check correlations
scatterplot(simdata$price,simdata$price3,xlab="Price1",ylab="Price3",main="Scatterplot between price and price3")
cor(simdata$price,simdata$price3)



##
#Not used
# #Natural experiment
# price=c(1,1,1,1,1,1.1,1.1,1.1,1.1,1.1)
# sales=c(80,84,73,91,82,78,69,79,68,77)
# 
# 
# reg2=lm(log(sales)~log(price))
# coef=reg2$coefficients
# 
# pricespace=seq(1,1.1,0.01)
# fitted=(exp(coef[1]+coef[2]*log(pricespace)))
# 
# plot(price,sales, col=rep(1:2,each=5),pch=19, main="Price vs Sales",
#      xlab="Price", ylab="Sales", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlim=c(0.9,1.2))
# legend("bottomright", legend = c("Pre-tax", "Post-tax"), col = 1:2, pch = 19, bty = "n",cex=1.5)
# lines(pricespace,fitted,col="blue",lwd=2)
#
#End of unused part
##

