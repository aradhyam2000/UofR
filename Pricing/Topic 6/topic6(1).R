
#Load packages
library("mlogit")
library("gmnl")
library("data.table")


rm(list = ls());

#Change folder name to yours
setwd("YOUR FOLDER NAME")
#setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2023 Spring A pricing analytics/Topic 6")


data=fread("smartwatch.csv",stringsAsFactors = F)
specs=fread("design_choice.csv",stringsAsFactors = F)
data=data[,-1]

#Convert the data to mlogit form. This time assign all columns that
#contain all product attributes.
mlogitdata=mlogit.data(data,id="id",varying=4:23,choice="choice",shape="wide")

#Run MLE.
mle= gmnl(choice ~  -1+const+battery+memory+android+price, data = mlogitdata)

summary(mle)
BIC(mle)


coef=mle$coefficients

#Calculate WTP for existing products.
wtpprod=-as.matrix(cbind(1,specs[,3:5]))%*%(as.matrix(coef[1:4]))/coef[5]

#Histogram
hist(wtpprod,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay for product j",ylab="Number of products",main="")

#Plot WTP against attributes
plot(specs$battery,wtpprod,cex=2,xlim=c(0,5),ylim=c(50,350),
     col = "orange",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Battery life",ylab="Willingness to pay")

#Plot WTP against attributes
plot(specs$memory,wtpprod,cex=2,xlim=c(1,8),ylim=c(50,350),
     col = "orange",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Memory size",ylab="Willingness to pay")


#Add lines to demostrate extrapolation risks
mlogitdata$memorysq=mlogitdata$memory^2/10
mle2= gmnl(choice ~  -1+const+battery+memory+memorysq+android+price, data = mlogitdata)
coef2=mle2$coefficients

memoryspace=seq(0,32,0.1)
ss=as.matrix(specs)
wtp1=(coef[1]+mean(ss[,3])*coef[2]+memoryspace*coef[3]+mean(ss[,5])*coef[4])/(-coef[5])
wtp2=(coef2[1]+mean(ss[,3])*coef2[2]+memoryspace*coef2[3]+memoryspace^2/10*coef2[4]+mean(ss[,5])*coef2[5])/(-coef2[6])


plot(specs$memory,wtpprod,cex=2,xlim=c(1,32),ylim=c(50,1000),
     col = "orange",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Memory size",ylab="Willingness to pay")
lines(memoryspace,wtp1,col="blue",lwd=2)
lines(memoryspace,wtp2,col="red",lwd=2)

legend(3, 900, legend=c("Linear model", "Quadratic model"),
       col=c("blue", "red"), lty=c(1,1), cex=0.8)


#What if we create a new product, with 8GB memory, 4 days of battery life with android OS?
beta0new=sum(coef[1:4]*c(1,4,6,1))
wtp0new=-beta0new/coef[5]

#WTP for each component
wtpcoef=-coef[1:4]/coef[5]





##
# #Create data - please ignore
# rm(list = ls());
# setwd("G:/Takeaki/Google drive/Past classes/Rochester Simon/2021 Fall B pricing analytics/Topic 6")
# 
# aux=fread("survey2.csv",stringsAsFactors = F)
# attributes=fread("design_choice2.csv",stringsAsFactors = F)
# 
# data=merge(aux,attributes,by="scenario")
# data=data[order(data$id),]
# 
# 
# #Change name of the variables, so that mlogit can read it
# setnames(data,"battery1","battery.1")
# setnames(data,"battery2","battery.2")
# setnames(data,"battery3","battery.3")
# setnames(data,"memory1","memory.1")
# setnames(data,"memory2","memory.2")
# setnames(data,"memory3","memory.3")
# setnames(data,"price1","price.1")
# setnames(data,"price2","price.2")
# setnames(data,"price3","price.3")
# setnames(data,"android1","android.1")
# setnames(data,"android2","android.2")
# setnames(data,"android3","android.3")
# data[,battery.0:=0]
# data[,memory.0:=0]
# data[,android.0:=0]
# data[,price.0:=0]
# data[,const.0:=0]
# data[,const.1:=1]
# data[,const.2:=1]
# data[,const.3:=1]
# 
# data=data[,c(2,1,3,21,4:7,22,8:11,23,12:15,20,16:19)]
# 
# write.csv(data,file="smartwatch.csv")
#
#Data creation done.
##
