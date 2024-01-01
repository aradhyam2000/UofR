
#####################################################################
#IMPORTANT NOTE:
#Most recent version of mlogit package has some compatibility issues
#with the gmnl package. Before you try the code in this file,
#please close Rstudio once, and launch it again AS ADMINISTRATOR,
#and run the following code.

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source", INSTALL_opts=c("--no-multiarch"))

#This will install mlogit version 1.0.2. To see if you have the correct
#version, run
packageVersion("mlogit")
#If it shows 1.0.2, you are good to go!
####################################################################

#Load packages - because we do some advanced stuff we need more packages
library("mlogit")
library("gmnl")
library("data.table")
library('RColorBrewer')
library("plotly")
library("dplyr")
rcol <- brewer.pal(9, 'Blues')

#Change folder name to yours
#setwd("YOUR FOLDER NAME")
setwd("C:\ Users\ aradh\ Desktop\ Spring 23\ Pricing\ Topic 5")

##
#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)
#Load demographic data
demo=fread("demo_P2.csv",stringsAsFactors = F)
#Merge with original data
data=merge(data,demo,by="id")

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

meanPKB=mean(data$price.KB)
meanPKR=mean(data$price.KR)
meanPMB=mean(data$price.MB)

#Re-number the id - use only those who remain in the data.
#Unlike demographic-based segmenting, proportion is estimated along with other parameters.
#Hence no way we can include those without purchase data in calculating the proportion.
data$id <- data %>% group_indices(id) 

#Define mlogitdata in gmnl format
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")


##
#Baseline model without any segmentation
#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)
BIC(mle)

##
#Run k-type model
#Set number of segments 
#By changing NC=2,3,4, we can replicate results from the slide.
NC=3

lc2=gmnl(choice~price|1|0|0|1,data=mlogitdata,model='lc',Q=NC,panel=TRUE)
#NOTE: if you see "argument not a matrix" error here, please close the Rstudio and follow
#the instruction at the beginning of this code file.

#If it runs but produces NaN, manupulate the starting value.
#,start=rep(0,4*NC+(NC-1))
summary(lc2)

BIC(lc2)
#BIC improves.

#Define a matrix of coefficient
coef.est=matrix(0L,NC,5)
coef.est = data.frame(segment = 1:NC, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 
for (i in 1:NC){
  coef.est[i,]=rbind(c(i,lc2$coefficients[((i-1)*4+1):(i*4)]))
}

#Define segment share
seg.share=matrix(0L,NC,1)
for (i in 2:NC){
  denom=1+sum(exp(lc2$coefficients[(NC*4+1):(NC*4+NC-1)]))
  seg.share[i]=exp(lc2$coefficients[(NC*4+i-1)])/denom
}
seg.share[1]=1/(1+sum(exp(lc2$coefficients[(NC*4+1):(NC*4+NC-1)])))
seg.share=as.double(seg.share)

#Plot results
#Define demand function
demand=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}

#Plot demand
pricespace=seq(0.5,1.8,0.01)
plot(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[1,2:5]))[,1],type='l',xlab='Prices',
     ylab='Probability of buying KB',col="blue",lwd=10*seg.share[1],ylim=c(0,1),
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
for (seg in 2:(NC)) {
  lines(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[seg,2:5]))[,1],col="blue",lwd=10*seg.share[seg])
}





#For further analysis, we switch data.
#This data is easier to highlight the key segmentation with.
rm(list = ls());
setwd("G:/Takeaki/Google drive/Past classes/Rochester Simon/2021 Fall B pricing analytics/Topic 5")

##
#Read data
data=fread("kiwi_bubbles_2.csv",stringsAsFactors = F)
#Load demographic data
demo=fread("demo_2.csv",stringsAsFactors = F)
#Merge with original data
data=merge(data,demo,by="id")

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

meanPKB=mean(data$price.KB)
meanPKR=mean(data$price.KR)
meanPMB=mean(data$price.MB)

#Re-number the id - use only those who remain in the data.
#Unlike demographic-based segmenting, proportion is estimated along with other parameters.
#Hence no way we can include those without purchase data in calculating the proportion.
data$id=group_indices(data,id)


#Define mlogitdata in gmnl format
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")


#Set number of segments 
#We keep 4 segment model
NC=4

#Run four type model
lc4=gmnl(choice~price|1|0|0|1,data=mlogitdata,model='lc',Q=NC,panel=TRUE)
#If it produces NaN, manupulate the starting value.
#,start=rep(0,4*NC+(NC-1))
summary(lc4)

coef.est=matrix(0L,NC,5)
coef.est = data.frame(segment = 1:NC, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 
for (i in 1:NC){
  coef.est[i,]=rbind(c(i,lc4$coefficients[((i-1)*4+1):(i*4)]))
}

seg.share=matrix(0L,NC,1)
for (i in 2:NC){
  denom=1+sum(exp(lc4$coefficients[(NC*4+1):(NC*4+NC-1)]))
  seg.share[i]=exp(lc4$coefficients[(NC*4+i-1)])/denom
}
seg.share[1]=1/(1+sum(exp(lc4$coefficients[(NC*4+1):(NC*4+NC-1)])))
seg.share=as.double(seg.share)


#Plot results
#Define demand function
demand=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}

#Plot demand
pricespace=seq(0.5,1.8,0.01)
plot(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[1,2:5]))[,1],type='l',xlab='Prices',
     ylab='Probability of buying KB',col="blue",lwd=10*seg.share[1],ylim=c(0,1),
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
for (seg in 2:(NC)) {
  lines(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[seg,2:5]))[,1],col="blue",lwd=10*seg.share[seg])
}


colorset2=c("blue","green","orange","red")
#Scatterplot of parameters KB - KR against KR - MB
colorset=c("red","blue","green")
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,4]-coef.est[1,3],cex=20*seg.share[1],xlim=c(-4,4),ylim=c(-4,4),
     col = colorset2[1],pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab=("beta0MB-beta0KR"))
for (seg in 2:NC) {
  points(coef.est[seg,2]-coef.est[seg,3],coef.est[seg,4]-coef.est[seg,3],cex=20*seg.share[seg],col = colorset2[seg],pch=16)
}
legend(1.7, -0.4, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)
lines(c(0,0),c(-5,5),lwd=1,lty=2)
lines(c(-5,5),c(0,0),lwd=1,lty=2)


#Location of beta 1
plot(c(-8,-1),c(0,0),type="l",xlim=c(-6,-2),ylim=c(-0.2,0.4),
     lwd=1,lty=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta1",ylab=(""), yaxt="n")
for (seg in 1:NC) {
  points(coef.est[seg,5],0,cex=20*seg.share[seg],col = colorset2[seg],pch=16)
}
legend(-3.5, 0.4, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)


#Scatterplot of parameters KB against KR
colorset=c("red","blue","green")
plot(coef.est[1,2]-coef.est[1,3],coef.est[1,5],cex=20*seg.share[1],xlim=c(-4,5),ylim=c(-8,-0.7),
     col = colorset2[1],pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab=("beta1"))
for (seg in 2:NC) {
  points(coef.est[seg,2]-coef.est[seg,3],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=16)
}
legend(1.2, -0.4, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)


#KR against MB
plot(coef.est[1,3]-coef.est[1,4],coef.est[1,5],cex=20*seg.share[1],xlim=c(-4,4),ylim=c(-8,-1),
     col = colorset2[1],pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KR-beta0MB",ylab=("beta1"))
for (seg in 2:NC) {
  points(coef.est[seg,3]-coef.est[seg,4],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=16)
}
legend(1.2, -5.3, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)




type=c(1,2,4,3)
xaxis=list(autorange = "reversed",title="b0KB-b0KR")
yaxis=list(autorange = "reversed",title="b0KR-b0MB")
zaxis=list(title="b1")
p=plot_ly(coef.est,x=(coef.est$intercept.KB-coef.est$intercept.KR),y=(coef.est$intercept.KR-coef.est$intercept.MB),
          z=coef.est$price.coef,split=type,type="scatter3d",mode="markers")%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p



#Plot demand with different coloring
pricespace=seq(0.5,1.8,0.01)
plot(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[1,2:5]))[,1],type='l',xlab='Prices',
     ylab='Probability of buying KB',col="blue",lwd=10*seg.share[1],ylim=c(0,1),
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
for (seg in 2:(NC)) {
  lines(pricespace,demand(pricespace,meanPKR,meanPMB,as.numeric(coef.est[seg,2:5]))[,1],col=colorset2[seg],lwd=10*seg.share[seg])
}
legend(1.3, 1, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), lty=1, cex=1.1)

pricespace=seq(0.5,3,0.01)
plot(pricespace,demand(meanPKB,pricespace,meanPMB,as.numeric(coef.est[1,2:5]))[,2],type='l',xlab='Prices',
     ylab='Probability of buying KB',col="blue",lwd=10*seg.share[1],ylim=c(0,1),
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
for (seg in 2:(NC)) {
  lines(pricespace,demand(meanPKB,pricespace,meanPMB,as.numeric(coef.est[seg,2:5]))[,2],col=colorset2[seg],lwd=10*seg.share[seg])
}
legend(2.0, 1, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), lty=1, cex=1.1)




#Consider targeting in this market.
#Calculate coefficient using prior proportion (w_k)
priorcoef=colSums(seg.share*coef.est[,2:5])

#Calculate each consumer's predicted beta0 and beta1 using posterior.
aux=effect.gmnl(lc4)
postcoef=aux$mean


#Check if the posterior makes sense.
#Those who buy a lot of KR
i=3
subdata=data[data$id==i,]
subdata$choice

postcoef[3,]
coef.est[4,2:5]


#Those who buy a lot of KB
i=11
subdata=data[data$id==i,]
subdata$choice

postcoef[11,]
coef.est

#Those who never buy any
i=4
subdata=data[data$id==i,]
subdata$choice

postcoef[4,]
priorcoef

coefmat=rbind(postcoef[4,],priorcoef)
rownames(coefmat)=c("effect.gmnl outcome","Prior-based weighted avg")

#Histogram of WTP
#KB
hist(-postcoef[,1]/postcoef[,4],
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay for KB",ylab=("Density"),main="")
#KR
hist(-postcoef[,2]/postcoef[,4],
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay for KR",ylab=("Density"),main="")



#Scatterplot of parameters KB against KR
plot(as.matrix(postcoef[,1]-postcoef[,2]),as.matrix(postcoef[,4]),cex=1,xlim=c(-3,3),ylim=c(-6.5,-1),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab="beta1")
#overlay with model-predicted segments
for (seg in 1:NC) {
  points(coef.est[seg,2]-coef.est[seg,3],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=1)
}
legend(1.2, -0.7, legend=c("Bubble type", "KB type", "KR type", "MB type"),
       col=c( "blue","orange","red","green"), pch=1, cex=1.1)




#Scatterplot of parameters KB against KR
colorset=c("red","blue","green","orange")
colorvec=(postcoef[,1]-postcoef[,2]<(-1.5))+2*(((postcoef[,1]-postcoef[,2])>0.8)&(postcoef[,4]<(-5)))+3*(((postcoef[,1]-postcoef[,2])<0.8)&((postcoef[,1]-postcoef[,2])>(-1.5)))+4*(((postcoef[,1]-postcoef[,2])>0.8)&(postcoef[,4]>(-5)))

plot(postcoef[1,1]-postcoef[1,2],postcoef[1,4],cex=1,xlim=c(-3,3),ylim=c(-6.5,-1),
     col = colorset[colorvec[1]],pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab=("beta1"))
for (seg in 2:nrow(postcoef)) {
  points(postcoef[seg,1]-postcoef[seg,2],postcoef[seg,4],cex=1,col = colorset[colorvec[seg]],pch=16)
}
for (seg in 1:NC) {
  points(coef.est[seg,2]-coef.est[seg,3],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=1)
}
legend(0.6, -0.7, legend=c("Likely bubble type", "Likely KB type", "Likely KR type", "Likely MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)



#Scatterplot of parameters KR against MB
plot(postcoef[1,2]-postcoef[1,3],postcoef[1,4],cex=1,xlim=c(-3,4),ylim=c(-6.5,-1),
     col = colorset[colorvec[1]],pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KR-beta0MB",ylab=("beta1"))
for (seg in 2:nrow(postcoef)) {
  points(postcoef[seg,2]-postcoef[seg,3],postcoef[seg,4],cex=1,col = colorset[colorvec[seg]],pch=16)
}
for (seg in 1:NC) {
  points(coef.est[seg,3]-coef.est[seg,4],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=1)
}
legend(-3, -0.7, legend=c("Likely bubble type", "Likely KB type", "Likely KR type", "Likely MB type"),
       col=c( "blue","orange","red","green"), pch=16, cex=1.1)



