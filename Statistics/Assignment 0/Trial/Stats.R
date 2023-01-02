library(readr)
library(ggplot2)
library(moments)
#question1
data1 <- read_csv("car_sales.csv")
summary(data1)
data2 <- data1[c('price')]
Price <- as.numeric(data2$price)
bins <- ceiling(log2(n)) + 1 #Sturges Rule
n <- 152 # 152 observations are seen in data2
bins <- ceiling(log2(152)) + 1 #Substituting value of n

hist(Price, breaks = bins, xlab = "Price of Cars", ylab = "Number of Cars", col = blues9, main = "Histogram with 9 Bins depicting Price of Cars")
#question2
mean <- mean(data2$price)
median <- median(data2$price)
trim <- mean(data2$price, trim=0.1)
abline(v = mean(data2$price), col="red", lwd=3)
abline(v = median(data2$price), col="blue", lwd=3)
abline(v = mean(data2$price, trim=0.1), col="green", lwd=3)
text(mean, 54.5, bquote("(" * bar(x) * " = " * .(mean) *  ")"), col="#ff0000")
text(median, 50.5, bquote("(" * tilde(x) * " = " * .(median) * ")"), col="#0000FF")
text(trim, 46.5, bquote("(" * bar(x)[10] * " = " * .(trim) *  ")"), col="#00ff00")
quantile <- quantile(data2$price, probs = c(.25, .75))
IQR <- IQR(data2$price)
percentile1 <- quantile(data2$price, probs = c(.25))
percentile2 <- quantile(data2$price, probs = c(.75))
lfence <- percentile1 - (IQR*1.5)
ufence <- percentile2 + (IQR*1.5)
span <- IQR*1.5
outlier1 <- data2[data2$price >= 53013.75, ]
outlier2 <- data2[data2$price <= -3186.25, ]
var <- var(data2$price)
sd <- sd(data2$price)
cv <- sd/mean*100
skewness(Price)
#question3
bxcx <- boxcox(data2$price ~ 1)
bxcx$data2$price[bxcx$y==max(bxcx$y)]
