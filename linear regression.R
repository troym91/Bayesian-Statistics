rm(list=ls())
clr
golf <- read.table("http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat")
golf
names(golf) <- c("Average_Distance", "Percentage", "Sex")
golfF <- subset(golf, Sex == 1, select = 1:2)
par(mfrow=c(1,1))
golfM <- subset(golf, Sex == 2, select = 1:2)
plot(golfM$Average_Distance, golfM$Percentage)
plot(golfF$Average_Distance, golfF$Percentage)
Fem <- lm(Percentage ~ Average_Distance, data=golfF)
summary(Fem)
round(coef(Fem)[2],2)
predict(Fem, data.frame(Average_Distance = 260), interval = "predict")


golf$Sex <- golf$Sex-1
golf$Sex
golf
attach(golf)
golf_lin <- lm(Percentage ~ .,data = golf)
summary(golf_lin)
plot(fitted(golf_lin), residuals(golf_lin))
1000/sqrt(2)
