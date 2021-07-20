###Question 1
##Understand the data
Dir="/Volumes/data/Deakin/Real World Analytics"
Dir
setwd(Dir)
the.data <- as.matrix(read.table("Energy20.txt"))
the.data = data.frame(the.data)
my.data<-the.data[sample(1:671,300),c(1:6)]
my.data = data.frame(my.data)
attach(my.data)
summary(my.data)

## Create 5 Scatter Plots
#X1~Y
png("Scatterplot1.png",width=677,height=400)
plot(V6~V1, data = my.data,frame=FALSE,
     main="Relationship between Energy use and Temperature in kitchen area",
     xlab="Temperature in kitchen area, in Celsius",
     ylab="Energy use of applicances, in Wh",col="Dark Green")
dev.off()
#X2~Y
png("Scatterplot2.png",width=677,height=400)
plot(V6~V2, data = my.data,frame=FALSE,
     main="Relationship between Energy use and Humidity in kitchen area",
     xlab="Humidity in kitchen area, in Percentage",
     ylab="Energy use of applicances, in Wh",col="Dark Blue")
dev.off()
#X3~Y
png("Scatterplot3.png",width=677,height=400)
plot(V6~V3, data = my.data,frame=FALSE,
     main="Relationship between Energy use and Temperature outside",
     xlab="Temperature outside, in Celsius",
     ylab="Energy use of applicances, in Wh",col="Dark Orange")
dev.off()
#X4~Y
png("Scatterplot4.png",width=677,height=400)
plot(V6~V4, data = my.data,frame=FALSE,
     main="Relationship between Energy use and Humidity outside",
     xlab="Humidity outside, in Percentage",
     ylab="Energy use of applicances, in Wh",col="Black")
dev.off()
#X5~Y
png("Scatterplot5.png",width=677,height=400)
plot(V6~V5, data = my.data,frame=FALSE,
     main="Relationship between Energy use and Visibility",
     xlab="Visibility, in Km",
     ylab="Energy use of applicances, in Wh",col="Brown")
dev.off()

##Create 6 Histograms
#X1
png("Hist1.png",width=677,height=400)
hist(Energy20$V1,
     main="Histogram of Temperature in Kitchen area",
     xlab="Temperature in Kitchen area, in Celsius",
     ylab="Frequency",col="dark green")
dev.off()
#X2
png("Hist2.png",width=677,height=400)
hist(Energy20$V2,
     main="Histogram of Humidity in Kitchen area",
     xlab="Humidity in Kitchen area, in Percentage",
     ylab="Frequency",col="orange")
dev.off()
#X3
png("Hist3.png",width=677,height=400)
hist(Energy20$V3,
     main="Histogram of Temperature outside",
     xlab="Temperature outside, in Celsius",
     ylab="Frequency",col="dark blue")
dev.off()
#X4
png("Hist4.png",width=677,height=400)
hist(Energy20$V4,
     main="Histogram of Humidity outside",
     xlab="Humidity outside, in Percentage",
     ylab="Frequency",col="Dark Grey")
dev.off()
#X5
png("Hist5.png",width=677,height=400)
hist(Energy20$V5,
     main="Histogram of Visibility",
     xlab="Visibility, in Km",
     ylab="Frequency",col="Dark Red")
dev.off()
#Y
png("Hist6.png",width=677,height=400)
hist(Energy20$V6,
     main="Histogram of Energy use of applicances",
     xlab="Energy use of applicances, in Wh",
     ylab="Frequency",col="Yellow")
dev.off()

###Question 2
##Data transformation
#Select any 4 variables from 5 variables (X1,X2,X3 and X4 are chosen)

#Transform X1 (Scaling)
V1<-(my.data$V1-min(my.data$V1))/(max(my.data$V1)-min(my.data$V1))

#Transform X2 (Scaling)
V2<-(my.data$V2-min(my.data$V2))/(max(my.data$V2)-min(my.data$V2))

#Transform X3 (Scaling)
V3<-(my.data$V3-min(my.data$V3))/(max(my.data$V3)-min(my.data$V3))

#Transform X4 (Scaling)
V4<-(my.data$V4-min(my.data$V4))/(max(my.data$V4)-min(my.data$V4))

#Transfrom Y
#Apply log transformation
V6=log(my.data$V6)
#Apply feature scaling
V6<-(V6-min(V6))/(max(V6)-min(V6))

##Save data file
transformed.data=cbind(V1,V2,V3,V4,V6)
write.table(transformed.data,
            file="/Volumes/data/Deakin/Real World Analytics/BUI-transformed.txt",row.names=F)
detach(my.data)
attach(transformed.data)

###Question 3
##Add required file to working dicrectory
install.packages("lpSolve")
source('/Volumes/data/Deakin/Real World Analytics/SIT718 T3 2019 Task 3 Data Script/AggWaFit718.R')
##Use the fitting functions to learn the parameters
#Weighted Average Mean (WAM)
fit.QAM(transformed.data,output.1="WAMoutput.txt",stats.1 = "WAMstats.txt")
#Weighted Power Mean (WPM), p=0.5
fit.QAM(transformed.data,output.1="PMoutput.txt",stats.1 = "PMstats.txt",
        g=PM05,g.inv = invPM05)
#Weighted Power Mean (WPM), p=2
fit.QAM(transformed.data,output.1="QMoutput.txt",stats.1 = "QMstats.txt",
        g=QM,g.inv = invQM)
#Ordered Weighted Average (OWA)
fit.OWA(transformed.data,output.1="OWAoutput.txt",stats.1 = "OWAstats.txt")
#Choquet Integral
fit.choquet(transformed.data,output.1="Choquetoutput.txt",
            stats.1 = "Choquetstats.txt")

###Question 4
##Use my model for prediction
#Transform inputs
Transformed.X1 = (18-min(my.data$V1))/(max(my.data$V1)-min(my.data$V1))
Transformed.X1

Transformed.X2 = (44-min(my.data$V2))/(max(my.data$V2)-min(my.data$V2))
Transformed.X2

Transformed.X3 = (4-min(my.data$V3))/(max(my.data$V3)-min(my.data$V3))
Transformed.X3

Transformed.X4 = (74.8-min(my.data$V4))/(max(my.data$V4)-min(my.data$V4))
Transformed.X4

#Use the Choquet Integral model
values = c(Transformed.X1,Transformed.X2,Transformed.X3,Transformed.X4)
choquetweights = c(0.0754288267610806,
                   0.164398750252198,
                   0.44639019335741,
                   0.560107752700718,
                   0.984208433626041,
                   0.560107752700715,
                   0.984208433626038,
                   0.337713790443329,
                   0.596176352073788,
                   0.337713790443328,
                   0.596176352073788,
                   0.790304307043272,
                   1,
                   0.790304307043271,
                   0.999999999999999)
result = choquet(values,choquetweights)
result

#Reserve the transformation to get the predicted Y value
Predicted_V6 = (result*(max(V6)-min(V6)))+min(V6)
Predicted_V6

###Question 5
##Linear regression model
transformed <- data.frame(transformed.data)
model=lm(V6~V1+V2+V3+V4,data=transformed)
summary(model)

##Predict Y value with transformed values of X1->X4 in Q4
Predicted_Y=-0.008439 + 0.273266*Transformed.X1+(-0.056620*Transformed.X2)+0.569366*Transformed.X3+0.284949*Transformed.X4+0.1561
Predicted_Y

##Predict Y values with Choquet Integral
choquet_values = read.delim("choquetoutput.txt",sep="",col.names = c("V1","V2","V3","V4","V5","V6"))
print(choquet_values)

##Predict Y values with Linear Regression
for(i in 1:300){lr_values = ((-0.008439) + 0.273266*V1 + (-0.056620)*V2 + 0.569366*V3 +0.284949*V4 + 0.1561)}
print(lr_values)

##Visualize the predicted Y values of both models with Transformed data
plot(data=choquet_values,transformed.data$V6,lr_values,main="Choquet values vs Regression values vs Transformed values",
     xlab="Energy of appliances in Unit Interval",col=c("orange","dark green","dark blue"))


