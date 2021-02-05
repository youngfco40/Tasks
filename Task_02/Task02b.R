setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote = F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]

Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[, 'event'] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data$event == 'bottle')
berenMilk2 <- Data[Feeds,]
head(berenMilk2)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse = '-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv' , quote = F, row.names = FALSE)

read.csv('beren_new.csv')

Feeds <-which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds],sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <-cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", yab = "amount of milk consumed (oz)")
par(las =1, mar=c(5,5,1,1), mgp = c(2, 0.5, 0) , tck = -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days" , ylab = "ounces of milk")
abline(h = mean(totalFeed), lty=2, col = 'red')
pdf("r02-totalMilkBYDay.pdf", height = 4, width =4)
par(las =1, mar =c(5,5,1,1), mgp =c(2, 0.5, 0), tck = -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab = "ounces of milk")
abline(h = mean(totalFeed), lty=2, col = 'red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)