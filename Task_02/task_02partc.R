Data <- read.csv('beren_new.csv', nrows = 18)
head(Data)


weight <-which(Data$event == 'trait_mass')
head(weight)
dayID <- apply(Data,1, function(x) paste (x[1:3], collapse = '='))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'trait_mass')]
head(Data$age)
beren2 <-Data
beren3 <-beren2[order(beren2$age) ,]
head(Data)
?plot
numWeight <- tapply(beren3$value[weight], beren3$age[weight], length)
totalWeight <-tapply(beren3$value[weight], beren3$age[weight], sum)
pdf("weightovertime.pdf", height = 5, width =5)
plot(as.numeric(names(totalWeight)), totalWeight, type = "l", main = "weight over time", pch = 5, xlab = "age in days", ylab = "weight in kg")
dev.off()