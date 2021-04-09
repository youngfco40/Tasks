setwd('~/Desktop/Evolution/Tasks/Task_09')
library(phytools)

#Question 1-3
trees <- list()
births <-c()
Fractions <-c()

for (i in 1:100) {

	x <- sample(1:100, 1, replace=FALSE)
	y <- sample(1:100, 1, replace=FALSE)
	births[i] = x
	Fractions[i] = y
	trees[[i]]<- pbtree(b=births[i], d= (births[i]*Fractions[i]), n=100)

}

#Question 4:Net diversification rate is 0.0391 vs log of -7.354 to -6.867
library(ape)
library(geiger)

plot(trees[[i]], show.tip.label=FALSE)
ltt.plot(trees[[i]], log="y")
bd.ms(phy=trees[[i]]$phy,time=100, n=100)

log(trees[[i]]$edge.length)

#Question 5:Average branch length is 0.000146 and lower than speciation rate

bd.km(trees[[i]], n=100)
for (i in 1:length(trees[[i]])) {
	
branchLength <-mean(trees[[i]]$edge.length)

}
print(branchLength)

plotTree(trees[[i]], fsize= 1.5,ftype="i", lwd=1.5, bty=1.5)


#Question 6: Relationship is 1
 cor(1:2, 1:2)

#Question 7:
trees[[i]]
Tree <- trees[[branchLength]]
plot(Tree, type = 'b')
rates <- c()
traits <- list()

for (i in 1:100) {
	rates[i] <- sample(1:100, 1, replace = FALSE)
	traits[[i]] <- fastBM(Tree, sig2= rates[i])
	print(rates[i])
	print(traits[[i]])
}

#Question 8:No correlation between rates and mean of traits

traitsMean <- mean(traits[[i]])
traitsMean

ratesMean <- mean(rates[i])

cor(traitsMean, ratesMean)

plot(traitsMean, ratesMean)

#Question 9:No correlation between variance of traits and rates
a <- var(traits[[i]])
b <- var(rates[i])

cor(a,b)
#Question 10: No correlation between 1st trait and 2nd trait elements
traitMat <- cbind(traits[[1]], traits[[4]])
cor(traitMat)

