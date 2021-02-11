trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
head(Sample1)
head(Sample2)

#samples were not that different because the means are pretty close to each other. The population of 2 did not seem to have much range as population 1.

boxplot(Sample1, Sample2)

source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

Focus <- makeBaby(Brenda, Alan)
#The number should be 0.5 because youre taking half the loci from mom 
ToMom <- length(grep("mom", Focus))/length(Focus)
#I believe the shares would stil be close to the same since you still take half from each grandparent as well
ToMomMom <-length(grep("grandma_mom", Focus))/length(Focus)
head(ToMomMom)
ToMomDad <-length(grep("grandpa_mom", Focus))/length(Focus)
head(ToMomDad)
#The expected numbers were not as expected. The Focus from grandpa_mom was way less. Maybe this is because the focus was random and more was from the grandma_mom.
Sibling_01 <- makeBaby(Brenda, Alan)
#The DNA shared with siblings should be close to the same, however siblings can have totally different genes from the same parents if different alleles are passed down.
ToSib <-length(intersect(Focus, Sibling_01))/length(Focus)
head(ToSib)
ManySiblings <- replicate(1e3,length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
head(ManySiblings)
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main = "", xlab = "proportion shared genes")

#Random assorment can determine how often the frequency of the alleles get distributed, therefore there is wide range if how often the allele appears.

HWE <- function(p) {
	aa <- p^2
	ab <- 2 * p * (1-p)
	bb <- (1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)

plot(1,1, type = "n", xlim = c(0,1), ylim=c(0,1), xlab = "freq.allele a", ylab = "geno.freq")

p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))

lines(p, GenoFreq[,"aa"], lwd = 2, col = "red")

#The frequency of the allele stays in trend with the predicted outcomes. It seems to have less frequency as it increases and more when it decreases. Time is not shown on this plot exactly, rather than just as the allele changes the frequency changes. 

lines(p, GenoFreq[,"ab"], lwd=2, col = "purple")
lines(p, GenoFreq[, "bb"], lwd=2, col = "blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")

Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")

#Yes it matches the HWE because there is not any stray points as it stays close to within the expected frequencies of the trend line. 

Pop <-simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")

#There is a different kind of point plotted along the line. This is because of the different population number given.

install.packages("learnPopGen")
library(learnPopGen)

x <-genetic.drift(Ne =100, nrep =5, pause= 0.01)

head(x)

PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
Line2 <- lm(tExt ~Samples + 0)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
abline(Line2)

#Adding the +0 line made it start slightly above 0 on the y-axis. For both lines though, the points drift farther away from it as the number of samples increase. 

