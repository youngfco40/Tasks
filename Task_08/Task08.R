setwd('~/Desktop/Evolution/Tasks/Task_08')
library(phytools)

tree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

plot(tree, type ="fan")
tree$tip.label
tree$edge.length
#Question 1: 81 tips and 161 branches present

data <-read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
dim(data)

#Question 2: The data is a list of the snout-vent length of verterbrate. There are 100 rows and 1 column. 
svl <- setNames(data$svl, rownames(data))
svl
?fastAnc()
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
print(Ancestors)
#Question 3: Estimated values range from 3.5-5.0. The CI95 element is confidence interval
#Question 4: The variance is low for ancestral states. CI are always very similar so maybe not many differnces bewteen the ancestros.

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))

fossilData <- data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))

#Question 5:
fossilNodes <- c()
nodeN <- c()
for (i in 1:nrow(fossilData)) {
	Node <-fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
	fossilNodes[i] <-fossilData[i, "svl"]
	nodeN[i] <- Node
}

names(fossilNodes) <-nodeN

nodeN

Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
print(Ancestors_withFossils)

#Question 7: 
x <- fastBM(tree, internal=TRUE)
a <- x[as.character(1:tree$Nnode+Ntip(tree))]
par(mfrow=c(2,1))
plot(a, Ancestors_withFossils$ace)
plot(a, Ancestors$ace)

install.packages("geiger")
install.packages("picante")
library(geiger)
library(picante)

#Question 8
svlData <-read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)

geo=get(data(geospiza))

tmp=treedata(geo$phy, geo$dat)
phy=tmp$phy
dat=tmp$data

#---- STORE RESULTS
brownFit <-  fitContinuous(phy, dat[,"wingL"], SE=NA, control=list(niter=50), ncores=2)

#---- PRINT RESULTS
print(names(brownFit))
print(brownFit)


#---- COMPUTE LIKELIHOOD
flik=brownFit$lik
print(argn(flik))

#---- CREATE a FUNCTION to COMPARE MODELS
fitGeospiza=function(trait=c("wingL","tarsusL","culmenL","beakD","gonysW")){

	trait=match.arg(trait, c("wingL","tarsusL","culmenL","beakD","gonysW"))

	# define set of models to compare
	models=c("BM", "OU", "EB", "white")
	summaries=c("diffusion", "Ornstein-Uhlenbeck", "early burst", "white noise")

	## ESTIMATING measurement error ##
	aic.se=numeric(length(models))
	lnl.se=numeric(length(models))

	for(m in 1:length(models)){
		cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m],
			" with SE *** \n", sep="")
		tmp=fitContinuous(phy,dat[,trait],SE=NA, model=models[m],
                                    bounds=list(SE=c(0,0.5)), ncores=2)
		print(tmp)
		aic.se[m]=tmp$opt$aicc
		lnl.se[m]=tmp$opt$lnL
	}


	## ASSUMING no measurement error ##
	aic=numeric(length(models))
	lnl=numeric(length(models))

	for(m in 1:length(models)){
		cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m],
			 " *** \n", sep="")
		tmp=fitContinuous(phy,dat[,trait],SE=0,model=models[m], ncores=2)
		print(tmp)
		aic[m]=tmp$opt$aicc
		lnl[m]=tmp$opt$lnL
	}

	## COMPARE AIC ##
	names(aic.se)<-names(lnl.se)<-names(aic)<-names(lnl)<-models
	delta_aic<-function(x) x-x[which(x==min(x))]

	# no measurement error
	daic=delta_aic(aic)
	cat("\n\n\n\t\t\t\t*** MODEL COMPARISON: ",trait," *** \n",sep="")
	cat("\tdelta-AIC values for models assuming no measurement error
    \t\t\t\t zero indicates the best model\n\n")
	print(daic, digits=2)

		# measurement error
	daic.se=delta_aic(aic.se)
	cat("\n\n\n\n\t\t\t\t*** MODEL COMPARISON: ",trait," ***\n",sep="")
	cat("\t\t   delta-AIC values for models estimating SE
    \t\t\t\t zero indicates the best model\n\n")
	print(daic.se, digits=2)
	cat("\n\n\n")

	res_aicc=rbind(aic, aic.se, daic, daic.se)
	rownames(res_aicc)=c("AICc","AICc_SE","dAICc", "dAICc_SE")

	return(res_aicc)
}

#---- COMPARE MODELS for WING LENGTH
res=fitGeospiza("wingL")
print(res)


lambda <- fitContinuous(phy, dat, model=c("lambda"))
BM <- fitContinuous(phy, dat, model=c("BM"))
nos <- fitContinuous(phy, dat, model=c("white"))
lambda$opt$aicc
BM$opt$aicc
lambda$opt$aicc
nos$opt$aicc
BM
lambda
nos

?fastAnc
?fitContinuous

#Question 9: best fit model is white.
#Questio 10: FastAnc would tell us the estimated states and if it were correct, it would match this model.



