install.packages("phytools")
install.packages("ape")

library('phytools')
library('ape')

text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)

#Question 1: The shark is more closey related to the gold fish

vert.tree
str(vert.tree)

#Question 2: There are no branch lengths since they are rooted

tree <- read.tree(text="(((A,B), (C,D)),E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge

AnolisTree <-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <=Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs,cex=0.25)

?plot.phylo

#Question 3: plot
plot(AnolisTree, cex=0.25, show.tip.label=FALSE)

#Question 4: plot
plot(AnolisTree, cex=0.25, type="unrooted")

#Question 5: plot
plot(AnolisTree, cex=0.50, tip.color ='red')

#Question 6: shortest living species is Anolis_occultus

edges <- AnolisTree$edge.length
names(edges) <-AnolisTree$tip.label
names(edges) [which(edges == min(edges))]

#Question 7:

drop <- c("occultus")
ii <- sapply(drop, grep,AnolisTree$tip.label)
ii

drop <- AnolisTree$tip.label[ii]
drop
dropsp <- drop.tip(AnolisTree,drop)


#Question 8:
plot(dropsp, cex=0.25)



#Question 9: Over time it is slowly growling each lineage then kind of leveling out in growth. It does not go down however. The slope was higher at the beginning of the linage then probably is the same as the curve levels out. The slope is postive, so it means the lineage is probably constantly growing/evolving over time.
ltt(AnolisTree)
abline(0,1, lwd=2, col='red', lty=2)

#Question 10:
# b/lambda = 0.8031
# d/ mu = 0
?fit.bd
fit.bd(AnolisTree, b= NULL, d= NULL, rho=0.2)