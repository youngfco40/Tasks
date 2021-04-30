setwd('~/Desktop/Evolution/Tasks/Task_10')
install.packages("diversitree")
library(diversitree)

transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1
speciation_1 <- 0.2
extinction_1 <- 0.1

maxN <- 1e3
maxT <- 50
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)

stateTable <- table(simTree$tip.state)
stateTable
stateTable/sum(stateTable)

transition_0to1 <-0.1
transition_1to0 <- 0.1
speciation_state0 <- 0.1
extinction_state0 <- 0.1
speciation_state1 <- 0.2
extinction_state1 <- 0.1
maxN <-1e3
maxT <-50
Parstest1 <- c(speciation_state0, speciation_state1, extinction_state0, extinction_state1,transition_0to1, transition_1to0)
simTreetest1 <- tree.bisse(Parstest1, max.taxa = maxN, max.t = maxT)
stateTable1 <- table(simTreetest1$tip.state)
stateTable1
stateTable1/sum(stateTable1)

#frequency of state 1 not higher than state 0 when net diversification is lower

transition_0to1 <-0.0
transition_1to0 <- 0.0
speciation_state0 <- 0.1
extinction_state0 <- 0.1
speciation_state1 <- 0.2
extinction_state1 <- 0.1
maxN <-1e3
maxT <-50
Parstest1 <- c(speciation_state0, speciation_state1, extinction_state0, extinction_state1,transition_0to1, transition_1to0)
simTreetest1 <- tree.bisse(Parstest1, max.taxa = maxN, max.t = maxT)
stateTable1 <- table(simTreetest1$tip.state)
stateTable1
stateTable1/sum(stateTable1)

#Transition states can never be , so frequency will always be higher than 0 itself

transition_0to1 <-0.1
transition_1to0 <- 0.1
speciation_state0 <- 0.1
extinction_state0 <- 0.1
speciation_state1 <- 0.1
extinction_state1 <- 0.1
maxN <-1e3
maxT <-50
Parstest1 <- c(speciation_state0, speciation_state1, extinction_state0, extinction_state1,transition_0to1, transition_1to0)
simTreetest1 <- tree.bisse(Parstest1, max.taxa = maxN, max.t = maxT)
stateTable1 <- table(simTreetest1$tip.state)
stateTable1
stateTable1/sum(stateTable1)
#No varaitaion of frequency when parameters are same
plot(stateTable1)

transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_state0 <- 0.3
extinction_state0 <- 0.1
speciation_state1 <- 0.1
extinction_state1 <- 0.1
maxN <-1e3
maxT <-50
Parstest1 <- c(speciation_state0, speciation_state1, extinction_state0, extinction_state1,transition_0to1, transition_1to0)
simTreetest1 <- tree.bisse(Parstest1, max.taxa = maxN, max.t = maxT)
stateTable1 <- table(simTreetest1$tip.state)
stateTable1
stateTable1/sum(stateTable1)

# extinction parameters cannot be more than 0.1 if you change the the speciaton state 0 parameter for 

?tree.bisse
