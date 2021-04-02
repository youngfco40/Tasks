setwd('~/Desktop/Evolution/Tasks/Project/Data')
ProjectData <- read.csv('Datasetforproject.csv', stringsAsFactors = F)
head(ProjectData)

Scores <- tapply(ProjectData$Front_muscularity, ProjectData$Front_muscularity, length)

par(mgp=c(2.5, 0.5, 0), las=1, tck=-0.01)
barplot(Scores, col='white', border='black', ylab="frequency", xlab="front muscularity score")


