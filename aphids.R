#script to analyze aphid data

library(ape)
library(pegas)

#read in fasta

aphids<- read.FASTA("C:/Users/acahill/Documents/Github/aphids/aphidsloc.fasta")

#paired distance matrix
dist.dna(aphids)

#make and plot tree
treeaphids<-bionj(dist.dna(aphids))
plot.phylo(treeaphids)

#now this stuff uses pegas

#calculate haplotype diversity
hap.div(aphids)

#get haplotype numbers and frequencies
aphids_haplo<-haplotype(aphids)
aphids_haplo

#make a network
aphidsNet <- haploNet(aphids_haplo)

#plot the network
plot(aphidsNet, size = attr(aphidsNet, "freq"), fast = FALSE)

#now get colors on the piechart
aphidhaps<-read.table("C:/Users/acahill/Documents/GitHub/aphids/aphidhaps.txt",header=TRUE)
aphidmatrix<-as.matrix(aphidhaps)
aphidmatrix2<-aphidmatrix[,2:3]

plot(aphidsNet, size=attr(aphidsNet, "freq"),fast=FALSE, scale.ratio = 2, cex = 0.5, pie=aphidmatrix2)

legend("bottomleft", c("Pennsylvania","Michigan"), text.col=c("red","cyan"))

