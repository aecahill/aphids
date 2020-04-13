#script to analyze aphid data

library(ape)
library(pegas)

#read in fasta

aphids<- read.FASTA("C:/Users/aecsk/Desktop/aphids.fasta")

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
plot(aphidsNet, size = attr(aphidsNet, "freq"), fast = F, show.mutation=0)

#now get colors on the piechart
aphidhaps<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/aphidhaps2020.txt",header=TRUE)
aphidmatrix<-as.matrix(aphidhaps)
aphidmatrix2<-aphidmatrix[,2:5]

plot(aphidsNet, size=attr(aphidsNet, "freq"),fast=FALSE, show.mutation=0, scale.ratio = 0.25, cex = 0.5, pie=aphidmatrix2)

legend("bottomleft", c("Pennsylvania","Albion","South Haven","Saut Ste Marie"), text.col=c("red","cyan","green","black"))

