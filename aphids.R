#script to analyze aphid data

library(ape)
library(pegas)

#read in fasta

aphids<- read.FASTA("C:/Users/acahill/Desktop/aphidstrimmed.fasta")

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
