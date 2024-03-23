#analyses done Jan 2 2023 including Patrick's 2021 field data

library(ggplot2)

#read in data for fall only
aphidsfall21<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/aphidsfall21.txt",header=T)

#calculate stats by region
summary(aov(aphidsfall21$AvgAphidsPerLeaf~aphidsfall21$Region))

#what about totals, not just leaves
summary(aov(aphidsfall21$total_aphids~aphidsfall21$Region))

#graph by region
ggplot(aphidsfall21, aes(color = Region,y=total_aphids,x=Region)) + 
  geom_jitter(position=position_jitter(0.2),cex=3)+
  #geom_boxplot(alpha=0.5,outlier.shape=NA)+
  scale_color_manual(values=c("#FFC000","#4286BE"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Region", y = "Total aphids")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#trying with fall only, no zeros

aphidsfallnz21<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/aphidsfallnozero21.txt",header=T)

summary(aov(aphidsfallnz21$AvgAphidsPerLeaf~aphidsfallnz21$Region))

#and with total aphid count
summary(aov(aphidsfallnz21$total_aphids~aphidsfallnz21$Region))

ggplot(aphidsfallnz21, aes(color = Region,y=total_aphids,x=Region)) + 
  geom_jitter(position=position_jitter(0.2),cex=3)+
  geom_boxplot(alpha=0.5)+
  scale_color_manual(values=c("gold","purple"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Region", y = "Total aphids")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#plotting correlations with height and leaf number

cor.test(aphidsfall21$Height_cm,aphidsfall21$total_aphids)
cor.test(aphidsfall21$NumLeaves,aphidsfall21$total_aphids)

#and with no zeroes

cor.test(aphidsfallnz21$Height_cm,aphidsfallnz21$total_aphids)
cor.test(aphidsfallnz21$NumLeaves,aphidsfallnz21$total_aphids)

ggplot(aphidsfall21,aes(Height_cm,total_aphids))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
  geom_smooth(method='lm',colour="#364156")+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("#FFC000","#4286BE"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nPlant Height (cm)")+ylab("Total aphids\n")

ggplot(aphidsfallnz21,aes(Height_cm,total_aphids))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
  geom_smooth(method='lm',colour="#364156")+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("#FFC000","#4286BE"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nPlant Height (cm)")+ylab("Total aphids\n")

ggplot(aphidsfallnz21,aes(NumLeaves,total_aphids))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("#FFC000","#4286BE"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nNumber of Leaves")+ylab("Total aphids\n")


#analysis with caterpillars

has_cats<-c()
rownums<-c(1:length(aphidsfallnz21$Region))

for (i in rownums){
  if (aphidsfallnz21$NumCaterpillars[i] == 0) {
    b = "NO"} else {
      b = "YES"
    }
  has_cats<-c(has_cats,b)
}

aphidsfallnz21cats<-cbind(aphidsfallnz21,has_cats)

cor.test(aphidsfallnz21$AvgAphidsPerLeaf,aphidsfallnz21$NumCaterpillars)

#graph by caterpillars or no
ggplot(aphidsfallnz21cats, aes(color = Region,y=total_aphids,x=has_cats)) + 
  geom_jitter(position=position_jitter(0.2),alpha = 0.75, cex=3)+
  #geom_boxplot(alpha=0.5)+
  scale_color_manual(values=c("#FFC000","#4286BE"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Has Caterpillars", y = "Total aphids")+
  theme_bw()+
  #theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

t.test(aphidsfallnz21cats$total_aphids~aphidsfallnz21cats$has_cats)


# experimental results
aphids22<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/aphids22.txt",header=T)

cor.test(aphids22$Aphid_num_end,aphids22$Pod_num_end)
cor.test(aphids22$Aphid_num_end,aphids22$Height)

ggplot(aphids22,aes(Aphid_num_end,Height))+
  geom_point(alpha = 0.65, size = 4,colour="#4286BE",position=position_jitter(width=0.1, height=0))+
  geom_smooth(method='lm',colour="#4286BE")+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nHeight (cm)")+ylab("Total aphids\n")

#adding column to say aphids y/n
has_aphids<-c()
rownums<-c(1:length(aphids22$Plant))

for (i in rownums){
  if (aphids22$Aphid_num_end[i] == 0) {
    b = "NO"} else {
      b = "YES"
    }
  has_aphids<-c(has_aphids,b)
}

aphids22aphids<-cbind(aphids22,has_aphids)

t.test(aphids22aphids$Pod_num_end~aphids22aphids$has_aphids)
t.test(aphids22aphids$Height~aphids22aphids$has_aphids)

ggplot(aphids22aphids,aes(has_aphids,Pod_num_end))+
  #geom_boxplot(colour="#4286BE",outlier.shape=NA)+
  geom_point(alpha = 0.65, size = 4,colour="#4286BE",position=position_jitter(width=0.1, height=0))+
  ylim(0,13)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nAphids present")+ylab("Pod number\n")

ggplot(aphids22aphids,aes(has_aphids,Height))+
  #geom_boxplot(colour="#4286BE", outlier.shape = NA)+
  geom_point(alpha = 0.65, size = 4,colour="#4286BE",position=position_jitter(width=0.1, height=0))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nAphids present")+ylab("Height (cm)\n")

#ok preliminary seed analysis
seeds<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/seeds_prelim.txt",header=T)

t.test(seeds$Seeds_per_pod~seeds$Aphids)
ggplot(seeds,aes(Aphids,Seeds_per_pod))+
  geom_boxplot(colour="purple")+
  geom_point(alpha = 0.65, size = 4,colour="purple",position=position_jitter(width=0.1, height=0))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nAphids present")+ylab("Seeds per pod\n")

# New analyses March 23 2024
# total seed data

seedtot<-read.table("C:/Users/aecsk/Documents/GitHub/aphids/seed_data.txt",header=T)
library(tidyverse)

seedsummary <- seedtot %>%
  group_by(Plant)%>%
  summarize(MeanSeedsPerPod = mean(Num_seeds))

aphids22seeds<-as.data.frame(cbind(aphids22aphids$Plant,aphids22aphids$Location,aphids22aphids$Height,aphids22aphids$Pod_num_end,seedsummary$MeanSeedsPerPod,tapply(seedtot$Num_seeds,seedtot$Plant,sum),aphids22aphids$has_aphids))


colnames(aphids22seeds)<-c("Plant","Location","Height","Pod_num_end","MeanSeedsPerPod","Total_seeds","aphids")

t.test(as.numeric(aphids22seeds$MeanSeedsPerPod)~aphids22seeds$aphids)
t.test(as.numeric(aphids22seeds$Total_seeds)~aphids22seeds$aphids)


ggplot(aphids22seeds,aes(aphids,as.numeric(MeanSeedsPerPod)))+
  #geom_boxplot(colour="#4286BE", outlier.shape = NA)+
  geom_point(alpha = 0.65, size = 4,colour="#4286BE",position=position_jitter(width=0.1, height=0))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nAphids present")+ylab("Mean Seeds Per Pod\n")

ggplot(aphids22seeds,aes(aphids,as.numeric(Total_seeds)))+
  #geom_boxplot(colour="#4286BE", outlier.shape = NA)+
  geom_point(alpha = 0.65, size = 4,colour="#4286BE",position=position_jitter(width=0.1, height=0))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  #scale_colour_manual(values=c("gold","purple"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nAphids present")+ylab("Total Seeds\n")
