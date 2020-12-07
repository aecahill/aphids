library(ggplot2)

#read in data for fall only
aphidsfall<-read.table("C:/Users/aecsk/OneDrive/Desktop/aphidsfall.txt",header=T)

#calculate stats by region
summary(aov(aphidsfall$AvgAphidsPerLeaf~aphidsfall$Region))

#graph by region
ggplot(aphidsfall, aes(color = Region,y=AvgAphidsPerLeaf,x=Region)) + 
  geom_jitter(position=position_jitter(0.2),cex=3)+
  geom_boxplot(alpha=0.5)+
  scale_color_manual(values=c("gold","purple"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Region", y = "Aphid Density")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#trying with fall only, no zeros

aphidsfallnz<-read.table("C:/Users/aecsk/OneDrive/Desktop/aphidsfallnozero.txt",header=T)

summary(aov(aphidsfallnz$AvgAphidsPerLeaf~aphidsfallnz$Region))

ggplot(aphidsfallnz, aes(color = Region,y=AvgAphidsPerLeaf,x=Region)) + 
  geom_jitter(position=position_jitter(0.2),cex=3)+
  geom_boxplot(alpha=0.5)+
  scale_color_manual(values=c("gold","purple"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Region", y = "Aphid Density")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))

#plotting correlations with height and leaf number

cor.test(aphidsfall$Height_cm,aphidsfall$AvgAphidsPerLeaf)

#and with no zeroes

cor.test(aphidsfallnz$Height_cm,aphidsfallnz$AvgAphidsPerLeaf)
cor.test(aphidsfallnz$NumLeaves,aphidsfallnz$AvgAphidsPerLeaf)

ggplot(aphidsfall,aes(Height_cm,AvgAphidsPerLeaf))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
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
  xlab("\nPlant Height (cm)")+ylab("Aphid Density\n")

ggplot(aphidsfallnz,aes(Height_cm,AvgAphidsPerLeaf))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
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
  xlab("\nPlant Height (cm)")+ylab("Aphid Density\n")

ggplot(aphidsfallnz,aes(NumLeaves,AvgAphidsPerLeaf))+
  geom_point(aes(color=Region), alpha = 0.65, size = 4)+
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
  xlab("\nNumber of Leaves")+ylab("Aphid Density\n")


#analysis with caterpillars

cats<-read.table("C:/Users/aecsk/OneDrive/Desktop/cats.txt",header=T)

t.test(cats$AvgAphidsPerLeaf~cats$NumCaterpillars)

#graph by caterpillars or no
ggplot(cats, aes(color = Region,y=AvgAphidsPerLeaf,x=NumCaterpillars)) + 
  geom_jitter(position=position_jitter(0.2),alpha = 0.75, cex=3)+
  #geom_boxplot(alpha=0.5)+
  scale_color_manual(values=c("gold","purple"))+
  #stat_summary(fun.y=mean, geom="point", shape=18,size=5, color="black")+
  #stat_summary(fun.data=data_summary, color="red", size=1.5)+
  labs(x ="Caterpillars", y = "Aphid Density")+
  theme_bw()+
  #theme(legend.position="none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  theme(axis.text.x = element_text(angle = 65,hjust=1))


##2019 data

aphids2019<-read.table("C:/Users/aecsk/OneDrive/Desktop/aphids2019.txt",header=T)

ggplot(aphids2019,aes(AvgAphidsPerLeaf,NumLeaves,color=Region))+
  geom_point(alpha = 0.65, size = 4)+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("gold"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  theme(legend.position = "none") +
  xlab("\nNumber of Leaves")+ylab("Aphid Density\n")+
  facet_wrap(~ Week)

#no zeros 2019 only

aphids2019nz<-read.table("C:/Users/aecsk/OneDrive/Desktop/aphids2019nz.txt",header=T)

ggplot(aphids2019nz,aes(AvgAphidsPerLeaf,Height_cm,color=Region))+
  geom_point(alpha = 0.65, size = 4)+
  geom_smooth(method='lm',se=F)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("gold"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  theme(legend.position = "none") +
  xlab("\nPlant Height (cm)")+ylab("Aphid Density\n")+
  facet_wrap(~ Week)
