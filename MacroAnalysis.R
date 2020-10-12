library(car)
library(lattice)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

data <- read.csv("MacroForR.csv",head=T)
str(data)

####AFDW
shapiro.test(data$afdw)
shapiro.test(log(data$afdw))
  #suggests that it is not normally distributed unless log transformed

bwplot(data$afdw)
bwplot(log(data$afdw))

t.test(log(data$afdw) ~ data$Type, paired=T, alternative="two.sided")
exp(0.5591117)

####Chl a
shapiro.test(data$chl_a)
shapiro.test(log(data$chl_a))
  #suggests that it is not normally distributed unless log transformed

bwplot(data$chl_a)
bwplot(log(data$chl_a))
  ##Still outliers after log transformation

a.test<-wilcox.test(data$chl_a[c(1:20)],data$chl_a[c(21:40)],paired=T,alternative="two.sided")
zstat <-  qnorm(a.test$p.value/2)

####Chl b
shapiro.test(data$chl_b)
shapiro.test(log(data$chl_b))
  #just about

bwplot(data$chl_b)
bwplot(log(data$chl_b))
  ##Still outliers after log transformation

b.test<-wilcox.test(data$chl_b[c(1:20)],data$chl_b[c(21:40)],paired=T,alternative="two.sided")
zstat <- qnorm(b.test$p.value/2)

####Chl c
shapiro.test(data$chl_c)
shapiro.test(log(data$chl_c+1))##Because of a few zeros
  #just about

bwplot(data$chl_c)
bwplot(log(data$chl_c))
  ##No outliers
  ##I'm going to use wilcoxon because of the marginal normality

c.test<-wilcox.test(data$chl_c[c(1:20)],data$chl_c[c(21:40)],paired=T,alternative="two.sided")
zstat <-  qnorm(c.test$p.value/2)
##No diff as expected

t.test(data$chl_c[c(1:20)],data$chl_c[c(21:40)],paired=T,alternative="two.sided")
  ##Doesn't change the broad outcome

####Chl d
shapiro.test(data$chl_d)
shapiro.test(log(data$chl_d))##Because of a few zeros
  #nope

d.test<-wilcox.test(data$chl_d[c(1:20)],data$chl_d[c(21:40)],paired=T,alternative="two.sided")
zstat <-  qnorm(d.test$p.value/2)
  ##No diff as expected

ddply(data, c("Type"),summarise,mean.c=mean(chl_c), mean.d=mean(chl_d))

##PLOTTING
##AFDW
afdw <- data[,c(1:3)]
summ.afdw <- ddply(afdw,c("Type"),summarise,mean=mean(afdw),se=sd(afdw)/sqrt(20))

ggplot(summ.afdw,aes(x=Type,y=mean,fill=Type))+ geom_bar(stat='identity',colour='black',position=position_dodge())+ 
       ylab('Microendolithic Biomass (mg cm-3)')+
       geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9))+
       xlab('')+ scale_fill_manual(values=c('forestgreen','slategrey'))+theme_classic()

##Chlorophyll
chloro <- data[,-c(1,3)]
chloro <- melt(chloro,id.vars="Type",variable.name="Chl",value.name="Conc")
chloro
str(chloro)
summ.chl <- ddply(chloro, c("Type","Chl"), summarise, mean=mean(Conc), se=sd(Conc)/sqrt(20))


ggplot(summ.chl,aes(x=Chl,y=mean,fill=Type))+geom_bar(stat="identity",colour="black",position=position_dodge())+ 
       ylab("Pigment concentration (ug cm-3)")+
       geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=position_dodge(.9))+
       scale_fill_manual(values=c("forestgreen","slategrey"))+theme_classic()


