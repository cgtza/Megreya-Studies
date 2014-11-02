# Load some useful libraries
library(dplyr);library(reshape2);library(ggplot2)

#Read data
RSAMegdata<-read.csv("DataSet_MR.csv",header=T,row.names=NULL,stringsAsFactors=F)

#   Summarise data down
MegMatchbySubj<-RSAMegdata %>%
  filter(Block<17) %>%
  group_by(Subject,StimGroup,Ethnicity,MatchView,MatchTPTA) %>%
  summarise(matchHitorFAsum=sum(MatchLineupACC,na.rm=T),
            matchconfmean=mean(as.numeric(MatchConfRESP,na.rm=T)),
            matchrtmean=mean(MatchLineupRT,na.rm=T))

MegbySubj<-RSAMegdata %>%
  group_by(Subject,StimGroup,Ethnicity,MatchView,MatchTPTA) %>%
  summarise(matchHitorFAsum=sum(MatchLineupACC,na.rm=T),
            recogHitorFAsum=sum(RecLineupACC,na.rm=T),
            matchconfmean=mean(as.numeric(MatchConfRESP,na.rm=T)),
            matchrtmean=mean(MatchLineupRT,na.rm=T),
            recconfmean=mean(as.numeric(RecConfRESP,na.rm=T)),
            recrtmean=mean(RecLineupRT,na.rm=T))


# recode conditions to make crossover possible
MegbySubj$Stim[MegbySubj$Ethnicity=="Black - South African" & MegbySubj$StimGroup=="OWNG"] <- "BlackSA"
MegbySubj$Stim[MegbySubj$Ethnicity=="Black - South African" & MegbySubj$StimGroup=="OTHG"] <- "WhiteSA"
MegbySubj$Stim[MegbySubj$Ethnicity=="White - South African" & MegbySubj$StimGroup=="OWNG"] <- "WhiteSA"
MegbySubj$Stim[MegbySubj$Ethnicity=="White - South African" & MegbySubj$StimGroup=="OTHG"] <- "BlackSA"


#   Reverse lineupaccsum to reflect FAs    
x<-filter(MegMatchbySubj,MatchTPTA=="TA") 
x$matchHitorFAsum<-4-x$matchHitorFAsum
x$recogHitorFAsum<-4-x$recogHitorFAsum
y<-filter(MegMatchbySubj,MatchTPTA=="TP")
MegMatchbySubj<-rbind(x,y)


#   Reverse lineupaccsum to reflect FAs    
x<-filter(MegbySubj,MatchTPTA=="TA") 
x$matchHitorFAsum<-4-x$matchHitorFAsum
y<-filter(MegbySubj,MatchTPTA=="TP")
MegbySubj<-rbind(x,y)

#   Compute Z of H and FA, with corrections
MegMatchbySubj$matchHitorFAsum[MegMatchbySubj$matchHitorFAsum==0]<-0.5
MegMatchbySubj$matchHitorFAsum[MegMatchbySubj$matchHitorFAsum==4]<-3.5 
MegMatchbySubj$z_matchHitorFAsum<-qnorm(MegMatchbySubj$matchHitorFAsum/4) 


#   Compute Z of H and FA, with corrections
MegbySubj$matchHitorFAsum[MegbySubj$matchHitorFAsum==0]<-0.5
MegbySubj$matchHitorFAsum[MegbySubj$matchHitorFAsum==4]<-3.5 
MegbySubj$z_matchHitorFAsum<-qnorm(MegbySubj$matchHitorFAsum/4)
MegbySubj$recogHitorFAsum[MegbySubj$matchHitorFAsum==0]<-0.5
MegbySubj$matchHitorFAsum[MegbySubj$matchHitorFAsum==4]<-3.5 
MegbySubj$z_matchHitorFAsum<-qnorm(MegbySubj$matchHitorFAsum/4)





#   Use reshape2 package to melt dataframe, and to cast one for analysis
x<-melt(MegMatchbySubj,id=c("Subject","StimGroup","Ethnicity","MatchView","MatchTPTA","Stim"),
        measure=c("matchHitorFAsum","matchconfmean","matchrtmean","z_matchHitorFAsum"))


#   Use reshape2 package to melt dataframe, and to cast one for analysis
x<-melt(MegbySubj,id=c("Subject","StimGroup","Ethnicity","MatchView","MatchTPTA","Stim"),
        measure=c("matchHitorFAsum","matchconfmean","matchrtmean","z_matchHitorFAsum"))

#   Compute dprimes
Matchdprimedf<-dcast(x,Subject+Ethnicity+StimGroup+MatchView+Stim~MatchTPTA+variable,value.var="value")
Matchdprimedf$dprime<-Matchdprimedf$TP_z_matchHitorFAsum-Matchdprimedf$TA_z_matchHitorFAsum


#   Compute dprimes
alldprimedf<-dcast(x,Subject+Ethnicity+StimGroup+MatchView+Stim~MatchTPTA+variable,value.var="value")
alldprimedf$matchdprime<-dprimedf$TP_z_matchHitorFAsum-dprimedf$TA_z_matchHitorFAsum

#   Summarise dprimes by experimental variables 
dprimedf<-Matchdprimedf %>%
  group_by(StimGroup,Ethnicity,MatchView,Stim) %>%
  summarise(mean_dprime=mean(dprime,na.rm=T),
            sd_dprime=sd(dprime,na.rm=T),
            n_dprime=n(),
            serror_dprime=sd_dprime/n_dprime,
            CI95=serror_dprime*1.96)

#   Summarise dprimes by experimental variables 
bigalldprimedf<-alldprimedf %>%
  group_by(StimGroup,Ethnicity,MatchView,Stim) %>%
  summarise(mean_matchdprime=mean(dprime,na.rm=T),
            sd_dprime=sd(dprime,na.rm=T),
            n_dprime=n(),
            serror_dprime=sd_dprime/n_dprime,
            CI95=serror_dprime*1.96)






# Draw plot to show result of experiment:matching conditions only,error bars =SE
ggplot(dprimedf,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+facet_grid(.~MatchView)+ggtitle("Matching vs recognition cross race experiment: matching+SE")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-serror_dprime, ymax=mean_dprime+serror_dprime), width=.1)


# Draw plot to show result of experiment:matching conditions only,error bars = 95% CI
ggplot(dprimedf,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+facet_grid(.~MatchView)+ggtitle("Matching vs recognition cross race experiment: matching+CI")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-CI95, ymax=mean_dprime+CI95), width=.1)

# Draw plot to show result of experiment:matching conditions only,error bars = 95% CI and collapse view

dprimedfallviews<-Matchdprimedf %>%
  group_by(StimGroup,Ethnicity,Stim) %>%
  summarise(mean_dprime=mean(dprime,na.rm=T),
            sd_dprime=sd(dprime,na.rm=T),
            n_dprime=n(),
            serror_dprime=sd_dprime/n_dprime,
            CI95=serror_dprime*1.96)

ggplot(dprimedfallviews,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+ggtitle("Matching vs recognition cross race experiment:\n matching+CI, collapse view")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-CI95, ymax=mean_dprime+CI95), width=.1)



