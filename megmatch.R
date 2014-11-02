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

#   Reverse lineupaccsum to reflect FAs    
x<-filter(MegMatchbySubj,MatchTPTA=="TA") 
x$matchHitorFAsum<-4-x$matchHitorFAsum
y<-filter(MegMatchbySubj,MatchTPTA=="TP")
MegMatchbySubj<-rbind(x,y)

#   Compute Z of H and FA, with corrections
MegMatchbySubj$matchHitorFAsum[MegMatchbySubj$matchHitorFAsum==0]<-0.5
MegMatchbySubj$matchHitorFAsum[MegMatchbySubj$matchHitorFAsum==4]<-3.5 
MegMatchbySubj$z_matchHitorFAsum<-qnorm(MegMatchbySubj$matchHitorFAsum/4) 

# recode conditions to make crossover possible
MegMatchbySubj$Stim[MegMatchbySubj$Ethnicity=="Black - South African" & MegMatchbySubj$StimGroup=="OWNG"] <- "BlackSA"
MegMatchbySubj$Stim[MegMatchbySubj$Ethnicity=="Black - South African" & MegMatchbySubj$StimGroup=="OTHG"] <- "WhiteSA"
MegMatchbySubj$Stim[MegMatchbySubj$Ethnicity=="White - South African" & MegMatchbySubj$StimGroup=="OWNG"] <- "WhiteSA"
MegMatchbySubj$Stim[MegMatchbySubj$Ethnicity=="White - South African" & MegMatchbySubj$StimGroup=="OTHG"] <- "BlackSA"


#   Use reshape2 package to melt dataframe, and to cast one for analysis
x<-melt(MegMatchbySubj,id=c("Subject","StimGroup","Ethnicity","MatchView","MatchTPTA","Stim"),
        measure=c("matchHitorFAsum","matchconfmean","matchrtmean","z_matchHitorFAsum"))

#   Compute dprimes
Matchdprimedf<-dcast(x,Subject+Ethnicity+StimGroup+MatchView+Stim~MatchTPTA+variable,value.var="value")
Matchdprimedf$dprime<-Matchdprimedf$TP_z_matchHitorFAsum-Matchdprimedf$TA_z_matchHitorFAsum

#   Summarise dprimes by experimental variables 
dprimedf<-Matchdprimedf %>%
  group_by(StimGroup,Ethnicity,MatchView,Stim) %>%
  summarise(mean_dprime=mean(dprime,na.rm=T),
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

# Write wide dataframe for merging later
  x<-melt(Matchdprimedf,id=c("Subject","StimGroup","Ethnicity","MatchView","Stim"))
  matchwidedf<-dcast(x,Subject~StimGroup+Ethnicity+MatchView+Stim+variable,value.var="value")


