library(dplyr);library(reshape2);library(ggplot2)
#Read data
RSAMegdata<-read.csv("DataSet_MR.csv",header=T,row.names=NULL,stringsAsFactors=F)


#   Summarise data down
MegRecbySubj<-RSAMegdata %>%
  filter(Procedure=="RecognitionProc") %>%
  group_by(Subject,StimGroup,Ethnicity,RecView,RecTPTA) %>%
  summarise(RecHitorFAsum=sum(RecLineupACC,na.rm=T),
            Recconfmean=mean(as.numeric(RecConfRESP,na.rm=T)),
            Recrtmean=mean(RecLineupRT,na.rm=T))

#   Reverse lineupaccsum to reflect FAs    
x<-filter(MegRecbySubj,RecTPTA=="TA") 
x$RecHitorFAsum<-4-x$RecHitorFAsum
y<-filter(MegRecbySubj,RecTPTA=="TP")
MegRecbySubj<-rbind(x,y)

#   Compute Z of H and FA, with corrections
MegRecbySubj$RecHitorFAsum[MegRecbySubj$RecHitorFAsum==0]<-0.5
MegRecbySubj$RecHitorFAsum[MegRecbySubj$RecHitorFAsum==4]<-3.5 
MegRecbySubj$z_RecHitorFAsum<-qnorm(MegRecbySubj$RecHitorFAsum/4) 

# recode conditions to make crossover possible
MegRecbySubj$Stim[MegRecbySubj$Ethnicity=="Black - South African" & MegRecbySubj$StimGroup=="OWNG"] <- "BlackSA"
MegRecbySubj$Stim[MegRecbySubj$Ethnicity=="Black - South African" & MegRecbySubj$StimGroup=="OTHG"] <- "WhiteSA"
MegRecbySubj$Stim[MegRecbySubj$Ethnicity=="White - South African" & MegRecbySubj$StimGroup=="OWNG"] <- "WhiteSA"
MegRecbySubj$Stim[MegRecbySubj$Ethnicity=="White - South African" & MegRecbySubj$StimGroup=="OTHG"] <- "BlackSA"


#   Use reshape2 package to melt dataframe, and to cast one for analysis
x<-melt(MegRecbySubj,id=c("Subject","StimGroup","Ethnicity","RecView","RecTPTA","Stim"),
        measure=c("RecHitorFAsum","Recconfmean","Recrtmean","z_RecHitorFAsum"))

#   Compute dprimes
Recdprimedf<-dcast(x,Subject+Ethnicity+StimGroup+RecView+Stim~RecTPTA+variable,value.var="value")
Recdprimedf$dprime<-Recdprimedf$TP_z_RecHitorFAsum-Recdprimedf$TA_z_RecHitorFAsum

#   Summarise dprimes by experimental variables 
dprimedf<-Recdprimedf %>%
  group_by(StimGroup,Ethnicity,RecView,Stim) %>%
  summarise(mean_dprime=mean(dprime,na.rm=T),
            sd_dprime=sd(dprime,na.rm=T),
            n_dprime=n(),
            serror_dprime=sd_dprime/n_dprime,
            CI95=serror_dprime*1.96)

# Draw plot to show result of experiment:Matching conditions only,error bars =SE
ggplot(dprimedf,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+facet_grid(.~RecView)+ggtitle("Matching vs recognition cross race experiment: Recognition+SE")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-serror_dprime, ymax=mean_dprime+serror_dprime), width=.1)


# Draw plot to show result of experiment:Matching conditions only,error bars = 95% CI
ggplot(dprimedf,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+facet_grid(.~RecView)+ggtitle("Matching vs recognition cross race experiment: Recognition+CI")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-CI95, ymax=mean_dprime+CI95), width=.1)

# Draw plot to show result of experiment:Matching conditions only,error bars = 95% CI and collapse view

dprimedfallviews<-Recdprimedf %>%
  group_by(StimGroup,Ethnicity,Stim) %>%
  summarise(mean_dprime=mean(dprime,na.rm=T),
            sd_dprime=sd(dprime,na.rm=T),
            n_dprime=n(),
            serror_dprime=sd_dprime/n_dprime,
            CI95=serror_dprime*1.96)


ggplot(dprimedfallviews,aes(x=Stim,y=mean_dprime,colour=Ethnicity,group=Ethnicity))+
  geom_line()+ggtitle("Matching vs recognition cross race experiment:\n Recognition+CI, collapse view")+
  geom_point(size=3, shape=21, fill="white")+theme(plot.title = element_text(vjust = 1))+
  geom_errorbar(aes(ymin=mean_dprime-CI95, ymax=mean_dprime+CI95), width=.1)

# Write wide dataframe for merging later
x<-melt(Recdprimedf,id=c("Subject","StimGroup","Ethnicity","RecView","Stim"))
Recwidedf<-dcast(x,Subject~StimGroup+Ethnicity+RecView+Stim+variable,value.var="value")

