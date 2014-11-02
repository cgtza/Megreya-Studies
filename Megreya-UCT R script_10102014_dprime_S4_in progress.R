library(dplyr)
library(reshape)

substrRight <- function(x, startposition, strlength){
  if(missing(strlength)){
    strlength<-nchar(x)
  }
  substr(x, nchar(x) - startposition +1, nchar(x) - (startposition-strlength))
}

substrLeft <-function(x, startposition, strlength){
  if(missing(startposition)){
    startposition<-1
  }
  substr(x, 0+startposition, strlength)
}

dprime <- function(hit,fa, total) {
  if (hit == 1.00) {
    hit = (total - 0.5/total)/total
  }
  if (fa == 1.00) {
    fa = (total - 0.5/total)/total
  }
  
  if (hit == 0){ 
    hit = 0.5/total}
  
  if (fa == 0 ){
    fa = 0.5/total
  } 
  
  qnorm(hit) - qnorm(fa)
}

beta <- function(hit,fa,total) {
  if (hit == 1.00) {
    hit = (total - 0.5/total)/total
  }
  if (fa == 1.00) {
    fa = (total - 0.5/total)/total
  }
  
  if (hit == 0){ 
    hit = 0.5/total}
  
  if (fa == 0 ){
    fa = 0.5/total
  } 
  zhr <- qnorm(hit)
  zfar <- qnorm(fa)
  exp(-zhr*zhr/2+zfar*zfar/2)
}

RSAMegdata_S4<-read.csv("Solomon4.csv",header=T,row.names=NULL)

#Creates new  variable 'Accuracy' that lists the accuracy for Matching, Recognition and Forced Choice, rather than having to reference three separate columns (MatchingLineupAcc, RecLineupAcc and ForcedChoiceLineupACC) for accuracy
RSAMegdata_S4$Accuracy [RSAMegdata_S4$Procedure == "MatchProc"] <- NA
RSAMegdata_S4$Accuracy [RSAMegdata_S4$RecLineupACC == 1 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 1
RSAMegdata_S4$Accuracy [RSAMegdata_S4$RecLineupACC == 0 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 0
RSAMegdata_S4$Accuracy [RSAMegdata_S4$ForcedChoiceLineupACC == 1 & RSAMegdata_S4$Procedure == "FCProc"] <- 1
RSAMegdata_S4$Accuracy [RSAMegdata_S4$ForcedChoiceLineupACC == 0 & RSAMegdata_S4$Procedure == "FCProc"] <- 0
str(RSAMegdata_S4$Accuracy)
RSAMegdata_S4$Accuracy

#Creates new  variable 'ForcedChoiceAccuracy' that lists the accuracy for Matching, and Total Accuracy for Recognition (this includes RecognitionACC, and ForcedChoice data that is correct)  rather than having to reference three separate columns (MatchingLineupAcc, RecLineupAcc and ForcedChoiceLineupACC) for accuracy
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$Procedure == "MatchProc"] <- NA
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$RecTPTA == "TA" & RSAMegdata_S4$Accuracy == 1 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 1
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$RecTPTA == "TA" & RSAMegdata_S4$Accuracy == 0 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 0
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$RecTPTA == "TP" & RSAMegdata_S4$Accuracy == 1 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 1
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$RecTPTA == "TP" & RSAMegdata_S4$Accuracy == 0 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- 0
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$Accuracy == 0 & RSAMegdata_S4$Procedure == "FCProc"] <- 0
RSAMegdata_S4$ForcedChoiceAccuracy [RSAMegdata_S4$Accuracy == 1 & RSAMegdata_S4$Procedure == "FCProc"] <- 1

#Creates new variable 'TargetPresence'
RSAMegdata_S4$TargetPresence [RSAMegdata_S4$Procedure == "MatchProc"] <- "NA"
RSAMegdata_S4$TargetPresence [RSAMegdata_S4$Procedure == "RecognitionProc"] <- as.character(RSAMegdata_S4$RecTPTA[which(RSAMegdata_S4$Procedure == "RecognitionProc")])
RSAMegdata_S4$TargetPresence [RSAMegdata_S4$Procedure == "FCProc"] <- "TP"
RSAMegdata_S4$TargetPresence

#Creates new variable 'LineupView'
RSAMegdata_S4$LineupView [RSAMegdata_S4$Procedure == "MatchProc"] <- "NA"
RSAMegdata_S4$LineupView [RSAMegdata_S4$Procedure == "RecognitionProc"] <-as.character(RSAMegdata_S4$RecView[which(RSAMegdata_S4$Procedure == "RecognitionProc")])
RSAMegdata_S4$LineupView [RSAMegdata_S4$Procedure == "FCProc"] <-as.character(RSAMegdata_S4$View[which(RSAMegdata_S4$Procedure == "FCProc")])
RSAMegdata_S4$LineupView

#Creates new Variable that lists experiement procedure with a combined recognition procedure (forced choice + recognition)

RSAMegdata_S4$ExpProc [RSAMegdata_S4$Procedure == "MatchProc"] <- "Matching"
RSAMegdata_S4$ExpProc [RSAMegdata_S4$RecTPTA == "TA" & RSAMegdata_S4$RecLineupACC == 1 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata_S4$ExpProc [RSAMegdata_S4$RecTPTA == "TA" & RSAMegdata_S4$RecLineupACC == 0 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata_S4$ExpProc [RSAMegdata_S4$RecTPTA == "TP" & RSAMegdata_S4$RecLineupACC == 1 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata_S4$ExpProc [RSAMegdata_S4$RecTPTA == "TP" & RSAMegdata_S4$RecLineupACC == 0 & RSAMegdata_S4$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata_S4$ExpProc [RSAMegdata_S4$Procedure == "FCProc"] <- "Total Recognition Accuracy"
#RSAMegdata_S4$ExpProc [RSAMegdata_S4$ForcedChoiceLineupACC == 1 & RSAMegdata_S4$Procedure == "FCProc"] <- "Total Recognition Accuracy"

RSAMegdata_S4$Hits<- c("")
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "MatchProc"] <- "NA"
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TP" & RSAMegdata_S4$Accuracy == 1] <-1
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TP" & RSAMegdata_S4$Accuracy != 1] <-0
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "FCProc" & RSAMegdata_S4$Accuracy == 1] <- 1
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "FCProc" & RSAMegdata_S4$Accuracy != 1] <- 0
#
RSAMegdata_S4$FalseAlarms<- c("")
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "MatchProc"]<- "NA"
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TP" & RSAMegdata_S4$Accuracy == 1] <-0
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TP" & RSAMegdata_S4$Accuracy != 1] <-0
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "FCProc" & RSAMegdata_S4$Accuracy == 1] <- 0
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "FCProc" & RSAMegdata_S4$Accuracy != 1] <- 0
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TA" & RSAMegdata_S4$Accuracy == 0] <-1
RSAMegdata_S4$FalseAlarms [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TA" & RSAMegdata_S4$Accuracy != 0] <-0
#
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TA" & RSAMegdata_S4$Accuracy == 0] <-0
RSAMegdata_S4$Hits [RSAMegdata_S4$Procedure == "RecognitionProc" & RSAMegdata_S4$TargetPresence == "TA" & RSAMegdata_S4$Accuracy != 0] <-0

RSAMegdata_S4<-RSAMegdata_S4[RSAMegdata_S4$Procedure !="MatchProc",] 

RSAMegdata_S4$Hits <- as.numeric(RSAMegdata_S4_3$Hits)
RSAMegdata_S4$FalseAlarms <- as.numeric(RSAMegdata_S4_3$FalseAlarms)

#DPrime and Beta

# Creating a dataframe for Forced choice data
D_forcedchoice_S4 <- grouped_df(RSAMegdata_S4, list("Subject", "Ethnicity", "StimGroup", "LineupView", "ExpProc"), drop = TRUE)
# Summarising this Forced Choice data, and including d'prime, and beta to the frame
D_FC_S4<- summarise(D_forcedchoice_S4, sum(Hits), sum(FalseAlarms), dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4), beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_FC_S4<-rename(D_FC_S4, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_FC_S4<-rename(D_FC_S4, c("beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'beta'))
D_FC_S4<-D_FC_S4[D_FC_S4$ExpProc != "NA",]
#groupings_FC_S4<-grouped_df(D_FC_S4, list("Ethnicity", "ExpProc", "LineupView", "StimGroup"))
groupings_FC_S4<-grouped_df(D_FC_S4, list("Ethnicity", "ExpProc", "StimGroup"))
Forced_choice_S4<-summarise(groupings_FC_S4, mean_d = mean(dprime),sd_dprime=sd(dprime), beta = mean(beta), sd_beta=sd(beta))
Forced_choice_S4<-Forced_choice_S4[Forced_choice_S4$ExpProc != "FCProc",]
Forced_choice_S4

# Creating a dataframe for Not Forced choice data
D_Notforcedchoice_S4<-NULL
D_Notforcedchoice_S4 <- grouped_df(RSAMegdata_S4, list("Subject", "Ethnicity", "StimGroup", "LineupView", "Procedure"), drop = TRUE)

# Summarising this Forced Choice data, and cluding d' prime and beta to the frae
D_NFC_S4<- summarise(D_Notforcedchoice_S4, sum(Hits), sum(FalseAlarms), dprime=dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4), beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_NFC_S4<-rename(D_NFC_S4, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_NFC_S4<-rename(D_NFC_S4, c("beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'beta'))
#groupings_NFC_S4<-grouped_df(D_NFC_S4, list("Ethnicity", "Procedure", "LineupView", "StimGroup"))
groupings_NFC_S4<-grouped_df(D_NFC_S4, list("Ethnicity", "Procedure", "StimGroup"))
UnForced_Choice_S4<-summarise(groupings_NFC_S4, mean_d = mean(dprime),sd_dprime=sd(dprime), beta = mean(beta), sd_beta=sd(beta))
UnForced_Choice_S4<-UnForced_Choice_S4[UnForced_Choice_S4$Procedure != "FCProc",]
UnForced_Choice_S4


D_S4_forcedchoice<-NULL
D_S4_FC<-NULL
D_S4_forcedchoice <- grouped_df(RSAMegdata_S4_3, list("Subject", "Ethnicity", "StimGroup", "LineupView"), drop = TRUE)
#D_S4_forcedchoice <- na.omit(D_S4_forcedchoice)
D_S4_FC<- summarise(D_S4_forcedchoice , sum(Hits), sum(FalseAlarms), dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_S4_FC<-rename(D_S4_FC, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_S4_FC<-rename(D_S4_FC, c("sum(Hits)" = 'Hits'))
D_S4_FC<-rename(D_S4_FC, c("sum(FalseAlarms)" = 'FalseAlarms'))
D_S4_FC

#Check the above tables against the tables below
#Set parameters
#NotForcedChoice<-aggregate(x = D_NFC$dprime, list(Ethnicity = D_NFC$Ethnicity, StimGroup = D_NFC$StimGroup, Procedure = D_NFC$Procedure, View = D_NFC$LineupView), FUN = "mean")
NotForcedChoice_S4<-aggregate(x = D_NFC_S4$dprime, list(Ethnicity = D_NFC_S4$Ethnicity, StimGroup = D_NFC_S4$StimGroup, Procedure = D_NFC_S4$Procedure), FUN = "mean")
NotForcedChoice_S4<-rename(NotForcedChoice_S4, c("x" ='dprime'))

#Forced choice => D_FC
ForcedChoice_S4<-aggregate(x = D_S4_FC$dprime, list(Ethnicity = D_S4_FC$Ethnicity, StimGroup = D_S4_FC$StimGroup, Procedure = D_S4_FC$ExpProc, View = D_S4_FC$LineupView), FUN = "mean")
ForcedChoice_S4<-rename(ForcedChoice_S4, c("x" ='dprime'))

ForcedChoices_S4<-aggregate(x = D_FC_S4$dprime, list(Ethnicity = D_NFC_S4$Ethnicity, StimGroup = D_NFC_S4$StimGroup, Procedure = D_NFC_S4$Procedure,View = D_NFC_S4$LineupView), FUN = "mean")
ForcedChoices_S4<-rename(ForcedChoices_S4, c("x" ='dprime'))
ggplot(ForcedChoices_S4, aes(x = View, y = dprime, colour = StimGroup, group = StimGroup)) + 
  geom_line()+
  geom_point()+
  facet_grid(Ethnicity~.)




D_NFC_S4<-D_NFC_S4[D_NFC_S4$Procedure != "FCProc",]
NotForcedChoices_S4<-aggregate(x = D_NFC_S4$dprime, list(Ethnicity = D_NFC_S4$Ethnicity, StimGroup = D_NFC_S4$StimGroup, Procedure = D_NFC_S4$Procedure, View = D_NFC_S4$LineupView), FUN = "mean")
NotForcedChoices_S4<-rename(NotForcedChoices_S4, c("x" ='dprime'))
ggplot(NotForcedChoices_S4, aes(x = View, y = dprime, colour = StimGroup, group = StimGroup)) + 
  geom_line()+
  geom_point()+
  facet_grid(Ethnicity~.)

ForcedChoices_S4<-aggregate(x = D_FC_S4$dprime, list(Ethnicity = D_NFC_S4$Ethnicity, StimGroup = D_NFC_S4$StimGroup, Procedure = D_NFC_S4$Procedure, View = D_NFC_S4$LineupView), FUN = "mean")
ForcedChoices_S4<-rename(ForcedChoices_S4, c("x" ='dprime'))
ggplot(ForcedChoices_S4, aes(x = View, y = dprime, colour = StimGroup, group = StimGroup)) + 
  geom_line()+
  geom_point()+
  facet_grid(Ethnicity~.)

RSAMegdata_S4_4<-RSAMegdata_S4_3[RSAMegdata_S4_3$Procedure != "FCProc",]
RSAMegdata_S4_4
RSAMegdata_S4_4<-RSAMegdata_S4_4[RSAMegdata_S4_4$Procedure != "Matching",]
D_S4_Notforcedchoice <- grouped_df(RSAMegdata_S4_4, list("Subject", "Ethnicity", "StimGroup", "LineupView", "Procedure"), drop = TRUE)
D2<-NULL
D_S4_NFC<- summarise(D_S4_Notforcedchoice, sum(Hits), sum(FalseAlarms), dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_S4_NFC<-rename(D_S4_NFC, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_S4_NFC<-rename(D_S4_NFC, c("sum(Hits)" = 'Hits'))
D_S4_NFC<-rename(D_S4_NFC, c("sum(FalseAlarms)" = 'FalseAlarms'))
D_S4_NFC

subset_FC<-subset(D_S4_FC, LineupView == "Threequarter")
#forced choice => D_FC
aggregate(x = subset_FC$dprime, list(Ethnicity = subset_FC$Ethnicity, StimGroup = subset_FC$StimGroup), FUN = "mean")

#not Forced choice => D_NFC
subset_NFC<-subset(D_S4_NFC, LineupView == "Threequarter")
aggregate(x = subset_NFC$dprime, list(Ethnicity = subset_NFC$Ethnicity, StimGroup = subset_NFC$StimGroup), FUN = "mean")

#Crosstab of d'
Dprime_ForcedChoice<-aggregate(x = D_S4_FC$FalseAlarms, list(Ethnicity = D_S4_FC$Ethnicity, StimGroup = D_S4_FC$StimGroup), FUN = "mean")
Dprime_ForcedChoice
Dprime_NotForcedChoice<-aggregate(x = D_S4_NFC$FalseAlarms, list(Ethnicity = D_S4_NFC$Ethnicity, StimGroup = D_S4_NFC$StimGroup), FUN = "mean")
Dprime_NotForcedChoice

#Crosstab of accuracy data
#This table shows you the accuracy for participants across Matching [MatchProc], Recognition [RecognitionProc] and Forced Choice Recognition [FCProc]. However, NOTE that FCProc is the accuracy for the forced choice trials only, and thus does not display overall recognition accuracy.

TotalAccuracydata_S4<- aggregate(x = RSAMegdata_S4$Accuracy, list(StimulusGroup = RSAMegdata_S4$StimGroup, ExperimentStage = RSAMegdata_S4$Procedure, ParticipantRace = RSAMegdata_S4$Ethnicity), FUN = "mean")
#TotalAccuracydata_S4
TotalAccuracydata_S4.NAremoved<-na.omit(TotalAccuracydata_S4)
TotalAccuracydata_S4.NAremoved

TotalAccuracydata_S4<- aggregate(x = RSAMegdata_S4$Accuracy, list(StimulusGroup = RSAMegdata_S4$StimGroup, ExperimentStage = RSAMegdata_S4$Procedure, ParticipantRace = RSAMegdata_S4$Ethnicity), FUN = "su")
#TotalAccuracydata_S4
TotalAccuracydata_S4.NAremoved<-na.omit(TotalAccuracydata_S4)
TotalAccuracydata_S4.NAremoved

TotalAccuracydata_S42<- aggregate(x = RSAMegdata_S4$Accuracy, list(StimulusGroup = RSAMegdata_S4$StimGroup, ExperimentStage = RSAMegdata_S4$Procedure, ParticipantRace = RSAMegdata_S4$Ethnicity, TPTA = RSAMegdata_S4$TargetPresence), FUN = "mean")
#TotalAccuracydata_S4
TotalAccuracydata_S42.NAremoved<-na.omit(TotalAccuracydata_S42)
TotalAccuracydata_S42.NAremoved

#
##This table shows you the accuracy for participants for  Matching [Matching], and gives a combined recognition result, which is a combination of the results from recognition with the correct forced choice results (which replace the incorrect ones from recognition). 
ForcedChoiceAccuracydata_agg_S4<- aggregate(x = RSAMegdata_S4$ForcedChoiceAccuracy, list(StimulusGroup = RSAMegdata_S4$StimGroup,ExperimentStage = RSAMegdata_S4$ExpProc, ParticipantRace = RSAMegdata_S4$Ethnicity), FUN = "mean")
#ForcedChoiceAccuracydata_agg_S4
ForcedChoiceAccuracydata_agg_S4.NAremoved<-na.omit(ForcedChoiceAccuracydata_agg_S4)
ForcedChoiceAccuracydata_agg_S4

ForcedChoiceAccuracydata_agg_S42<- aggregate(x = RSAMegdata_S4$ForcedChoiceAccuracy, list(StimulusGroup = RSAMegdata_S4$StimGroup,ExperimentStage = RSAMegdata_S4$ExpProc, ParticipantRace = RSAMegdata_S4$Ethnicity, TPTA = RSAMegdata_S4$TargetPresence), FUN = "mean")
#ForcedChoiceAccuracydata_agg_S4
ForcedChoiceAccuracydata_agg_S42.NAremoved<-na.omit(ForcedChoiceAccuracydata_agg_S42)
ForcedChoiceAccuracydata_agg_S42.NAremoved