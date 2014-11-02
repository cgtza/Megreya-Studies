#RSA-Megreya Data S4
#packages: reshape
#These two functions allow for the user to read user-defined string lengths from either right or left.

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
#Set your working directory. Mine is set to My Documents.

#setwd()


#First read in the data set for the Complete Megreya set
#Read data
RSAMegdata<-read.csv("DataSet_MR.csv",header=T,row.names=NULL)
RSAMegdata

#Creates new  variable 'Accuracy' that lists the accuracy for Matching, Recognition and Forced Choice, rather than having to reference three separate columns (MatchingLineupAcc, RecLineupAcc and ForcedChoiceLineupACC) for accuracy
RSAMegdata$Accuracy [RSAMegdata$MatchLineupACC == 1 & RSAMegdata$Procedure == "MatchProc"] <- 1
RSAMegdata$Accuracy [RSAMegdata$MatchLineupACC == 0 & RSAMegdata$Procedure == "MatchProc"] <- 0
RSAMegdata$Accuracy [RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- 1
RSAMegdata$Accuracy [RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- 0
RSAMegdata$Accuracy [RSAMegdata$ForcedChoiceLineupACC == 1 & RSAMegdata$Procedure == "FCProc"] <- 1
RSAMegdata$Accuracy [RSAMegdata$ForcedChoiceLineupACC == 0 & RSAMegdata$Procedure == "FCProc"] <- 0

#___________________________________
#Creates new  variable 'ForcedChoiceAccuracy' that lists the accuracy for Matching, and Total Accuracy for Recognition (this includes RecognitionACC, and ForcedChoice data that is correct)  rather than having to reference three separate columns (MatchingLineupAcc, RecLineupAcc and ForcedChoiceLineupACC) for accuracy
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "MatchProc"] <- 1
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$Accuracy == 0 & RSAMegdata$Procedure == "MatchProc"] <- 0
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$RecTPTA == "TA" & RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- 1
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$RecTPTA == "TA" & RSAMegdata$Accuracy == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- 0
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$RecTPTA == "TP" & RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- 1
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$RecTPTA == "TP" & RSAMegdata$Accuracy == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- 0
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$Accuracy == 0 & RSAMegdata$Procedure == "FCProc"] <- 0
RSAMegdata$ForcedChoiceAccuracy [RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "FCProc"] <- 1

#  Note from Colin: why do I get the same table report for Accuracy and ForcedChoiceAccuracy?

#__________________________________________
#Creates new variable called ResponseTypeValue
RSAMegdata$ResponseTypeValue[RSAMegdata$MatchLineupACC == 1 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TP"]<- c("Hit")
RSAMegdata$ResponseTypeValue[RSAMegdata$MatchLineupACC == 0 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TA"]<- c("FA")
RSAMegdata$ResponseTypeValue[RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP"]<- c("Hit")
RSAMegdata$ResponseTypeValue[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TA"]<- c("FA")
RSAMegdata$ResponseTypeValue[(RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP") | (RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "FCProc")]<- c("Hit")
RSAMegdata$ResponseTypeValue[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TA"]<- c("FA")

# Note from Colin - shouldn't we have foilid, Miss, and Correct Rejection also coded here?

#Creates new variable called ResponseType
RSAMegdata$ResponseType<-c("")
RSAMegdata$ResponseType[RSAMegdata$MatchLineupACC == 1 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TP"]<- 1
RSAMegdata$ResponseType[RSAMegdata$MatchLineupACC == 0 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TA"]<- 1
RSAMegdata$ResponseType[RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP"]<- 1
RSAMegdata$ResponseType[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TA"]<- 1
RSAMegdata$ResponseType[(RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP") | (RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "FCProc")]<- 1
RSAMegdata$ResponseType[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TA"]<- 1

#  Ditto from Colin, again - should we not have Miss, FoilId etc. coded, too?


#RSAMegdata$MatchHit[RSAMegdata$MatchLineupACC == 1 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TP"]<- c("Hit")
#RSAMegdata$MatchFA[RSAMegdata$MatchLineupACC == 0 & RSAMegdata$Procedure == "MatchProc" & RSAMegdata$MatchTPTA =="TA"]<- c("FA")
#RSAMegdata$RecHit[RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP"]<- c("Hit")
#RSAMegdata$RecFA[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TA"]<- c("FA")
#RSAMegdata$ForcedChoiceHit[(RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$RecTPTA =="TP") | (RSAMegdata$Accuracy == 1 & RSAMegdata$Procedure == "FCProc")]<- c("Hit")
#RSAMegdata$ForcedChoiceFA[RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$MatchTPTA =="TA"]<- c("FA")

#Removes all spaces from the lineup image names
RSAMegdata$MLineupImage<-gsub(" ","", x=RSAMegdata$MLineupImage, fixed = TRUE)
RSAMegdata$RecLineupImage<-gsub(" ","", x=RSAMegdata$RecLineupImage, fixed = TRUE)
RSAMegdata$RecImage<-gsub(" ","", x=RSAMegdata$RecImage, fixed = TRUE)

#Gets the ID (e.g. BFT1 or WMT3) from the lineup image name
RSAMegdata$LineupID  <- c("")
RSAMegdata$LineupID [which(RSAMegdata$Procedure == "MatchProc")]<- as.character(substrLeft(RSAMegdata$MLineupImage,1,4)[which(RSAMegdata$Procedure == "MatchProc")])
RSAMegdata$LineupID [which(RSAMegdata$Procedure == "RecognitionProc")]<- as.character(substrLeft(RSAMegdata$RecLineupImage,1,4)[which(RSAMegdata$Procedure == "RecognitionProc")])
RSAMegdata$LineupID [which(RSAMegdata$Procedure == "FCProc")]<- as.character(substrLeft(RSAMegdata$RecImage,1,4)[which(RSAMegdata$Procedure == "FCProc")])

#Creates new variable 'TargetPresence'
RSAMegdata$TargetPresence <- c("")
RSAMegdata$TargetPresence [which(RSAMegdata$Procedure == "MatchProc")] <- as.character(RSAMegdata$MatchTPTA[which(RSAMegdata$Procedure == "MatchProc")])
RSAMegdata$TargetPresence [which(RSAMegdata$Procedure == "RecognitionProc")] <- as.character(RSAMegdata$RecTPTA[which(RSAMegdata$Procedure == "RecognitionProc")])
RSAMegdata$TargetPresence [which(RSAMegdata$Procedure == "FCProc")] <- "TP"

#Creates new variable 'LineupView'
RSAMegdata$LineupView<-c("")
RSAMegdata$LineupView [which(RSAMegdata$Procedure == "MatchProc")] <- as.character(RSAMegdata$MatchView[which(RSAMegdata$Procedure == "MatchProc")])
RSAMegdata$LineupView [which(RSAMegdata$Procedure == "RecognitionProc")] <-as.character(RSAMegdata$RecView[which(RSAMegdata$Procedure == "RecognitionProc")])
RSAMegdata$LineupView [which(RSAMegdata$Procedure == "FCProc")] <-as.character(RSAMegdata$View[which(RSAMegdata$Procedure == "FCProc")])

#Creates new Variable that lists experiment procedure with a combined recognition procedure (forced choice + recognition)
RSAMegdata$ExpProc<- c("")
RSAMegdata$ExpProc [RSAMegdata$Procedure == "MatchProc"] <- "Matching"
RSAMegdata$ExpProc [RSAMegdata$Procedure == "FillertaskProc"] <- "Filler"
RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TP" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TP" & RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- "NA"
RSAMegdata$ExpProc [RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc [RSAMegdata$ForcedChoiceLineupACC == 1 & RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"

RSAMegdata$ExpProc2<- c("")
RSAMegdata$ExpProc2 [RSAMegdata$Procedure == "MatchProc"] <- "Matching"
RSAMegdata$ExpProc2 [RSAMegdata$Procedure == "FillertaskProc"] <- "Filler"
RSAMegdata$ExpProc2 [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc2 [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc2 [RSAMegdata$RecTPTA == "TP" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc2 [RSAMegdata$RecTPTA == "TP" & RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc2 [RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"
RSAMegdata$ExpProc2 [RSAMegdata$ForcedChoiceLineupACC == 1 & RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"


RSAMegdata$Hits<- c("")
RSAMegdata$FalseAlarms<- c("")

RSAMegdata$Hits [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 1 ] <- 1
RSAMegdata$Hits [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 0 ] <- 0
RSAMegdata$Hits [RSAMegdata$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata$Hits [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 1] <-1
RSAMegdata$Hits [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 0] <-0
RSAMegdata$Hits [RSAMegdata$Procedure == "FCProc" & RSAMegdata$Accuracy == 1] <- 1
RSAMegdata$Hits [RSAMegdata$Procedure == "FCProc" & RSAMegdata$Accuracy == 0] <- 0
#
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 1 ] <- 0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 0] <- 0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 1] <-0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TP" & RSAMegdata$Accuracy == 0] <-0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "FCProc" & RSAMegdata$Accuracy == 1] <- 0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "FCProc" & RSAMegdata$Accuracy == 0] <- 0

RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 0 ] <- 1
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 1 ] <- 0
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 0] <-1
RSAMegdata$FalseAlarms [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy != 0] <-0
#
RSAMegdata$Hits [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 0 ] <- 0
RSAMegdata$Hits [RSAMegdata$Procedure == "MatchProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 1 ] <- 0
RSAMegdata$Hits [RSAMegdata$Procedure == "FillertaskProc"] <- "NA"
RSAMegdata$Hits [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 0] <-0
RSAMegdata$Hits [RSAMegdata$Procedure == "RecognitionProc" & RSAMegdata$TargetPresence == "TA" & RSAMegdata$Accuracy == 1] <-0


#RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
#RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TA" & RSAMegdata$RecLineupACC == 0 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
#RSAMegdata$ExpProc [RSAMegdata$RecTPTA == "TP" & RSAMegdata$RecLineupACC == 1 & RSAMegdata$Procedure == "RecognitionProc"] <- "Total Recognition Accuracy"
#RSAMegdata$ExpProc [RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"
#RSAMegdata$ExpProc [RSAMegdata$ForcedChoiceLineupACC == 1 & RSAMegdata$Procedure == "FCProc"] <- "Total Recognition Accuracy"

# Creates new variable Confidence
#RSAMegdata$MatchConfRESP<-as.character(RSAMegdata$MatchConfRESP)
#RSAMegdata$Confidence<-c("")
#RSAMegdata$Confidence [RSAMegdata$Procedure == "MatchProc"]<-as.character(RSAMegdata$MatchConfRESP)
#RSAMegdata$Confidence [RSAMegdata$Procedure == "RecognitionProc"]<-as.character(RSAMegdata$RecConfRESP)
#RSAMegdata$Confidence [RSAMegdata$Procedure == "FCProc"]<-as.character(RSAMegdata$ForcedChoiceConfRESP)

# Creates new variable RT
# RSAMegdata$RT [RSAMegdata$Procedure == "MatchProc"]<-as.character(RSAMegdata$MatchLineupRT)
#@RSAMegdata$RT [RSAMegdata$Procedure == "RecognitionProc"]<-as.character(RSAMegdata$RSAMegdata$RecLineupRT)
#RSAMegdata$RT [RSAMegdata$Procedure == "FCProc"]<-as.character(RSAMegdata$ForcedChoiceLineupRT)

#Means for Accuracy and Forced Choice Accuracy
RecAggMeans<-aggregate(x = RSAMegdata$Accuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$Procedure), FUN = "mean")
RecAggMeans<-na.omit(RecAggMeans)
RecAggMeans<-arrange(RecAggMeans,Ethnicity)
RecAggMeans

RecAggSD<-aggregate(x = RSAMegdata$Accuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$Procedure), FUN = "sd")
RecAggSD<-na.omit(RecAggSD)
RecAggSD

ForcedChoiceRecAggMeans<-aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$ExpProc), FUN = "mean")
ForcedChoiceRecAggMeans<-na.omit(ForcedChoiceRecAggMeans)
ForcedChoiceRecAggMeans<-arrange(ForcedChoiceRecAggMeans,Ethnicity)
ForcedChoiceRecAggMeans

ForcedChoiceRecAggSD<-aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$ExpProc), FUN = "sd")
ForcedChoiceRecAggSD<-na.omit(ForcedChoiceRecAggSD)
ForcedChoiceRecAggSD

ForcedChoiceRecAgg2Means<-aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$ExpProc2), FUN = "mean")
ForcedChoiceRecAgg2Means<-na.omit(ForcedChoiceRecAgg2Means)
ForcedChoiceRecAgg2Means

#Aggregate table by TargetID
TargetRecMeans<-aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(Ethnicity = RSAMegdata$Ethnicity, StimGroup = RSAMegdata$StimGroup, Procedure = RSAMegdata$ExpProc2, TPTA=RSAMegdata$TargetPresence ,ID = RSAMegdata$LineupID), FUN = "mean")
TargetRecMeans<-na.omit(TargetRecMeans)
TargetRecMeans
Arranged<-arrange(TargetRecMeans,Procedure, TPTA)

#Crosstab of descriptives
#which(colnames(RSAMegdata)=="Accuracy")
#RSAMegdata2<-as.data.frame(cbind(RSAMegdata[2],RSAMegdata[5],RSAMegdata[12],RSAMegdata[15],RSAMegdata[41],RSAMegdata[58:67]))
#RSAMegdata2<-cbind(RSAMegdata2, temp)
#str(RSAMegdata2$ExpProc)

  # Creating a dataframe with Filler removed.
RSAMegdata3<-RSAMegdata[RSAMegdata$ExpProc != "Filler",]
RSAMegdata3$ResponseType<-as.numeric(RSAMegdata3$ResponseType)
RSAMegdata3$Hits <- as.numeric(RSAMegdata3$Hits)
RSAMegdata3$FalseAlarms <- as.numeric(RSAMegdata3$FalseAlarms)

  #DPrime and Beta

  # Creating a dataframe for Forced choice data
#D_forcedchoice <- grouped_df(RSAMegdata3, list("Subject", "Ethnicity", "StimGroup", "LineupView", "ExpProc"), drop = TRUE)
D_forcedchoice <- grouped_df(RSAMegdata3, list("Subject", "Ethnicity", "StimGroup", "ExpProc"), drop = TRUE)
# Summarising this Forced Choice data, and including d'prime, and beta to the frame
D_FC<- summarise(D_forcedchoice, sum(Hits), sum(FalseAlarms), dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4), beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_FC<-rename(D_FC, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_FC<-rename(D_FC, c("beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'beta'))
D_FC<-D_FC[D_FC$ExpProc != "NA",]
groupings_FC<-grouped_df(D_FC, list("Ethnicity", "ExpProc", "StimGroup"))
#groupings_FC<-grouped_df(D_FC, list("Ethnicity", "ExpProc", "LineupView", "StimGroup"))
Forced_choice<-summarise(groupings_FC, mean = mean(dprime),sd_dprime=sd(dprime), beta = mean(beta), sd_beta=sd(beta))
Forced_choice<-Forced_choice[Forced_choice$ExpProc != "FCProc",]
Forced_choice

  # Creating a dataframe for Not Forced choice data
D_Notforcedchoice<-NULL
#D_Notforcedchoice <- grouped_df(RSAMegdata3, list("Subject", "Ethnicity", "StimGroup", "LineupView", "Procedure"), drop = TRUE)
D_Notforcedchoice <- grouped_df(RSAMegdata3, list("Subject", "Ethnicity", "StimGroup", "Procedure"), drop = TRUE)

  # Summarising this Forced Choice data, and cluding d' prime and beta to the frae
D_NFC<- summarise(D_Notforcedchoice, sum(Hits), sum(FalseAlarms), dprime=dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4), beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_NFC<-rename(D_NFC, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_NFC<-rename(D_NFC, c("beta((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'beta'))
#groupings_NFC<-grouped_df(D_NFC, list("Ethnicity", "Procedure", "LineupView", "StimGroup"))
groupings_NFC<-grouped_df(D_NFC, list("Ethnicity", "Procedure", "StimGroup"))
UnForced_Choice<-summarise(groupings_NFC, mean = mean(dprime),sd_dprime=sd(dprime), beta = mean(beta), sd_beta=sd(beta))
UnForced_Choice<-UnForced_Choice[UnForced_Choice$Procedure != "FCProc",]
UnForced_Choice

#Check the above tables against the tables below
#Set parameters
#NotForcedChoice<-aggregate(x = D_NFC$dprime, list(Ethnicity = D_NFC$Ethnicity, StimGroup = D_NFC$StimGroup, Procedure = D_NFC$Procedure, View = D_NFC$LineupView), FUN = "mean")
NotForcedChoice<-aggregate(x = D_NFC$dprime, list(Ethnicity = D_NFC$Ethnicity, StimGroup = D_NFC$StimGroup, Procedure = D_NFC$Procedure), FUN = "mean")

#Means for D
NotForcedChoicesd<-aggregate(x = D_NFC$dprime, list(Ethnicity = D_NFC$Ethnicity, StimGroup = D_NFC$StimGroup, Procedure = D_NFC$Procedure), FUN = "sd")
NotForcedChoicesd
NotForcedChoice
hist(D_NFC$dprime)

#Forced choice => D_FC
ForcedChoice<-aggregate(x = D_FC$dprime, list(Ethnicity = D_FC$Ethnicity, StimGroup = D_FC$StimGroup, Procedure = D_FC$ExpProc, View = D_FC$LineupView), FUN = "mean")
ForcedChoicesd<-aggregate(x = D_FC$dprime, list(Ethnicity = D_FC$Ethnicity, StimGroup = D_FC$StimGroup, Procedure = D_FC$ExpProc), FUN = "mean")
ForcedChoicesd
#Means for D

qplot(D_FC$Ethnicity, D_FC$dprime, geom="bar", stat = "identity")

NotForcedChoice<-rename(NotForcedChoice, c("x" ='dprime'))

NotForcedChoice_pure<-filter(NotForcedChoice, Procedure !="FCProc")
NotForcedChoice_pure
ggplot(NotForcedChoice_pure, aes(x = Procedure, y = dprime, fill = StimGroup)) + 
  geom_bar(stat = "identity", fill = "red", colour = "black") +
  facet_grid(Ethnicity~View)

ggplot(NotForcedChoice_pure, aes(x = Procedure, y = dprime, colour = StimGroup, group = StimGroup)) + 
  geom_line()+
  geom_point()+
  facet_grid(Ethnicity~.)

#to work on
ggplot(NotForcedChoice_pure, aes(x = Procedure, y = dprime, colour = StimGroup, group = StimGroup, fill = Ethnicity)) + 
  geom_line()+
  geom_point()+
  facet_grid(View~.)
?ggplot

#Creating a wide version of the data set
D2_wide <- reshape(D2, v.names = c("StimGroup", "LineupView",	"Procedure","ResponseTypeValue"	"sum(ResponseType)/4"), idvar = "Subject", timevar = "Block", direction = "wide")

write.xlsx(D2, "c:/Data.xlsx")

RSAMegdata3$row.name<-NULL
num <-which(colnames(RSAMegdata3) =="row.name")
RSAMegdata3<-subset(RSAMegdata3, select = -c(row.names))
RSAMegdata3<-RSAMegdata3[,-(which(colnames(RSAMegdata3) == "DisplayRefreshRate"))]
RSAMegdata3<-RSAMegdata3[,-(which(colnames(RSAMegdata3) =="SessionDate"))]
RSAMegdata3<-RSAMegdata3[,-(which(colnames(RSAMegdata3) =="SessionStartDateTimeUtc"))]
RSAMegdata3<-RSAMegdata3[,-(which(colnames(RSAMegdata3) =="SessionTime"))]
RSAMegdata3<-RSAMegdata3[,-(which(colnames(RSAMegdata3) =="CorrectAnswer"))]
RSAMegdata3$DisplayRefreshRate<-NULL
RSAMegdata3$SessionDate<-NULL
RSAMegdata3$SessionStartDateTimeUtc<-NULL
RSAMegdata3$SessionTime<-NULL
RSAMegdata3$CorrectAnswer<-NULL
D2$StimGroup <- as.character(D2$StimGroup)
str(D2$Subject)
D2<-as.data.frame(D2)
?match


Descriptive_wide <- reshape(RSAMegdata3, v.names = c("Accuracy"), idvar = "Subject", timevar = "Block", direction = "wide")
Descriptive_wide$MatchingOwnGroupHits<-c("")
Descriptive_wide$MatchingOwnGroupHits<-D2[(
  match(Descriptive_wide$Subject, D2$Subject) && 
  which(D2$StimGroup == "OWNG") &&
  which(D2$Procedure == "MatchProc") &&
  which(D2$ResponseTypeValue == "Hit")
), 6]

tmp<-subset(D2, (
  (StimGroup == "OWNG") &&
  (Procedure == "MatchProc") &&
  (ResponseTypeValue == "Hit")))

?subset

Descriptive_wide$TotalOwnGroupHits<-sum(match(Accuracy),colnames(Descriptive_wide) & match(Accuracy),colnames(Descriptive_wide))
Descriptive_wide$Sex<-as.factor(Descriptive_wide$Sex)
table(Descriptive_wide$Sex)
length(Descriptive_wide$Subject)
summary(Descriptive_wide$Age)
sd(Descriptive_wide$Age)
table(Descriptive_wide$Ethnicity)
Descriptive_wide <- NULL

#D'


RSAMegdata$TotalHits[RSAMegdata=="Hits"] <- sum(RSAMegdata$ResponseType[RSAMegdata$ResponseTypeValue=="Hit"])
sum(RSAMegdata$ResponseType[RSAMegdata$ResponseTypeValue=="Hit"])
RSAMegdata$ResponseTypeValue<- as.numeric(RSAMegdata$ResponseTypeValue)
MatchSubset<-subset(RSAMegdata, RSAMegdata$Procedure =="MatchProc" & RSAMegdata$MatchTPTA == "TP")
MatchSubset
Match_wide <- reshape(MatchSubset, v.names = c("ResponseTypeValue","ResponseType"), idvar = "Subject", timevar = "MatchingListSample", direction = "wide")
Match_wide$TotalHits<-sum(match())

str(Match_wide)
Match_wide$TotalMatchingHits<-sum(match("MatchLineupACC", colnames(Match_wide)))
Match_wide$TotalMatchingHits
RSAMegdata$
RSAMegdata$TotalMatchHits <- sum()


#Crosstab of accuracy data
#This table shows you the accuracy for participants across Matching [MatchProc], Recognition [RecognitionProc] and Forced Choice Recognition [FCProc]. However, NOTE that FCProc is the accuracy for the forced choice trials only, and thus does not display overall recognition accuracy.
#Accuracy means for StimulusGroup (OWNG, OTHG) X ExperimentStage(MatchProc, RecognitionProc, FCProc) X ParticipantRace (Black, White)
TotalAccuracydata<- aggregate(x = RSAMegdata$Accuracy, list(StimulusGroup = RSAMegdata$StimGroup, ExperimentStage = RSAMegdata$Procedure, ParticipantRace = RSAMegdata$Ethnicity), FUN = "mean")
TotalAccuracydata
TotalAccuracydata.NAremoved<- na.omit(TotalAccuracydata)
TotalAccuracydata.NAremoved

AccuracyForFaces<-aggregate(x = RSAMegdata$Accuracy, list(StimulusGroup = RSAMegdata$StimGroup, ExperimentStage = RSAMegdata$Procedure, ParticipantRace = RSAMegdata$Ethnicity, TPTA = RSAMegdata$TargetPresence, Lineupview = RSAMegdata$LineupView, FaceID = RSAMegdata$LineupID), FUN = "mean")
AccuracyForFaces
AccuracyForFaces.NAremoved <-na.omit(AccuracyForFaces)
AccuracyForFaces.NAremoved
#
#Accuracy means for StimulusGroup (OWNG, OTHG) X ExperimentStage(MatchProc, RecognitionProc, FCProc) X ParticipantRace (Black, White) X TPTA X View
TotalAccuracydata.5<- aggregate(x = RSAMegdata$Accuracy, list(StimulusGroup = RSAMegdata$StimGroup, ExperimentStage = RSAMegdata$Procedure, ParticipantRace = RSAMegdata$Ethnicity, TPTA = RSAMegdata$TargetPresence, Lineupview = RSAMegdata$LineupView), FUN = "mean")
TotalAccuracydata.5
TotalAccuracydata.5.NAremoved<- na.omit(TotalAccuracydata.5)
TotalAccuracydata.5.NAremoved

#Number of observations for StimulusGroup (OWNG, OTHG) X ExperimentStage(MatchProc, RecognitionProc, FCProc) X ParticipantRace (Black, White) X TPTA
TotalAccuracydata3<- aggregate(x = RSAMegdata$Accuracy, list(StimulusGroup = RSAMegdata$StimGroup, ExperimentStage = RSAMegdata$Procedure, ParticipantRace = RSAMegdata$Ethnicity, TPTA = RSAMegdata$TargetPresence), FUN = "length")
TotalAccuracydata3
TotalAccuracydata3.NAremoved<- na.omit(TotalAccuracydata3)
TotalAccuracydata3.NAremoved

##This table shows you the accuracy for participants for  Matching [Matching], and gives a combined recognition result, which is a combination of the results from recognition with the correct forced choice results (which replace the incorrect ones from recognition). 
#Accuracy means for StimulusGroup (OWNG, OTHG) X ExperimentStage(MatchProc, RecognitionProc, FCProc) X ParticipantRace (Black, White)
ForcedChoiceAccuracydata_agg<- aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(StimulusGroup = RSAMegdata$StimGroup,ExperimentStage = RSAMegdata$ExpProc, ParticipantRace = RSAMegdata$Ethnicity), FUN = "mean")
ForcedChoiceAccuracydata_agg

#Accuracy means for StimulusGroup (OWNG, OTHG) X ExperimentStage(MatchProc, RecognitionProc, FCProc) X ParticipantRace (Black, White) X TPT
ForcedChoiceAccuracydata_agg2<- aggregate(x = RSAMegdata$ForcedChoiceAccuracy, list(StimulusGroup = RSAMegdata$StimGroup,ExperimentStage = RSAMegdata$ExpProc, ParticipantRace = RSAMegdata$Ethnicity, TPTA = RSAMegdata$TargetPresence), FUN = "mean")
ForcedChoiceAccuracydata_agg2

#If you compare the two tables, it is evident that performance appears poorer when participants are not forced to choose. However, their performance increases when they are forced to make a choice.

RSAMegdata_S4<-read.csv("Solomon4.csv",header=T,row.names=NULL)
RSAMegdata_S4$RecLineupACC

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

RSAMegdata_S4_3<-RSAMegdata_S4[RSAMegdata_S4$Procedure != "FillertaskProc",]
RSAMegdata_S4_3<-RSAMegdata_S4_3[RSAMegdata_S4_3$ExpProc != "Matching",]
RSAMegdata_S4_3$Hits <- as.numeric(RSAMegdata_S4_3$Hits)
RSAMegdata_S4_3$FalseAlarms <- as.numeric(RSAMegdata_S4_3$FalseAlarms)

D_S4_forcedchoice<-NULL
D_S4_FC<-NULL
D_S4_forcedchoice <- grouped_df(RSAMegdata_S4_3, list("Subject", "Ethnicity", "StimGroup", "LineupView"), drop = TRUE)
#D_S4_forcedchoice <- na.omit(D_S4_forcedchoice)
D_S4_FC<- summarise(D_S4_forcedchoice , sum(Hits), sum(FalseAlarms), dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4))
D_S4_FC<-rename(D_S4_FC, c("dprime((sum(Hits)/4), (sum(FalseAlarms)/4), 4)" = 'dprime'))
D_S4_FC<-rename(D_S4_FC, c("sum(Hits)" = 'Hits'))
D_S4_FC<-rename(D_S4_FC, c("sum(FalseAlarms)" = 'FalseAlarms'))
D_S4_FC
warnings()

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

