###############################################
########## T Maze Analysis Pipeline ###########
###############################################

workingdirectory <- "C:/Users/chise/Desktop/screen-analysis-for-yellow-Tmaze-and-Joystick/" #set working directory where
                                                                          
file_path <- "data/yellow_T_maze/screen_Tmaze_yellow.csv"


clearnames <- c("blue 1.csv","blue 2.csv","blue 3.csv","blue 4.csv","dark-green 1.csv","dark-green 2.csv","dark-green 3.csv",
                "dark-yellow 1.csv","dark-yellow 2.csv","dark-yellow 3.csv","dark-yellow 4.csv","light-green 1.csv" ,"light-green 2.csv","light-green 3.csv","light-green 4.csv","light-yellow 1.csv","light-yellow 2.csv",
                "light-yellow 3.csv","light-yellow 4.csv","orange 1.csv","orange 2.csv","orange 3.csv",
                "orange 4.csv","pink 1.csv","pink 2.csv","pink 3.csv","pink 4.csv","red 1.csv","red 2.csv","red 3.csv","red 4.csv","white 1.csv","white 2.csv")

#enter line names here in clearnames
clearnames <-gsub(".csv","",clearnames)

Tmazebla <- read.csv(paste0(workingdirectory,file_path), header = TRUE, sep = ",")
for (i in clearnames){
  setwd(workingdirectory)
  newdata <- Tmazebla[which(Tmazebla$X==i),]
  write.csv(newdata, file = paste(i,".csv",sep=""), row.names = F)
}

setwd(workingdirectory)
filename = workingdirectory
files <- list.files(path=filename, pattern="*.csv", full.names=T, recursive=FALSE)
files
Names <- basename(files)
Names 
numberoffiles <- length(Names)

average <- c()
waverage <- c()
standarddeviation <- c()
wstandarddeviation <- c()
NEXP <- c()
naam <- c()

for (i in files){

  Tmaze <- read.csv(i, header = TRUE, sep = ",") #quote = "\"",dec = "," )
  
  nExp <- length (Tmaze[[1]])
  nGroups <- length(levels(Tmaze[[1]]))
  if(nExp==0) next
  
  naam <- c(naam,i)
  Tmaze$PI <- vector("numeric", length = nExp)
  
  for(i in 1:nExp){
    Tmaze$PI[i] <- (Tmaze[[i,2]]-Tmaze[[i,4]])/(Tmaze[[i,2]]+Tmaze[[i,4]])                 
    Tmaze$LD[i] <- (Tmaze[[i,2]]+Tmaze[[i,4]])
  }
  
  Tmaze[nExp+1,6] <- 0
  Tmaze[nExp+1,5] <- 0
  Tmaze[nExp+3,5] <- 0
  for(i in 1:nExp){
    Tmaze[nExp+1,6] <- Tmaze[i,6] + Tmaze[nExp+1,6] ##Generates total number of flies
    Tmaze[nExp+1,5] <- Tmaze[i,5] + Tmaze[nExp+1,5] ##Generates sum of PIs 
    Tmaze[nExp+3,5] <- (Tmaze[i,2]-Tmaze[i,4])+ Tmaze[nExp+3,5] ##Generates Wighted PIs
  }
  
  Tmaze[nExp+2,5] <- Tmaze[nExp+1,5]/nExp ## Generates Mean
  
  Tmaze[nExp+3,5] <-  Tmaze[nExp+3,5]/ Tmaze[nExp+1,6] ##Generates Weighted Mean
  
  VR <- c()
  typeof(VR)
  WVR <- c()
  WVR2 <- c()
  for (i in 1:nExp){
    #Tmaze$VR[i]<- ((Tmaze[i,5])-Tmaze[nExp+2,5])^2
    vr <- ((Tmaze[i,5])-Tmaze[nExp+2,5])^2
    VR <- c(VR,vr)
    #Tmaze$WVR[i] <- ((Tmaze[i,5])-(Tmaze[nExp+3,5]))^2
    wvr <- ((Tmaze[i,5])-Tmaze[nExp+3,5])^2
    WVR <- c(WVR,wvr)
      
  
  }
  
  numberofNas <- length(Tmaze$PI)-nExp
  khaali <- rep(NA, numberofNas) 
  VR <- c(VR, khaali)
  WVR <- c(WVR, khaali)
  Tmaze$VR <- VR
  Tmaze$WVR <- WVR
  
  Tmaze[nExp+4,5] <- sqrt((sum(Tmaze$VR, na.rm = TRUE))/nExp) ## generates normal standard deviation
  
  for (i in 1:nExp)
  {
    wvr2 <- ((Tmaze[i,6])*(Tmaze[i,8]))
    WVR2 <- c(WVR2, wvr2)
    
  }
  
  Tmaze[nExp+5,5] <- sqrt(sum(WVR2)/(Tmaze[nExp+1,6] - 1 )) ##generates weighted standard deviation
  
  mean <- Tmaze[nExp+2,5]
  wmean <- Tmaze[nExp+3,5]
  stddevn <- Tmaze[nExp+4,5]
  Wstddevn <- Tmaze[nExp+5,5]
  
  average <- c(average, mean)
  waverage <- c(waverage, wmean)
  standarddeviation <- c(standarddeviation, stddevn)
  wstandarddeviation <- c(wstandarddeviation, Wstddevn)
  NEXP <- c(NEXP,nExp)

}

finaldatamean <- data.frame(average, standarddeviation, stringsAsFactors = FALSE)
finaldatawmean <- data.frame(waverage, wstandarddeviation, stringsAsFactors = FALSE)

names2 <- as.list(basename(naam))
names2<-gsub(".csv","",naam)
names2 <- basename(names2)

finaldatawmean$Names <- names2
finaldatamean$Names <- names2
finaldatawmean$trials <- NEXP
finaldatamean$trials <- NEXP

finaldatawmean$Names <- gsub("orange 1", "TH", finaldatawmean$Names)
finaldatawmean$Names <- gsub("orange 2", "mb304b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("orange 3", "mb109b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("orange 4", "58E02", finaldatawmean$Names)
finaldatawmean$Names <- gsub("blue 1", "TH-D'", finaldatawmean$Names)
finaldatawmean$Names <- gsub("blue 2", "TH-D1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("blue 3", "TH-D4", finaldatawmean$Names)
finaldatawmean$Names <- gsub("blue 4", "TH-F3", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-yellow 1", "Gr28bd+TrpA1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-yellow 2", "NP5272", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-yellow 3", "MZ840", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-yellow 4", "TH-C'", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-green 1", "mb315c", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-green 2", "TH-D1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-green 3", "mb299b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("red 1", "5htr1b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("red 2", "TH-G80+TH-G4", finaldatawmean$Names)
finaldatawmean$Names <- gsub("red 3", "mb025b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("red 4", "mb060b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-green 1", "mb056b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-green 2", "mb312b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-green 3", "mb032b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("light-green 4", "mb065b", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-yellow 1", "NP47", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-yellow 2", "NP1528", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-yellow 3", "TH-C1+TH-F1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("dark-yellow 4", "DDC(HL9)", finaldatawmean$Names)
finaldatawmean$Names <- gsub("pink 1", "MZ19", finaldatawmean$Names)
finaldatawmean$Names <- gsub("pink 2", "TH-F1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("pink 3", "TH-F2", finaldatawmean$Names)
finaldatawmean$Names <- gsub("pink 4", "DDC(HL8)", finaldatawmean$Names)
finaldatawmean$Names <- gsub("white 1", "TH-G1", finaldatawmean$Names)
finaldatawmean$Names <- gsub("white 2", "NP6510", finaldatawmean$Names)

finaldatamean$Names <- gsub("orange 1", "TH", finaldatamean$Names)
finaldatamean$Names <- gsub("orange 2", "mb304b", finaldatamean$Names)
finaldatamean$Names <- gsub("orange 3", "mb109b", finaldatamean$Names)
finaldatamean$Names <- gsub("orange 4", "58E02", finaldatamean$Names)
finaldatamean$Names <- gsub("blue 1", "TH-D'", finaldatamean$Names)
finaldatamean$Names <- gsub("blue 2", "TH-D1", finaldatamean$Names)
finaldatamean$Names <- gsub("blue 3", "TH-D4", finaldatamean$Names)
finaldatamean$Names <- gsub("blue 4", "TH-F3", finaldatamean$Names)
finaldatamean$Names <- gsub("light-yellow 1", "Gr28bd+TrpA1", finaldatamean$Names)
finaldatamean$Names <- gsub("light-yellow 2", "NP5272", finaldatamean$Names)
finaldatamean$Names <- gsub("light-yellow 3", "MZ840", finaldatamean$Names)
finaldatamean$Names <- gsub("light-yellow 4", "TH-C'", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-green 1", "mb315c", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-green 2", "TH-D1", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-green 3", "mb299b", finaldatamean$Names)
finaldatamean$Names <- gsub("red 1", "5htr1b", finaldatamean$Names)
finaldatamean$Names <- gsub("red 2", "TH-G80+TH-G4", finaldatamean$Names)
finaldatamean$Names <- gsub("red 3", "mb025b", finaldatamean$Names)
finaldatamean$Names <- gsub("red 4", "mb060b", finaldatamean$Names)
finaldatamean$Names <- gsub("light-green 1", "mb056b", finaldatamean$Names)
finaldatamean$Names <- gsub("light-green 2", "mb312b", finaldatamean$Names)
finaldatamean$Names <- gsub("light-green 3", "mb032b", finaldatamean$Names)
finaldatamean$Names <- gsub("light-green 4", "mb065b", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-yellow 1", "NP47", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-yellow 2", "NP1528", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-yellow 3", "TH-C1+TH-F1", finaldatamean$Names)
finaldatamean$Names <- gsub("dark-yellow 4", "DDC(HL9)", finaldatamean$Names)
finaldatamean$Names <- gsub("pink 1", "MZ19", finaldatamean$Names)
finaldatamean$Names <- gsub("pink 2", "TH-F1", finaldatamean$Names)
finaldatamean$Names <- gsub("pink 3", "TH-F2", finaldatamean$Names)
finaldatamean$Names <- gsub("pink 4", "DDC(HL8)", finaldatamean$Names)
finaldatamean$Names <- gsub("white 1", "TH-G1", finaldatamean$Names)
finaldatamean$Names <- gsub("white 2", "NP6510", finaldatamean$Names)


finaldatawmean <- finaldatawmean[order(-waverage),]
finaldatamean <- finaldatamean[order(-average),]

finaldatamean$stderror <- finaldatamean$standarddeviation / sqrt(finaldatamean$trials)
finaldatawmean$stderror <- finaldatawmean$wstandarddeviation / sqrt(finaldatawmean$trials)

par(mar=c(5,4,1,1))
bp <- barplot(finaldatawmean$waverage,axes = TRUE, names.arg = finaldatawmean$Names,xpd= TRUE, axisnames = TRUE, col = ifelse(Names=="light-yellow 1","Yellow","darkgreen"), las = 2, cex.axis = 1, ylim = c(-1.3,1.3), cex.names=0.7, main = "Weighted Average of PIs ", ylab = "Weighted Average ")
abline(h=0)

text(bp, finaldatawmean$waverage-finaldatawmean$stderror-0.1 , round(finaldatawmean$waverage, digits = 3),cex =0.6, font = 2) 
text(bp, finaldatawmean$waverage-finaldatawmean$stderror-0.2 ,paste("n=",finaldatawmean$trials,sep=" "),cex=0.6, font = 4) 
require(Hmisc)
arrows(bp, finaldatawmean$waverage+finaldatawmean$stderror, bp, finaldatawmean$waverage-finaldatawmean$stderror, length = 0.0, angle = 90, code = 3)

# Save in vector graphics
setEPS()
postscript("yellowTmaze_barplot.eps")
par(mar=c(5,4,1,1))
bp <- barplot(finaldatamean$average,axes = TRUE, names.arg = finaldatamean$Names,xpd= TRUE, axisnames = TRUE, col = ifelse(Names=="light-yellow 1","Yellow","darkgreen"), las = 2, cex.axis = 1, ylim = c(-1.3,1.3), cex.names=0.7, main = "Average of PIs ", ylab = "Average ")
abline(h=0)

text(bp, finaldatamean$average-finaldatamean$stderror-0.2 ,paste("n=",finaldatamean$trials,sep=" "),cex=0.6, font = 4) 
arrows(bp, finaldatamean$average+finaldatamean$stderror, bp, finaldatamean$average-finaldatamean$stderror, length = 0.0, angle = 90, code = 3)

dev.off()