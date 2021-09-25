

#################################################################### Functions ####################################################

# Check if the light is on or of depending of the hysteresis and the previous trace
check_switch <- function(trace_point, switch_off, switch_on) {
  
  if (trace_point == FALSE && switch_on == TRUE) { trace_point <- TRUE}
  
  if (trace_point == TRUE && switch_off == TRUE) {trace_point <- FALSE}
  
  return(trace_point)	# return the last timestamp in the dataframe
}

# make a vector of light on or of for the trace and hysteresis
ligth_state <- function(trace, Hysteresis) {
  
  switch_on <- (trace > Hysteresis) # potential signals to turn on the light
  switch_off <- (trace < -Hysteresis) # potential switch-off signals
  state <- vector("logical",length= lengthExp) # vector allocation for light ON/OFF state
  
  if (trace[1] > Hysteresis) {state[1]<-TRUE} 
  
  for (i in 2:lengthExp){
    state[i] <- state[i-1]
    state[i] <- check_switch(state[i], switch_off[i], switch_on[i])
  }
  
  return(state)
}

# Calculate a PI from a boolean light state vector
calculate_PI <- function(state_vector) {
  PI <- (sum(state_vector)-sum(!state_vector))/length(state_vector)
  return(PI)
}

all_false <- function(thres,trace,window=4800) {
  
  lag20 <- abs(diff(trace, lag = 20))
  over_thres <- lag20 > thres
  flat <- vector("logical",length(trace)-window)
  for (i in 1:(length(trace)-window))
  {
    flat[i]<-any(over_thres[i:(i+window)])
  }
  
  keep<-all(flat)
  
  return(keep)
  
}

########################################## Initialize some parameters  ###########################################

## Set initial parameters
data_path<-"C:/Users/chise/Desktop/screen-analysis-for-yellow-Tmaze-and-Joystick/data/Joystick/"
flat_thres <- 0.8
PI_thres <- 2 #no flies rejected on this basis
Hysteresis <- 0
filename_data<-"all.txt"

tested_flies <- read.table(paste0(data_path,filename_data), quote="\"", comment.char="")
all_screens <- unique(tested_flies$V2)
no_of_screens <- length(all_screens)
skip<-37
barplot_title <- "all experiments"

unblind <- TRUE

if (unblind){
  
  all_screens <- gsub("orange1", "TH", all_screens)
  all_screens <- gsub("orange2", "mb304b", all_screens)
  all_screens <- gsub("orange3", "mb109b", all_screens)
  all_screens <- gsub("orange4", "58E02", all_screens)
  all_screens <- gsub("blue1", "TH-D'", all_screens)
  all_screens <- gsub("blue2", "TH-D1", all_screens)
  all_screens <- gsub("blue3", "TH-D4", all_screens)
  all_screens <- gsub("blue4", "TH-F3", all_screens)
  all_screens <- gsub("positivecontrol", "Gr28bd+TrpA1", all_screens)
  all_screens <- gsub("lightyellow2", "NP5272", all_screens)
  all_screens <- gsub("lightyellow3", "MZ840", all_screens)
  all_screens <- gsub("lightyellow4", "TH-C'", all_screens)
  all_screens <- gsub("darkgreen1", "mb315c", all_screens)
  all_screens <- gsub("darkgreen2", "TH-D1", all_screens)
  all_screens <- gsub("darkgreen3", "mb299b", all_screens)
  all_screens <- gsub("red1", "5htr1b", all_screens)
  all_screens <- gsub("red2", "TH-G80+TH-G4", all_screens)
  all_screens <- gsub("red3", "mb025b", all_screens)
  all_screens <- gsub("red4", "mb060b", all_screens)
  all_screens <- gsub("lightgreen1", "mb056b", all_screens)
  all_screens <- gsub("lightgreen2", "mb312b", all_screens)
  all_screens <- gsub("lightgreen3", "mb032b", all_screens)
  all_screens <- gsub("lightgreen4", "mb065b", all_screens)
  all_screens <- gsub("darkyellow1", "NP47", all_screens)
  all_screens <- gsub("darkyellow2", "NP1528", all_screens)
  all_screens <- gsub("darkyellow3", "TH-C1+TH-F1", all_screens)
  all_screens <- gsub("darkyellow4", "DDC(HL9)", all_screens)
  all_screens <- gsub("darkpink1", "MZ19", all_screens)
  all_screens <- gsub("darkpink2", "TH-F1", all_screens)
  all_screens <- gsub("darkpink3", "TH-F2", all_screens)
  all_screens <- gsub("darkpink4", "DDC(HL8)", all_screens)
  all_screens <- gsub("white1", "TH-G1", all_screens)
  all_screens <- gsub("white2", "NP6510", all_screens)
  
  tested_flies$V2 <- gsub("orange1", "TH", tested_flies$V2)
  tested_flies$V2 <- gsub("orange2", "mb304b", tested_flies$V2)
  tested_flies$V2 <- gsub("orange3", "mb109b", tested_flies$V2)
  tested_flies$V2 <- gsub("orange4", "58E02", tested_flies$V2)
  tested_flies$V2 <- gsub("blue1", "TH-D'", tested_flies$V2)
  tested_flies$V2 <- gsub("blue2", "TH-D1", tested_flies$V2)
  tested_flies$V2 <- gsub("blue3", "TH-D4", tested_flies$V2)
  tested_flies$V2 <- gsub("blue4", "TH-F3", tested_flies$V2)
  tested_flies$V2 <- gsub("positivecontrol", "Gr28bd+TrpA1", tested_flies$V2)
  tested_flies$V2 <- gsub("lightyellow2", "NP5272", tested_flies$V2)
  tested_flies$V2 <- gsub("lightyellow3", "MZ840", tested_flies$V2)
  tested_flies$V2 <- gsub("lightyellow4", "TH-C'", tested_flies$V2)
  tested_flies$V2 <- gsub("darkgreen1", "mb315c", tested_flies$V2)
  tested_flies$V2 <- gsub("darkgreen2", "TH-D1", tested_flies$V2)
  tested_flies$V2 <- gsub("darkgreen3", "mb299b", tested_flies$V2)
  tested_flies$V2 <- gsub("red1", "5htr1b", tested_flies$V2)
  tested_flies$V2 <- gsub("red2", "TH-G80+TH-G4", tested_flies$V2)
  tested_flies$V2 <- gsub("red3", "mb025b", tested_flies$V2)
  tested_flies$V2 <- gsub("red4", "mb060b", tested_flies$V2)
  tested_flies$V2 <- gsub("lightgreen1", "mb056b", tested_flies$V2)
  tested_flies$V2 <- gsub("lightgreen2", "mb312b", tested_flies$V2)
  tested_flies$V2 <- gsub("lightgreen3", "mb032b", tested_flies$V2)
  tested_flies$V2 <- gsub("lightgreen4", "mb065b", tested_flies$V2)
  tested_flies$V2 <- gsub("darkyellow1", "NP47", tested_flies$V2)
  tested_flies$V2 <- gsub("darkyellow2", "NP1528", tested_flies$V2)
  tested_flies$V2 <- gsub("darkyellow3", "TH-C1+TH-F1", tested_flies$V2)
  tested_flies$V2 <- gsub("darkyellow4", "DDC(HL9)", tested_flies$V2)
  tested_flies$V2 <- gsub("darkpink1", "MZ19", tested_flies$V2)
  tested_flies$V2 <- gsub("darkpink2", "TH-F1", tested_flies$V2)
  tested_flies$V2 <- gsub("darkpink3", "TH-F2", tested_flies$V2)
  tested_flies$V2 <- gsub("darkpink4", "DDC(HL8)", tested_flies$V2)
  tested_flies$V2 <- gsub("white1", "TH-G1", tested_flies$V2)
  tested_flies$V2 <- gsub("white2", "NP6510", tested_flies$V2)
  
  
}


########################################################### Allocate variables  ###################################################################
on_wiggle1<- NA
on_wiggle2<- NA
on_wiggle3 <- NA

off_wiggle1<- NA
off_wiggle2<- NA
off_wiggle3 <- NA


Nexp_total <- 40 # they won´t be more experiment than these #round(length(tested_flies$V1)/no_of_screens)*4
effectsize_mat <- matrix(NA, 3*Nexp_total, no_of_screens)
just_rein_mat <- matrix(NA, 3*Nexp_total, no_of_screens)
used_traces <- as.numeric(rep(NA,no_of_screens))

on_wiggle_mat <- matrix(NA, 3*Nexp_total, no_of_screens)
off_wiggle_mat <- matrix(NA, 3*Nexp_total, no_of_screens)
all_PIs <- as.numeric(rep(NA,10))

for(j in 1:no_of_screens)
  
{
  count <- 0 #Defining a variable that keeps a count of the number of graphs taken 
  
  ## Groupname
  group_name <- all_screens[j]
  
  ## Number of experiments in the group. Each experiments runs three platforms at once
  Nexp <- length(tested_flies$V1[tested_flies$V2==group_name])
  
  ## Filenames from group
  data_files <- as.character(tested_flies$V1[tested_flies$V2==group_name])

  PI_platform <- matrix(NA, 3*Nexp, 10)    # Variable where PIs are saved for each of the ten segments
  combo_PI_platform <- matrix(NA, 3*Nexp, 5) # Variable where the mean of PI's are stored each double segments. There is always two equal consecutive segments
  
  ## Allocating vectors to save data
  on_wiggle <- as.numeric(rep(NA,3*Nexp))
  off_wiggle <- as.numeric(rep(NA,3*Nexp))
  diff_wiggle <- as.numeric(rep(NA,3*Nexp))
  
  effectsize <- as.numeric(rep(NA,3*Nexp)) # Saurabh variable defining effectsize score
  effectsize_reinf <- as.numeric(rep(NA,3*Nexp)) # Saurabh variable defining effectsize score

  # Start a for loop for the number of flies to analyze
  for(i in 1:Nexp){
    #group_name <- tested_flies$V2[i]   #Name of the group
    
    
    ############################## Import in a dataframe just the values. ################################
    

    data <- read.table(paste0(data_path,data_files[i]), header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 24037-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))
    
    
    
    ################################### Import in a dataframe the information of the experiments. ############################################
    
    
    info <-read.table(paste0(data_path,data_files[i]), header = FALSE, sep = "", 
                      col.names = paste0("V",seq_len(20)), fill = TRUE)
    info  <- info[1:20,]
    
    
    ######################################## Extracting some parameters from the meta data #########################################
    
    lengthExp <- length(data$t.s.)     # Number of data points
    # Side of the platform for turning the optogenetic light on, for each platform
    light_side1 <- c(as.character(info$V3[6]),as.character(info$V4[6]),as.character(info$V5[6]),as.character(info$V6[6]),as.character(info$V7[6]),as.character(info$V8[6]),as.character(info$V9[6]),as.character(info$V10[6]),as.character(info$V11[6]),as.character(info$V12[6])) 
    light_side2 <- c(as.character(info$V3[10]),as.character(info$V4[10]),as.character(info$V5[10]),as.character(info$V6[10]),as.character(info$V7[10]),as.character(info$V8[10]),as.character(info$V9[10]),as.character(info$V10[10]),as.character(info$V11[10]),as.character(info$V12[10])) 
    light_side3 <- c(as.character(info$V3[14]),as.character(info$V4[14]),as.character(info$V5[14]),as.character(info$V6[14]),as.character(info$V7[14]),as.character(info$V8[14]),as.character(info$V9[14]),as.character(info$V10[14]),as.character(info$V11[14]),as.character(info$V12[14])) 
    
    # If all true it means that the three experiments were ran with the lights switched on when platform moves right
    right_platform1 <- all(light_side1=="right")
    right_platform2 <- all(light_side2=="right")
    right_platform3 <- all(light_side3=="right")
    
    TimeExp <- data$t.s.[lengthExp]   # The total time it took for the experiment to complete
    data$Sampling<-c(0,diff(data$t.s., lag = 1)) # Calculating Inter Sample intervals (ISI)
    MaxSample<-max(data$Sampling)  # Checking what it the maximal ISI

    ######################################### Plot platform traces and decide which to keep for further analysis ############################################
    
    # Segment datapoint rechnen und plotten mit dem trace
    segment<- seq(from = 0,to = lengthExp, lengthExp/10)

    
    ############################################################### PI rechnen ##############################################

    # Save in state variable if the light is on or off taking care of hysteresis
    data$state1 <- ligth_state(data$pos1,Hysteresis)
    data$state2 <- ligth_state(data$pos2,Hysteresis)
    data$state3 <- ligth_state(data$pos3,Hysteresis)
    
    # Change the ON or OFF state if the platform were set to reinforce left
    if(right_platform1==FALSE){ data$state1 <- !data$state1}
    if(right_platform2==FALSE){ data$state2 <- !data$state2}
    if(right_platform3==FALSE){ data$state3 <- !data$state3}
    
    # Test Condition 1: if the platform signal stay too long flat, we want to discard results
    keep1<-all_false(flat_thres,data$pos1)
    keep2<-all_false(flat_thres,data$pos2)
    keep3<-all_false(flat_thres,data$pos3)
    
    ## Condition 2: Calculate PIs and sort the ones with good pretest
    
    # Allocate PI vectors
    PI_platform1 <- vector("numeric", length = 10)
    PI_platform2 <- vector("numeric", length = 10)
    PI_platform3 <- vector("numeric", length = 10)

    # Allocate effect size vectors
    effectsize1 <- vector("numeric", 1)
    effectsize2 <- vector("numeric", 1)
    effectsize3 <- vector("numeric", 1)
    
    ## Calculate the wiggle as a measure of locomotory/activity state
    wiggle1<-abs(diff(data$pos1))
    wiggle2<-abs(diff(data$pos2))
    wiggle3<-abs(diff(data$pos3))
    
    ## Calculate the wiggle for each of the two treatments: lights on/off. 
    on_wiggle1 <- mean(wiggle1[data$state1[-24000]])
    off_wiggle1 <- mean(wiggle1[!data$state1[-24000]])
    on_wiggle2 <- mean(wiggle2[data$state2[-24000]])
    off_wiggle2 <- mean(wiggle2[!data$state2[-24000]])
    on_wiggle3 <- mean(wiggle3[data$state3[-24000]])
    off_wiggle3 <- mean(wiggle3[!data$state3[-24000]])
    

    ## Apply the PI formula and save it
    for(oo in 1:10){
      PI_platform1[oo] <- calculate_PI(data$state1[segment[oo]:segment[oo+1]])
      PI_platform2[oo] <- calculate_PI(data$state2[segment[oo]:segment[oo+1]])
      PI_platform3[oo] <- calculate_PI(data$state3[segment[oo]:segment[oo+1]])
    }
    

    ## Calculate the effect size by substracting the initial baseline (segments 1 and 2) to the training Periods (3,4,7,8)
    effectsize1 <- mean(PI_platform1[c(3,4,7,8)])-mean(PI_platform1[1:2])
    effectsize2 <- mean(PI_platform2[c(3,4,7,8)])-mean(PI_platform2[1:2])
    effectsize3 <- mean(PI_platform3[c(3,4,7,8)])-mean(PI_platform3[1:2])
    
    ## Get the PIs the training Periods (3,4,7,8) but without normalizing by initial baseline
    just_reinf1 <- mean(PI_platform1[c(3,4,7,8)])
    just_reinf2 <- mean(PI_platform2[c(3,4,7,8)])
    just_reinf3 <- mean(PI_platform3[c(3,4,7,8)])
    
    # Condition 3: we set a threshold of number of light switching to make sure the fly had the opportunity to explore both sides enough.
    # Tally light encounters in the first training period
    light_encounter1 <- sum(abs(diff(data$state1[(lengthExp/5):((lengthExp*2)/5)])))
    light_encounter2 <- sum(abs(diff(data$state2[(lengthExp/5):((lengthExp*2)/5)])))
    light_encounter3 <- sum(abs(diff(data$state3[(lengthExp/5):((lengthExp*2)/5)])))
    
    
    ## Here all conditions should be met: No flat signal, good baseline pretest PI and enough switching 
    if(keep1 && abs(mean(PI_platform1[1:2]))<PI_thres & light_encounter1>2){
      count<-count+1
      PI_platform[((3*i)-2),] <- PI_platform1
      effectsize[((3*i)-2)] <- effectsize1
      effectsize_reinf[((3*i)-2)] <- just_reinf1
      on_wiggle[((3*i)-2)] <- on_wiggle1
      off_wiggle[((3*i)-2)] <- off_wiggle1
    }
    
    if(keep2 && abs(mean(PI_platform2[1:2]))<PI_thres & light_encounter2>2){
      count<-count+1
      PI_platform[((3*i)-1),] <- PI_platform2
      effectsize[((3*i)-1)] <- effectsize2
      effectsize_reinf[((3*i)-1)] <- just_reinf2
      on_wiggle[((3*i)-1)] <- on_wiggle2
      off_wiggle[((3*i)-1)] <- off_wiggle2
    }
    
    if(keep3 && abs(mean(PI_platform3[1:2]))<PI_thres & light_encounter3>2){
      count<-count+1
      PI_platform[((3*i)),] <- PI_platform3
      effectsize[(3*i)] <- effectsize3
      effectsize_reinf[((3*i))] <- just_reinf3
      on_wiggle[((3*i))] <- on_wiggle3
      off_wiggle[((3*i))] <- off_wiggle3
    }
  }
  

  effectsize_mat[1:length(effectsize),j] <- effectsize
  on_wiggle_mat[1:length(on_wiggle),j] <- on_wiggle
  off_wiggle_mat[1:length(off_wiggle),j] <- off_wiggle
  just_rein_mat[1:length(effectsize_reinf),j] <- effectsize_reinf
  all_PIs <- rbind(all_PIs,PI_platform)
  used_traces[j]<-count


  # Boxplot of the PIs
  boxplot(PI_platform, col="grey",xlab="",ylab="PI",main=group_name, ylim = c(-1, 1),names=c("Pretest","Pretest","Training","Training","Test","Test","Training","Training","Test","Test"), cex.lab=1.5, cex.axis = 1.2)
  abline(h = 0, untf = FALSE, col="black",lwd=3)
  group <- NULL
  for(s in 1:10){
    a <- rep(s,Nexp*3)
    group <- append(group,a)
  }
  stripchart(as.vector(PI_platform)~group,vertical = TRUE, method = "jitter",pch = 21, col = "maroon", bg = "bisque",add = TRUE) 
  
  accepted_flies <- which(!is.na(PI_platform[,1]))
  
  matplot(t(PI_platform[accepted_flies,]), type = c("b"),xlab=group_name,main=group_name, pch=1,col = 1:4)
  
}


colnames(effectsize_mat) <- as.character(all_screens)
colnames(just_rein_mat) <- as.character(all_screens)

boxplot(effectsize_mat,col="yellow",xlab="",ylab="reinforcement - pretest", names = as.character(all_screens), cex.lab=1.0, cex.axis = 1, las=2)
abline(h = 0, untf = FALSE, col="black",lwd=3)

boxplot(just_rein_mat,col="yellow",xlab="",ylab="just_reinforcement", names = as.character(all_screens), cex.lab=1.0, cex.axis = 1, las=2)
abline(h = 0, untf = FALSE, col="black",lwd=3)

diff_wiggle_mat=on_wiggle_mat-off_wiggle_mat
boxplot(diff_wiggle_mat,col="yellow",xlab="",ylab="wiggle difference(on - off)", names = as.character(all_screens),cex.lab=1.0, cex.axis = 1, las = 2)
abline(h = 0, untf = FALSE, col="black",lwd=3)

## Reinforcement barplot

mean_effectsize <- apply(effectsize_mat,2,function(effectsize_mat){mean(effectsize_mat, na.rm = TRUE)})
std_dev_effectsize <- apply(effectsize_mat,2,function(effectsize_mat){sd(effectsize_mat, na.rm = TRUE)})

std_err_effectsize <- std_dev_effectsize/count**0.5

mean_effectsize_ord <- mean_effectsize[order(mean_effectsize, decreasing = TRUE)]
std_err_effectsize_ord <- std_err_effectsize[order(mean_effectsize, decreasing = TRUE)]

# Save in vector graphics
setEPS()
postscript("joystick_barplot.eps")
#svg(filename="joystick_barplot.svg", width=20, height=20, pointsize=24)

barCenters <- barplot(height = mean_effectsize_ord,
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 1,
                      main = "Reinforcement with yellow light",
                      ylab = "Reinforcement",
                      border = "black", axes = TRUE)


segments(barCenters, mean_effectsize_ord - std_err_effectsize_ord, barCenters,
         mean_effectsize_ord + std_err_effectsize_ord, lwd = 1.5)

dev.off()

write.table(rbind(mean_effectsize_ord,std_err_effectsize_ord), "joystick_screen_results.txt")

## Wiggle barplot

mean_diff_wiggle <- apply(diff_wiggle_mat,2,function(diff_wiggle_mat){mean(diff_wiggle_mat, na.rm = TRUE)})
std_dev_diff_wiggle <- apply(diff_wiggle_mat,2,function(diff_wiggle_mat){sd(diff_wiggle_mat, na.rm = TRUE)})
std_err_diff_wiggle <- std_dev_diff_wiggle/count**0.5

barCenters <- barplot(height = mean_diff_wiggle,
                      beside = true, las = 2,ylim=c(-0.3,0.3),
                      cex.names = 1,
                      main = "Wiggle with yellow light",
                      ylab = "wiggle difference(on - off)",
                      border = "black", axes = TRUE)


segments(barCenters, mean_diff_wiggle - std_err_diff_wiggle, barCenters,
         mean_diff_wiggle + std_err_diff_wiggle, lwd = 1.5)


#barplot(mean_diff_wiggle,col="yellow",xlab="",ylab="wiggle difference(on - off)", cex.lab=1.0, cex.axis = 0.5)
#abline(h = 0, untf = FALSE, col="black",lwd=3)


## Total PI overall and number of experiments per line

boxplot(all_PIs,main="overall PI for the whole screen - mixed")

names(used_traces)<-all_screens
barplot(used_traces,las=2,main = barplot_title)

####################################

