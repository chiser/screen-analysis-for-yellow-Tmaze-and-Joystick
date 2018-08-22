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

# Calculate a stdev from a boolean light state vector
calculate_SD <- function(trace_chunk) {
  sd_trace_chunk <- sd(trace_chunk,na.rm = TRUE)
  return(sd_trace_chunk)
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

################################################################ Set working directory where I saved the data   ##################################################

setwd("C:/Users/LocalAdmin/Desktop/new_screens/all")


########################################## Initialize some parameters  ###########################################


tested_flies <- read.table("C:/Users/LocalAdmin/Desktop/new_screens/all/light_yellow1.txt", quote="\"", comment.char="")

skip<-37
flat_thres <- 0.6
PI_thres <- 2
cross_thres <- 2

#message("please enter the fly name")
#group_name <- scan(n=1,what= character())   #Name of the group

#message("please enter the number of experiments")
#Nexp <- scan(n=1,what= numeric())   #Number of flies
Nexp <- length(tested_flies$V1)
traces_platform <- matrix(NA, 3*Nexp, 24000)    # Variable where PIs are saved
SD_platform <- matrix(NA, 3*Nexp, 10)    # Variable where SDs are saved

# Start a for loop for the number of flies to analyze
for(i in 1:Nexp){
  
  ############################## Import in a dataframe just the values. ################################
  
  #data <- read.table(file.choose(), header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 24037-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))
  data <- read.table(as.character(tested_flies$V1[i]), header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 24037-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))
  
  ################################### Import in a dataframe the information of the experiments. ############################################
  
  
  info <-read.table(as.character(tested_flies$V1[i]), header = FALSE, sep = "", 
                    col.names = paste0("V",seq_len(20)), fill = TRUE)
  info  <- info[1:20,]
  
  
  ######################################## Extracting some parameters from the meta data #########################################
  lengthExp <- length(data$t.s.)     # Number of data points
  Hysteresis <- 0
  
  # Side of light for each platform
  light_side1 <- c(as.character(info$V3[6]),as.character(info$V4[6]),as.character(info$V5[6]),as.character(info$V6[6]),as.character(info$V7[6]),as.character(info$V8[6]),as.character(info$V9[6]),as.character(info$V10[6]),as.character(info$V11[6]),as.character(info$V12[6])) 
  light_side2 <- c(as.character(info$V3[10]),as.character(info$V4[10]),as.character(info$V5[10]),as.character(info$V6[10]),as.character(info$V7[10]),as.character(info$V8[10]),as.character(info$V9[10]),as.character(info$V10[10]),as.character(info$V11[10]),as.character(info$V12[10])) 
  light_side3 <- c(as.character(info$V3[14]),as.character(info$V4[14]),as.character(info$V5[14]),as.character(info$V6[14]),as.character(info$V7[14]),as.character(info$V8[14]),as.character(info$V9[14]),as.character(info$V10[14]),as.character(info$V11[14]),as.character(info$V12[14])) 
  
  right_platform1 <- all(light_side1=="right")
  right_platform2 <- all(light_side2=="right")
  right_platform3 <- all(light_side3=="right")
  
  left_platform1 <- all(light_side1=="left")
  left_platform2 <- all(light_side2=="left")
  left_platform3 <- all(light_side3=="left")
  
  TimeExp <- data$t.s.[lengthExp]   # The total time it took for the experiment to complete
  data$Sampling<-c(0,diff(data$t.s., lag = 1)) # Calculating Inter Sample intervals (ISI)
  MaxSample<-max(data$Sampling)  # Checking what it the maximal ISI
  #plot(data[[2]]~data[[1]], type="l")  # This graph show how homogeneous was the sampling rate
  
  ######################################### Plot platform traces and decide which to keep for further analysis ############################################
  
  # Segment rechnen und plotten mit dem trace
  
  segment<- seq(from = 0,to = lengthExp, lengthExp/10)
  
  
  #plot(data$n,data$pos1, type = "l",xlab = "data points",ylab = "Position", main = "Platform 1")
  #abline(v = segment, untf = FALSE, col="red",lwd=3)
  #message("please enter T for keeping the fly for analysis and F for deleting it and press enter")
  #keep_fly1 <- scan(n=1,what= logical())
  
  #plot(data$n,data$pos2, type = "l",xlab = "data points",ylab = "Position", main = "Platform 2")
  #abline(v = segment, untf = FALSE, col="red",lwd=3)
  #message("please enter T for keeping the fly for analysis and F for deleting it and press enter")
  #keep_fly2 <- scan(n=1,what= logical())
  
  #plot(data$n,data$pos3, type = "l",xlab = "data points",ylab = "Position", main = "Platform 3")
  #abline(v = segment, untf = FALSE, col="red",lwd=3)
  #message("please enter T for keeping the fly for analysis and F for deleting it and press enter")
  #keep_fly3 <- scan(n=1,what= logical())
  
  
  ############################################################### PI rechnen ##############################################
  
  
  # Save in state variable if the light is on or off taking care of hysteresis
  data$state1 <- ligth_state(data$pos1,Hysteresis)
  data$state2 <- ligth_state(data$pos2,Hysteresis)
  data$state3 <- ligth_state(data$pos3,Hysteresis)
  
  # Change the ON or OFF state if the platform were set to reinforce left
  if(any((right_platform1==FALSE && left_platform1==FALSE),(right_platform2==FALSE && left_platform2==FALSE),(right_platform2==FALSE && left_platform2==FALSE))){warning("check the right/left reinforcement")}
  if(right_platform1==FALSE){ data$state1 <- !data$state1}
  if(right_platform2==FALSE){ data$state2 <- !data$state2}
  if(right_platform3==FALSE){ data$state3 <- !data$state3}
  
  # Condition 1 for the flat line
  keep1<-all_false(flat_thres,data$pos1)
  keep2<-all_false(flat_thres,data$pos2)
  keep3<-all_false(flat_thres,data$pos3)
  
  # Condition 2: Calculate PIs and sort the ones with good pretest
  
  PI_platform1 <- vector("numeric", length = 10)
  PI_platform2 <- vector("numeric", length = 10)
  PI_platform3 <- vector("numeric", length = 10)
  SD_platform1 <- vector("numeric", length = 10)
  SD_platform2 <- vector("numeric", length = 10)
  SD_platform3 <- vector("numeric", length = 10)
  for(oo in 1:10){
    PI_platform1[oo] <- calculate_PI(data$state1[segment[oo]:segment[oo+1]])
    PI_platform2[oo] <- calculate_PI(data$state2[segment[oo]:segment[oo+1]])
    PI_platform3[oo] <- calculate_PI(data$state3[segment[oo]:segment[oo+1]])
  }
  
  for(oo in 1:10){
    SD_platform1[oo] <- calculate_SD(data$pos1[segment[oo]:segment[oo+1]])
    SD_platform2[oo] <- calculate_SD(data$pos1[segment[oo]:segment[oo+1]])
    SD_platform3[oo] <- calculate_SD(data$pos1[segment[oo]:segment[oo+1]])
  }
  
  # Condition 3: Tally light encounters in the first training period
  light_encounter1 <- sum(abs(diff(data$state1[(lengthExp/5):((lengthExp*2)/5)])))
  light_encounter2 <- sum(abs(diff(data$state2[(lengthExp/5):((lengthExp*2)/5)])))
  light_encounter3 <- sum(abs(diff(data$state3[(lengthExp/5):((lengthExp*2)/5)])))
  
  
  if(keep1 && abs(mean(PI_platform1[1:2]))<PI_thres & light_encounter1>cross_thres){
    
    traces_platform[3*i-2,] <- data$pos1
    SD_platform[3*i-2,] <- SD_platform1
  }
  
  if(keep2 && abs(mean(PI_platform2[1:2]))<PI_thres & light_encounter2>cross_thres){
    
    traces_platform[3*i-1,] <- data$pos2
    SD_platform[3*i-1,] <- SD_platform2
  }
  
  if(keep3 && abs(mean(PI_platform3[1:2]))<PI_thres & light_encounter3>cross_thres){
    
    traces_platform[3*i,] <- data$pos3
    SD_platform[3*i,] <- SD_platform3
  }
  
}

mean_trace <- colMeans(traces_platform,na.rm=TRUE)
#sd_trace <- apply(traces_platform,2,function(x){result<-sd(x,na.rm=TRUE);return(result)})

# Plot of the PIs
plot(mean_trace, type = "l",xlab = "data points",ylab = "Position", main = "TrpA1+Gr28bd>Chrimson n=12")
abline(v = segment, untf = FALSE, col="red",lwd=3)

#plot(sd_trace, type = "l",xlab = "data points",ylab = "Standard deviation", main = "TH-D'>Chrimson n=11")
#abline(v = segment, untf = FALSE, col="red",lwd=3)
boxplot(SD_platform, col="grey",xlab="",ylab="SD",main="TrpA1+Gr28bd>Chrimson n=12", ylim = c(0, 3.5),names=c("Pretest","Pretest","Training","Training","Test","Test","Training","Training","Test","Test"), cex.lab=1.5, cex.axis = 1.2)
group <- NULL
for(i in 1:10){
  a <- rep(i,Nexp*3)
  group <- append(group,a)
}
stripchart(as.vector(SD_platform)~group,vertical = TRUE, method = "jitter",pch = 21, col = "maroon", bg = "bisque",add = TRUE)

#approx_entropy(traces_platform[1,], edim = 2, r = 0.2*sd(ts), elag = 1)
#sample_entropy(ts, edim = 2, r = 0.2*sd(ts), tau = 1)
