
setwd("C:/Users/LocalAdmin/Desktop/data/platform_data2017")


########################################## Initialize some parameters  ###########################################


skip<-37
data <- read.table(file.choose(), header = FALSE, sep = "\t", quote = "\"" , dec = ".", fill = TRUE, skip = skip , comment.char = "", nrows = 24037-skip,col.names=c("n","t.s.","pos1","pos2","pos3"))


################ function #####################

all_false <- function(thres,trace,window=4800) {
  
  lag18 <- abs(diff(trace, lag = 18))
  over_thres <- lag18 > thres
  flat <- vector("logical",length(trace)-window)
  for (i in 1:(length(trace)-window))
  {
    flat[i]<-any(over_thres[i:(i+window)])
  }
  
  keep<-all(flat)
    
  return(keep)
  
}


################### plot the traces to check by eye ###############

plot(data$pos1,type = "l")
plot(data$pos2,type = "l")
plot(data$pos3,type = "l")


############ check if keep1 2 and 3 match your subjective criteria ###############

thres <- 0.5

keep1<-all_false(thres,data$pos1)
keep2<-all_false(thres,data$pos2)
keep3<-all_false(thres,data$pos3)
