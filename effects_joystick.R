write.table(PI_platform, "PIs_yellow.txt", sep="\t", row.names = FALSE,col.names = FALSE)
write.table(PI_platform, "PIs_yellow.txt", sep="\t", row.names = FALSE,col.names = FALSE)



PIs_yellow <- read.delim("C:/Users/LocalAdmin/Desktop/PIs_yellow.txt")
PIs_white <- read.delim("C:/Users/LocalAdmin/Desktop/PIs_white.txt", header=FALSE)



Pretest_white <- PIs_white[!is.na(PIs_white[,1]),1+2]/2

Training_white <- PIs_white[!is.na(PIs_white[,1]),3+4]/2

Pretest_yellow <- PIs_yellow[!is.na(PIs_yellow[,1]),1+2]/2

Training_yellow <- PIs_yellow[!is.na(PIs_yellow[,1]),3+4]/2

yellow_effect <- Training_yellow - Pretest_yellow
white_effect <- Training_white - Pretest_white
