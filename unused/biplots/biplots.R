working_directory <- "C:/Users/chise/Desktop/screen-analysis-for-yellow-Tmaze-and-Joystick/unused/biplots/"

effect_occupancy_clean <- read.table(paste0(working_directory,"Ymazes_screen.txt"), header = TRUE)
idGroup_yellow <- read.table(paste0(working_directory,"Tmaze_yellow_screen_aggregated.txt"), header = TRUE)
idGroup <- read.table(paste0(working_directory,"Tmaze_red_screen_aggregated.txt"), header = TRUE)
joystick_screen <- read.table(paste0(working_directory,"joystick_screen.txt"), header = TRUE)
Tmaze <- read.table(paste0(working_directory,"Tmaze_red_screen.txt"), header = TRUE)
Tmaze_yellow <- read.table(paste0(working_directory,"Tmaze_yellow_screen.txt"), header = TRUE)


########################################################################################################################


##Unblind tmaze yellow
Tmaze_yellow$Fly.line_unblind <- Tmaze_yellow$Fly.line
Tmaze_yellow$Fly.line_unblind <- gsub("orange 1", "TH", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("orange 2", "mb304b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("orange 3", "mb109b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("orange 4", "58E02", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("blue 1", "TH-D'", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("blue 2", "TH-D1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("blue 3", "TH-D4", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("blue 4", "TH-F3", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-yellow 1", "Gr28bd+TrpA1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-yellow 2", "NP5272", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-yellow 3", "MZ840", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-yellow 4", "TH-C'", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-green 1", "mb315c", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-green 2", "TH-D1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-green 3", "mb299b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("red 1", "5htr1b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("red 2", "TH-G80+TH-G4", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("red 3", "mb025b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("red 4", "mb060b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-green 1", "mb056b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-green 2", "mb312b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-green 3", "mb032b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("light-green 4", "mb065b", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-yellow 1", "th", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-yellow 2", "NP1528", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-yellow 3", "TH-C1+TH-F1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("dark-yellow 4", "DDC(HL9)", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("pink 1", "MZ19", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("pink 2", "TH-F1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("pink 3", "TH-F2", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("pink 4", "DDC(HL8)", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("white 1", "TH-G1", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("white 2", "NP6510", Tmaze_yellow$Fly.line_unblind)
Tmaze_yellow$Fly.line_unblind <- gsub("positivecontrol1", "Gr28bd+TrpA1", Tmaze_yellow$Fly.line_unblind)

## Joystick

colnames(joystick_screen) <- gsub("orange1", "TH", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("orange2", "mb304b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("orange3", "mb109b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("orange4", "58E02", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("blue1", "TH-D'", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("blue2", "TH-D1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("blue3", "TH-D4", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("blue4", "TH-F3", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightyellow1", "Gr28bd+TrpA1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightyellow2", "NP5272", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightyellow3", "MZ840", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightyellow4", "TH-C'", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkgreen1", "mb315c", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkgreen2", "TH-D1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkgreen3", "mb299b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("red1", "5htr1b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("red2", "TH-G80+TH-G4", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("red3", "mb025b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("red4", "mb060b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightgreen1", "mb056b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightgreen2", "mb312b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightgreen3", "mb032b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("lightgreen4", "mb065b", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkyellow1", "th", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkyellow2", "NP1528", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkyellow3", "TH-C1+TH-F1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkyellow4", "DDC(HL9)", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkpink1", "MZ19", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkpink2", "TH-F1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkpink3", "TH-F2", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("darkpink4", "DDC(HL8)", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("white1", "TH-G1", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("white2", "NP6510", colnames(joystick_screen))
colnames(joystick_screen) <- gsub("positivecontrol1", "Gr28bd+TrpA1", colnames(joystick_screen))


joystick<- colnames(joystick_screen)
joystick<-data.frame(joystick)
joystick$mean <- apply(joystick_screen,2, function(x) mean(x,na.rm=TRUE))
joystick$sd <- apply(joystick_screen,2, function(x) sd(x,na.rm=TRUE))
joystick$n <- apply(joystick_screen,2, function(x) length(x[!is.na(x)]))
joystick$se <- joystick$sd/sqrt(joystick$n)

new_order <- order(joystick$mean, decreasing = T)
joystick <- joystick[new_order,]

#joystick$Treatment <- c(rep("Experimental",11),"positive control") 
#joystick <- joystick[order(joystick$Treatment), ]
#joystick$rank <- (joystick$Treatment!="66")*1

new_order <- order(joystick$mean, decreasing = T)
joystick <- joystick[new_order,]
joystick$rank <- grepl("TrpA1", joystick$joystick)*1
new_order <- order(joystick$rank, decreasing = T)
joystick <- joystick[new_order,]

colnames(joystick)[1] <- "line"


#########################################################################################################################

library("psych")

############################################################

## Tmaze red and Ymazes
scores_in_c <- matrix(NaN,8,23)

for (i in 1:23) { 
  scores_in_col <-Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line]
  scores_in_c[1:length(scores_in_col),i] <-scores_in_col
}

idGroup$Group[c(3,5,6,7,9,10,11,12,13,14,15,17,18,19,20,21,22)]
effect_occupancy_clean[1:2,c(7,15,39,28,38,13,12,35,8,14,20,33,34,9,32,3,19)]

red_biplot_yMaze <- scores_in_c[,c(3,5,6,7,9,10,11,12,13,14,15,17,18,19,20,21,22)]
yMaze_biplot_red <- effect_occupancy_clean[,c(7,15,39,28,38,13,12,35,8,14,20,33,34,9,32,3,19)]

colnames(red_biplot_yMaze) <- colnames(yMaze_biplot_red)

Y_mazes_Tmaze_red<-rbind(yMaze_biplot_red,red_biplot_yMaze)
Y_mazes_Tmaze_red$experiment <- c(rep(1,120),rep(2,8))

colors <- "black" #c("green","red","green","orange","orange","red","red","red","orange","black","black","black","black","black","black","black","black")

setEPS()
postscript("redTmaze_ymazes_biplot.eps")
errorCircles(1:17,1:17,data=Y_mazes_Tmaze_red,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="T-maze scores",
             xlab="Y-mazes scores",main="Y-mazes vs T-maze",ylim=c(-0.6,0.65),xlim=c(-0.20,0.27))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

############################################################
## Joystick and Ymazes
ymazes_biplot_joystick <- effect_occupancy_clean[,c(15,23,34,14,9,13,12,3,31,16)]
joystick_biplot_ymazes <- joystick_screen[1:25,c(-9,-12)]
colnames(ymazes_biplot_joystick) <- colnames(joystick_biplot_ymazes)
ymazes_joystick<-rbind(ymazes_biplot_joystick,joystick_biplot_ymazes)
ymazes_joystick$experiment <- c(rep(1,120),rep(2,25))

colors <- c("red","black","red","black","black","black","black","green","black","black")

setEPS()
postscript("joystick_ymazes_biplot.eps")
errorCircles(1:10,1:10,data=ymazes_joystick,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="Joystick scores",
             xlab="Y-mazes scores",main="Y-mazes vs T-maze",ylim=c(-0.7,0.6),xlim=c(-0.12,0.27))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

##############################################################
## Joystick and T-maze yellow

scores_in_c_yellow <- matrix(NaN,12,28)

for (i in 1:28) { 
  scores_in_col <-Tmaze_yellow$PI[idGroup_yellow$Group[i]==Tmaze_yellow$Fly.line]
  scores_in_c_yellow[1:length(scores_in_col),i] <-scores_in_col
}

colnames(scores_in_c_yellow) <- idGroup_yellow$line
#c(18,17,16,21,26,15,14,9,24,5,12)
idGroup_yellow$line[c(18,15,5,17,26,12,14,9,16,21,24)]
joystick_screen[1:25,-12]

yellow_tmaze<-scores_in_c_yellow[,c(18,15,5,17,26,12,14,9,16,21,24)]
yellow_joystick<-joystick_screen[1:25,-12]
#colnames(yellow_tmaze)<-idGroup_yellow$line[c(18,15,5,17,26,12,14,9,16,21,24)]
#colnames(yellow_joystick)<-idGroup_yellow$line[c(18,15,5,17,26,12,14,9,16,21,24)]

yTmaze_joystick<-rbind(yellow_tmaze,yellow_joystick)
yTmaze_joystick<-as.data.frame(yTmaze_joystick)
yTmaze_joystick$experiment <- c(rep(1,12),rep(2,25))


#colors <- c("red","black","red","black","black","black","black","green","black","red","black")


setEPS()
postscript("Joystick_Tmaze_yellow_biplot.eps")
errorCircles(1:(ncol(yTmaze_joystick)-1),1:(ncol(yTmaze_joystick)-1),data=yTmaze_joystick,group="experiment",paired=TRUE,pch=16,cex=2,colors="black",circles=FALSE,ylab="Joystick scores",
             xlab="T-maze with yellow light",main="Joystick vs T-maze",ylim=c(-0.4,0.4),xlim=c(-0.4,0.4))
abline(a=0,b=0,v=2,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

############################################################
## Tmaze red and yellow

scores_in_c <- matrix(NaN,8,23)

for (i in 1:23) { 
  scores_in_col <-Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line]
  scores_in_c[1:length(scores_in_col),i] <-scores_in_col
}

scores_in_c_yellow <- matrix(NaN,12,28)

for (i in 1:28) { 
  scores_in_col <-Tmaze_yellow$PI[idGroup_yellow$Group[i]==Tmaze_yellow$Fly.line]
  scores_in_c_yellow[1:length(scores_in_col),i] <-scores_in_col
}

#idGroup$Group[c(19,4,22,20,14,18,2,10,9,7,17)]
#idGroup_yellow$line[c(5,6,7,8,11,12,13,14,21,22,23)]
idGroup$Group[c(3,5,7,8,9,10,11,12,13,14,16,18,19,20,21,22)]
idGroup_yellow$line[c(10,18,20,25,8,12,14,2,3,17,16,5,26,27,9,11)]



red_biplot_yellow <- scores_in_c[,c(3,5,7,8,9,10,11,12,13,14,16,18,19,20,21,22)]
yellow_biplot_red <- scores_in_c_yellow[,c(10,18,20,25,8,12,14,2,3,17,16,5,26,27,9,11)]
red_yellow_Tmaze<-rbind(red_biplot_yellow,yellow_biplot_red)
red_yellow_Tmaze<-as.data.frame(red_yellow_Tmaze)
colnames(red_yellow_Tmaze)<-idGroup_yellow$line[c(10,18,20,25,8,12,14,2,3,17,16,5,26,27,9,11)]
red_yellow_Tmaze$experiment <- c(rep(1,8),rep(2,12))

#colors <- c("black","green","black","black","red","black","red","red","orange","orange","black")

setEPS()
postscript("Tmaze_yellowVSred.eps")
errorCircles(1:ncol(yellow_biplot_red),1:ncol(yellow_biplot_red),data=red_yellow_Tmaze,group="experiment",paired=TRUE,pch=16,cex=2,colors="black",circles=FALSE,ylab="Tmaze with yellow light",
             xlab="Tmaze with red light",main="yellow T-maze vs red T-maze",ylim=c(-1,1),xlim=c(-0.6,0.7))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=2,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

############################################################

