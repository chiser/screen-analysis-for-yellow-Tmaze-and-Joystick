Gaia...Foglio1 <- read.csv("C:/Users/LocalAdmin/Downloads/Gaia - Foglio1.csv")
Naman...Foglio1 <- read.csv("C:/Users/LocalAdmin/Downloads/Naman - Foglio1.csv")
Tmaze_yellow <- c(as.character(Naman...Foglio1$X),as.character(Gaia...Foglio1$X))
Tmaze_yellow <- as.data.frame(Tmaze_yellow)
Tmaze_yellow$L <- c(Naman...Foglio1$L,Gaia...Foglio1$L)
Tmaze_yellow$M <- c(Naman...Foglio1$M,Gaia...Foglio1$M)
Tmaze_yellow$D <- c(Naman...Foglio1$D,Gaia...Foglio1$D)
Tmaze_yellow <-screen.data...Foglio1
Tmaze <- Tmaze_yellow
### Run the Tmazeplotting code to get PIs and other stuff
#idGroup <- data.frame("Group"=levels(Tmaze[[1]]),"Treatment"= ifelse(grepl("yellow 1", levels(Tmaze[[1]]), ignore.case = T), "positive control", "Experimental"),
#                      "Colour"=ifelse(grepl("yellow 1", levels(Tmaze[[1]]), ignore.case = T), "white", "darkgreen"))
idGroup$line <- idGroup$Group
idGroup$line <- gsub("orange 1", "TH", idGroup$line)
idGroup$line <- gsub("orange 2", "mb304b", idGroup$line)
idGroup$line <- gsub("orange 3", "mb109b", idGroup$line)
idGroup$line <- gsub("orange 4", "58E02", idGroup$line)
idGroup$line <- gsub("blue 1", "TH-D'", idGroup$line)
idGroup$line <- gsub("blue 2", "TH-D1", idGroup$line)
idGroup$line <- gsub("blue 3", "TH-D4", idGroup$line)
idGroup$line <- gsub("blue 4", "TH-F3", idGroup$line)
idGroup$line <- gsub("light-yellow 1", "Gr28bd+TrpA1", idGroup$line)
idGroup$line <- gsub("light-yellow 2", "NP5272", idGroup$line)
idGroup$line <- gsub("light-yellow 3", "MZ840", idGroup$line)
idGroup$line <- gsub("light-yellow 4", "TH-C'", idGroup$line)
idGroup$line <- gsub("dark-green 1", "mb315c", idGroup$line)
idGroup$line <- gsub("dark-green 2", "TH-D1", idGroup$line)
idGroup$line <- gsub("dark-green 3", "mb299b", idGroup$line)
idGroup$line <- gsub("red 1", "5htr1b", idGroup$line)
idGroup$line <- gsub("red 2", "TH-G80+TH-G4", idGroup$line)
idGroup$line <- gsub("red 3", "mb025b", idGroup$line)
idGroup$line <- gsub("red 4", "mb060b", idGroup$line)
idGroup$line <- gsub("light-green 1", "mb056b", idGroup$line)
idGroup$line <- gsub("light-green 2", "mb312b", idGroup$line)
idGroup$line <- gsub("light-green 3", "mb032b", idGroup$line)
idGroup$line <- gsub("light-green 4", "mb065b", idGroup$line)
idGroup$line <- gsub("dark-yellow 1", "NP47", idGroup$line)
idGroup$line <- gsub("dark-yellow 2", "NP1528", idGroup$line)
idGroup$line <- gsub("dark-yellow 3", "TH-C1+TH-F1", idGroup$line)
idGroup$line <- gsub("dark-yellow 4", "DDC(HL9)", idGroup$line)
idGroup$line <- gsub("pink 1", "MZ19", idGroup$line)
idGroup$line <- gsub("pink 2", "TH-F1", idGroup$line)
idGroup$line <- gsub("pink 3", "TH-F2", idGroup$line)
idGroup$line <- gsub("pink 4", "DDC(HL8)", idGroup$line)
idGroup$line <- gsub("white 1", "TH-G1", idGroup$line)
idGroup$line <- gsub("white 2", "NP6510", idGroup$line)

#idGroup$line <-ifelse(grepl("orange 1", idGroup$Group, ignore.case = T), "Genetic control", ifelse (grepl("orange 2", idGroup$Group, ignore.case = T), "positive control", "Genetic Control"))

setEPS()
postscript("T-maze_yellow_Christian_final.eps")
barCenters <- barplot(height = idGroup$mean,
                      names.arg = as.character(idGroup$line),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Screen T-maze with yellow light",
                      ylab = "PI",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = line_names_clean$V1, xpd = TRUE)

segments(barCenters, idGroup$mean - idGroup$se, barCenters,
         idGroup$mean + idGroup$se, lwd = 1.5)

#arrows(barCenters, line_names_clean$scores - line_names_clean$se * 2, barCenters,
#       line_names_clean$scores + line_names_clean$se * 2, lwd = 1.5, angle = 90,
#       code = 3, length = 0.05)

dev.off()
########################################################################################################################

joystick<- colnames(SCREEn_effectsize)
joystick<-data.frame(joystick)
joystick$mean <- apply(SCREEn_effectsize,2, function(x) mean(x,na.rm=TRUE))
joystick$sd <- apply(SCREEn_effectsize,2, function(x) sd(x,na.rm=TRUE))
joystick$n <- apply(SCREEn_effectsize,2, function(x) length(x[!is.na(x)]))
joystick$se <- joystick$sd/sqrt(joystick$n)

joystick$line <- joystick$joystick
joystick$line <- gsub("orange1", "TH", joystick$line)
joystick$line <- gsub("orange2", "mb304b", joystick$line)
joystick$line <- gsub("orange3", "mb109b", joystick$line)
joystick$line <- gsub("orange4", "58E02", joystick$line)
joystick$line <- gsub("blue1", "TH-D'", joystick$line)
joystick$line <- gsub("blue2", "TH-D1", joystick$line)
joystick$line <- gsub("blue3", "TH-D4", joystick$line)
joystick$line <- gsub("blue4", "TH-F3", joystick$line)
joystick$line <- gsub("lightyellow1", "Gr28bd+TrpA1", joystick$line)
joystick$line <- gsub("lightyellow2", "NP5272", joystick$line)
joystick$line <- gsub("lightyellow3", "MZ840", joystick$line)
joystick$line <- gsub("lightyellow4", "TH-C'", joystick$line)
joystick$line <- gsub("darkgreen1", "mb315c", joystick$line)
joystick$line <- gsub("darkgreen2", "TH-D1", joystick$line)
joystick$line <- gsub("darkgreen3", "mb299b", joystick$line)
joystick$line <- gsub("red1", "5htr1b", joystick$line)
joystick$line <- gsub("red2", "TH-G80+TH-G4", joystick$line)
joystick$line <- gsub("red3", "mb025b", joystick$line)
joystick$line <- gsub("red4", "mb060b", joystick$line)
joystick$line <- gsub("lightgreen1", "mb056b", joystick$line)
joystick$line <- gsub("lightgreen2", "mb312b", joystick$line)
joystick$line <- gsub("lightgreen3", "mb032b", joystick$line)
joystick$line <- gsub("lightgreen4", "mb065b", joystick$line)
joystick$line <- gsub("darkyellow1", "th", joystick$line)
joystick$line <- gsub("darkyellow2", "NP1528", joystick$line)
joystick$line <- gsub("darkyellow3", "TH-C1+TH-F1", joystick$line)
joystick$line <- gsub("darkyellow4", "DDC(HL9)", joystick$line)
joystick$line <- gsub("pink1", "MZ19", joystick$line)
joystick$line <- gsub("pink2", "TH-F1", joystick$line)
joystick$line <- gsub("pink3", "TH-F2", joystick$line)
joystick$line <- gsub("pink4", "DDC(HL8)", joystick$line)
joystick$line <- gsub("white1", "TH-G1", joystick$line)
joystick$line <- gsub("white2", "NP6510", joystick$line)
joystick$line <- gsub("positivecontrol1", "Gr28bd+TrpA1", joystick$line)

new_order <- order(joystick$mean, decreasing = T)
joystick <- joystick[new_order,]

#joystick$Treatment <- c(rep("Experimental",11),"positive control") 
#joystick <- joystick[order(joystick$Treatment), ]
#joystick$rank <- (joystick$Treatment!="66")*1

new_order <- order(joystick$mean, decreasing = T)
joystick <- joystick[new_order,]
joystick$rank <- grepl("TrpA1", joystick$line)*1
new_order <- order(joystick$rank, decreasing = T)
joystick <- joystick[new_order,]


setEPS()
postscript("Joystick_effectsize_rightnames.eps")
barCenters <- barplot(height = joystick$mean,
                      names.arg = as.character(joystick$line),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Screen Joystick effectsize with yellow light",
                      ylab = "Reinforcement (PI training - PI pretest)",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#     adj = 1, labels = line_names_clean$V1, xpd = TRUE)

segments(barCenters, joystick$mean - joystick$se, barCenters,
         joystick$mean + joystick$se, lwd = 1.5)

#arrows(barCenters, line_names_clean$scores - line_names_clean$se * 2, barCenters,
#       line_names_clean$scores + line_names_clean$se * 2, lwd = 1.5, angle = 90,
#       code = 3, length = 0.05)

dev.off()

#########################################################################################################################

library("psych", lib.loc="~/R/win-library/3.4")

#idGroup$ymazes <- line_names$scores[1:23]

plot(idGroup$ymazes ~ idGroup$mean)

idGroup$mean <- NULL
mean <- NULL

scores_in_c <- matrix(NaN,8,23)

for (i in 1:23) { 
  scores_in_col <-Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line]
  scores_in_c[1:length(scores_in_col),i] <-scores_in_col
}


colors <- c("green","red","green","orange","orange","red","red","red","orange","black","black","black","black","black","black","black","black")
#driver_group <- c("Positive classical","Negative classical","Positive classical","Neutral classical","Neutral classical","Negative classical","Negative classical","Neutral classical","Arousal-sleep","Arousal-sleep","Arousal-sleep","Arousal-sleep","Arousal-sleep","Arousal-sleep","Arousal-sleep","Arousal-sleep")

errorCircles(1:17,1:17,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="T-maze scores",
             xlab="Y-mazes scores",main="Y-mazes vs T-maze",ylim=c(-0.6,0.65),xlim=c(-0.20,0.27))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured


############################################################

## Tmaze red and Ymazes
colnames(scores_in_c) <- colnames(scores[,c(3,1,5,6,5,5,5,20,22,28,26,30,32,5,5,36,39,38,40,41,43,37,42)])

#scores2<-rbind(scores[,c(3,1,5,6,5,5,5,20,22,28,26,30,32,5,5,36,39,38,40,41,43,37,42)],scores_in_c)
#scores2$experiment <- c(rep(1,120),rep(2,8))
#scores2<-scores2[,c(-3,-5,-6,-7,-14,-15)]

############################################################
## Joystick and Ymazes
ymazes_biplot_joystick <- effect_occupancy_clean[,c(15,23,34,14,9,13,12,3,31,16)]
joystick_biplot_ymazes <- SCREEn_effectsize[1:25,-9]
scores2<-rbind(ymazes_biplot_joystick,joystick_biplot_ymazes)
scores2$experiment <- c(rep(1,120),rep(2,25))

colors <- c("red","black","red","black","black","black","black","green","black","black")


setEPS()
postscript("joystick_ymazes_biplot.eps")
errorCircles(1:10,1:10,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="Joystick scores",
             xlab="Y-mazes scores",main="Y-mazes vs T-maze",ylim=c(-0.7,0.6),xlim=c(-0.12,0.27))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

fit_effect<-lm(idGroup$mean[c(21,3,13,19,11,10,14,5,22,15,7,20,17,18,12,9,6)] ~ line_names_clean$means[c(3,7,8,9,12,13,14,15,19,20,28,32,33,34,35,38,39)])
summary(fit_effect)
##############################################################
## Joystick and T-maze yellow

scores_in_c_yellow <- matrix(NaN,12,28)

for (i in 1:28) { 
  scores_in_col <-Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line]
  scores_in_c_yellow[1:length(scores_in_col),i] <-scores_in_col
}

idGroup$line[c(18,17,16,21,26,15,14,9,24,5,12)]

yellow_tmaze<-scores_in_c_yellow[,c(18,17,16,21,26,15,14,9,24,5,12)]
yellow_joystick<-SCREEn_effectsize[1:25,-12]
colnames(yellow_tmaze)<-idGroup$line[c(18,17,16,21,26,15,14,9,24,5,12)]
colnames(yellow_joystick)<-idGroup$line[c(18,17,16,21,26,15,14,9,24,5,12)]

scores2<-rbind(yellow_tmaze,yellow_joystick)
scores2<-as.data.frame(scores2)
scores2$experiment <- c(rep(1,12),rep(2,25))


colors <- c("red","black","red","black","black","black","black","green","black","red","black")


setEPS()
postscript("Joystick_Tmaze_yellow_biplot.eps")
errorCircles(1:11,1:11,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="Joystick scores",
             xlab="T-maze with yellow light",main="Joystick vs T-maze",ylim=c(-0.5,0.6),xlim=c(-0.4,0.4))
abline(a=0,b=0,v=2,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

correl_effects <- lm(idGroup$mean[c(18,17,16,21,26,15,14,9,24,5,12)]~joystick$mean[-1])
summary(correl_effects)
############################################################
## Tmaze red and yellow

scores_in_c <- matrix(NaN,8,23)

for (i in 1:23) { 
  scores_in_col <-Tmaze$PI[idGroup$Group[i]==Tmaze$Fly.line]
  scores_in_c[1:length(scores_in_col),i] <-scores_in_col
}

scores_in_c_yellow <- matrix(NaN,12,24)

for (i in 1:24) { 
  scores_in_col <-Tmaze_yellow$PI[idGroup_yellow$Group[i]==Tmaze_yellow$Fly.line]
  scores_in_c_yellow[1:length(scores_in_col),i] <-scores_in_col
}

idGroup$Group[c(19,4,22,20,14,18,2,10,9,7,17)]
idGroup_yellow$line[c(5,6,7,8,11,12,13,14,21,22,23)]

red_biplot_yellow <- scores_in_c[,c(19,4,22,20,14,18,2,10,9,7,17)]
yellow_biplot_red <- scores_in_c_yellow[,c(5,6,7,8,11,12,13,14,21,22,23)]
scores2<-rbind(red_biplot_yellow,yellow_biplot_red)
scores2<-as.data.frame(scores2)
colnames(scores2)<-idGroup_yellow$line[c(5,6,7,8,11,12,13,14,21,22,23)]
scores2$experiment <- c(rep(1,8),rep(2,12))

colors <- c("black","green","black","black","red","black","red","red","orange","orange","black")


setEPS()
postscript("Tmaze_yellowVSred.eps")
errorCircles(1:11,1:11,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="Tmaze with yellow light",
             xlab="Tmaze with red light",main="Y-mazes vs T-maze",ylim=c(-1,1),xlim=c(-0.6,0.7))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=2,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()

############################################################

# Save in vector graphics
#svg(filename="Std_SVG.svg", 
#    width=5, 
#    height=4, 
#    pointsize=12)
#plot( scores_indiv ~ df$V17)
#dev.off()

setEPS()
postscript("whatever.eps")
errorCircles(1:10,1:10,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,circles=FALSE,ylab="Joystick scores",
             xlab="Y-mazes scores",main="Y-mazes vs T-maze",ylim=c(-0.7,0.6),xlim=c(-0.12,0.27))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=1,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()
#postscript("foo.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
#cairo_ps("image.eps")
#plot(1, 10)
#dev.off()

setEPS()
postscript("Y-mazes_screen_occu2.eps")
barCenters <- barplot(height = line_names_clean$scores,
                      names.arg = as.character(line_names_clean$V1),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Occupancy screen Y-mazes",
                      ylab = "Occupancy rate",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = line_names_clean$V1, xpd = TRUE)

segments(barCenters, line_names_clean$scores - line_names_clean$se * 2, barCenters,
         line_names_clean$scores + line_names_clean$se * 2, lwd = 1.5)

#arrows(barCenters, line_names_clean$scores - line_names_clean$se * 2, barCenters,
#       line_names_clean$scores + line_names_clean$se * 2, lwd = 1.5, angle = 90,
#       code = 3, length = 0.05)

dev.off()

