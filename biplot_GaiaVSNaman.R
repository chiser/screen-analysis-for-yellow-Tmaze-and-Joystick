## Tmaze red and yellow

id_Christian <- order(idGroup_Christian$Group)
id_Gaia <- order(idGroup_Gaia$Group)
id_Naman <- order(idGroup_Naman$Group)
id_all <- order(idGroup$Group)

idGroup_Christian_ordered<-idGroup_Christian[id_Christian,]
idGroup_Gaia_ordered<-idGroup_Gaia[id_Gaia,]
idGroup_Naman_ordered<-idGroup_Naman[id_Naman,]
idGroup_all_ordered<-idGroup[id_all,]

new_Gaia<-idGroup_Gaia_ordered[c(-2,-7,-15,-28,-29,-31),]
new_Naman<-idGroup_Naman_ordered[-23,]
#new_Christian<-idGroup_Christian_ordered[-7,]
#new_Gaia<-idGroup_Gaia_ordered[c(2,11,15,16,20,21,28,31),]

new_Gaia$Group
new_Naman$Group

scores_in_c <- matrix(NaN,9,25)

for (i in 1:25) { 
  scores_in_col <-Tmaze_Gaia$PI[new_Gaia$Group[i]==Tmaze_Gaia$Fly.line]
  scores_in_c[1:length(scores_in_col),i] <-scores_in_col
}

scores_in_c_yellow <- matrix(NaN,9,25)

for (i in 1:25) { 
  scores_in_col <-Tmaze_Naman$PI[new_Naman$Group[i]==Tmaze_Naman$Fly.line]
  scores_in_c_yellow[1:length(scores_in_col),i] <-scores_in_col
}


scores2<-rbind(scores_in_c,scores_in_c_yellow)
scores2<-as.data.frame(scores2)
colnames(scores2)<-new_Gaia$line
scores2$experiment <- c(rep(1,9),rep(2,9))

colors <- c("black","green","black","black","red","black","red","red","orange","orange","black")


setEPS()
postscript("Tmaze_GaiaVSNaman.eps")
errorCircles(1:25,1:25,data=scores2,group="experiment",paired=TRUE,pch=16,cex=2,colors=colors,circles=FALSE,ylab="Tmaze with Naman",
             xlab="Tmaze with Gaia",main="Naman vs Gaia",ylim=c(-1,1),xlim=c(-0.6,0.7))
abline(a=0,b=0,v=1,h=0,lwd =3 )
abline(a=0,b=0,v=0,h=2,lwd=3 )

rect(0, 0, 3, 4, col= rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
rect(0, 0, -3, -4, col = rgb(0,0,1.0,alpha=0.1), border = "transparent") # coloured
dev.off()