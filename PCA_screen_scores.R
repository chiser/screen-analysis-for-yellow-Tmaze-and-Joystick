## Importing the data

Joystick_screen_results <- read.delim("C:/Users/LocalAdmin/Desktop/Joystick_screen_results.txt")
Occupancy_ymazes_screen_results <- read.delim("C:/Users/LocalAdmin/Desktop/Occupancy_ymazes_screen_results.txt")
Tmaze_screen_results <- read.delim("C:/Users/LocalAdmin/Desktop/Tmaze_screen_results.txt")
Tmaze_red_screen_results <- read.delim("C:/Users/LocalAdmin/Desktop/Tmaze_red_screen_results.txt")


## Throwing away unintersting controls
ymazes_screen<-Occupancy_ymazes_screen_results[c(-5,-24,-25,-29,-34,-35,-48),]

## Changing names so that all have same nomenclature
#Joystick_screen_final$final_names <- Joystick_screen_final$real_names
#Joystick_screen_final$final_names <- gsub("mb315c", "MB315C", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb312b", "MB312B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb304b", "MB304B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb299b", "MB299B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb109b", "MB109B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb065b", "MB065B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb056b", "MB056B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("mb025b", "MB025B", Joystick_screen_final$final_names)
#Joystick_screen_final$final_names <- gsub("5htr1b", "5HTR1B", Joystick_screen_final$final_names)




## Reorder all in alphabetic order
new_order <- order(ymazes_screen$names, decreasing = T)
ymazes_screen_final <- ymazes_screen[new_order,]

new_order <- order(Joystick_screen_results$real_names, decreasing = T)
Joystick_screen_final <- Joystick_screen_results[new_order,]

new_order <- order(Tmaze_red_screen_results$names, decreasing = T)
Tmaze_screenred_final <- Tmaze_red_screen_results[new_order,]

new_order <- order(Tmaze_screen_results$line, decreasing = T)
Tmaze_screenyellow_final <- Tmaze_screen_results[new_order,]


## Checking if they are correctly ordered
Joystick_screen_final$real_names
ymazes_screen_final$names
Tmaze_screenred_final$names
Tmaze_screenyellow_final$line


## Adding slots with NA to match every entrance

#total_lines<-NULL

total_lines<-c("TH-G4+TH-G80","TH-G4+Cha-G80","TH-G1","TH-F3","TH-F2","TH-F1","TH-D4","TH-D1","TH-D'","TH-C1","TH-C'","TH-C1+TH-F1","TH","NP6510NP5272","NP6510","NP5272MZ840","NP5272","NP47+Cha-G80","NP47","NP1528","MZ840NP6510","MZ840","MZ19+Cha-G80","MZ19","MB439B","MB438B","MB315C","MB312B","MB304B","MB301B","MB299C","MB109B","MB065B","MB060B","MB058B","MB056B","MB032B","MB025B","Gr66a(Co)","Gr66a","Gr28bd+TrpA1(Co)","Gr28bd+TrpA1","DDC(HL9)","DDC(HL8)","58E02+TH-G80","58E02","5-HTR1B","5-HTR1B+Cha-G80") 

#<-data.frame(NA,48,4,colnames(c("Joystick","Ymazes","Tmaze_red","Tmaze_yellow")))

#total_means <- data.frame(
#  Joystick=character(),
#  Ymazes=numeric(),
#  Tmaze_red=numeric(),
#  Tmaze_yellow=numeric(),
#  stringsAsFactors=FALSE
#)

total_means<-matrix(NA,48,4)
total_se<-matrix(NA,48,4)
total_n<-matrix(NA,48,4)

for (i in 1:length(total_lines)){
    
  total_means[i,c(any(total_lines[i]==Joystick_screen_final$real_names),any(total_lines[i]==ymazes_screen_final$names),any(total_lines[i]==Tmaze_screenred_final$names),
  any(total_lines[i]==Tmaze_screenyellow_final$line))]<-c(Joystick_screen_final$mean_effectsize[total_lines[i]==Joystick_screen_final$real_names],
  ymazes_screen_final$mean[total_lines[i]==ymazes_screen_final$names],Tmaze_screenred_final$mean[total_lines[i]==Tmaze_screenred_final$names],Tmaze_screenyellow_final$mean[total_lines[i]==Tmaze_screenyellow_final$line])
  
  total_se[i,c(any(total_lines[i]==Joystick_screen_final$real_names),any(total_lines[i]==ymazes_screen_final$names),any(total_lines[i]==Tmaze_screenred_final$names),
                  any(total_lines[i]==Tmaze_screenyellow_final$line))]<-c(Joystick_screen_final$std_err_effectsize[total_lines[i]==Joystick_screen_final$real_names],
                                                                          ymazes_screen_final$se[total_lines[i]==ymazes_screen_final$names],Tmaze_screenred_final$se[total_lines[i]==Tmaze_screenred_final$names],Tmaze_screenyellow_final$se[total_lines[i]==Tmaze_screenyellow_final$line])
  
  total_n[i,c(any(total_lines[i]==Joystick_screen_final$real_names),any(total_lines[i]==ymazes_screen_final$names),any(total_lines[i]==Tmaze_screenred_final$names),
  any(total_lines[i]==Tmaze_screenyellow_final$line))]<-c(Joystick_screen_final$used_traces[total_lines[i]==Joystick_screen_final$real_names],
                                                                          ymazes_screen_final$n[total_lines[i]==ymazes_screen_final$names],Tmaze_screenred_final$n[total_lines[i]==Tmaze_screenred_final$names],Tmaze_screenyellow_final$n[total_lines[i]==Tmaze_screenyellow_final$line])
  
}

colnames(total_means)<-c("Joystick","Ymazes","Tmaze_red","Tmaze_yellow")
rownames(total_means)<-total_lines
colnames(total_se)<-c("Joystick","Ymazes","Tmaze_red","Tmaze_yellow")
rownames(total_se)<-total_lines
colnames(total_n)<-c("Joystick","Ymazes","Tmaze_red","Tmaze_yellow")
rownames(total_n)<-total_lines


### Save data
#write.table(total_means, "Means_experiments.txt", sep="\t", row.names = TRUE,col.names = TRUE)
#write.table(total_se, "se_experiments.txt", sep="\t", row.names = TRUE,col.names = TRUE)
#write.table(total_n, "n_experiments.txt", sep="\t", row.names = TRUE,col.names = TRUE)

## Check graphically how the lines cluster in the four phenotypes

plot(total_means[,1:2])
text(total_means[,1],total_means[,2],rownames(total_means))

plot(total_means[,3:4])
text(total_means[,3],total_means[,4],rownames(total_means))

plot(total_means[,2:3])
text(total_means[,2],total_means[,3],rownames(total_means))

library(rgl)
plot3d(total_means[,1:3], col=length(rownames(total_means)),size=10)
text3d(total_means[,1:3],texts=rownames(total_means),cex=0.7,font=2)
#dir.create("animation")
#for (i in 1:90) {
#  view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, -1))
#  rgl.snapshot(filename=paste("animation/frame-",
#                              sprintf("%03d", i), ".png", sep=""))
#}

plot3d(total_means[,c(1,2,4)], col=length(rownames(total_means)),size=10)
text3d(total_means[,c(1,2,4)],texts=rownames(total_means),cex=0.7,font=2)

## Check discretely phenotypes

discrete_phenotype<-matrix(NA,nrow(total_means),2)

for (i in 1:nrow(total_means)){
  discrete_phenotype[i,]<-c(sum(total_means[i,]>0,na.rm=TRUE),sum(total_means[i,]<0,na.rm=TRUE))
}

row.names(discrete_phenotype)<-row.names(total_means)

hist(discrete_phenotype,breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5))

### From here the interesting lines for appetitive are: TH-G1 (4),TH-D1 (4),HL9 (3), TH-C1 (3)
### From here the intersting lines for aversive are: TH-D' (4), MB025B (3)

#hist(total_means[,1])
#hist(total_means[,2])
#hist(total_means[,3])
#hist(total_means[,4])

Joyst_sample<-sample(total_means[,1],size=48, replace=TRUE)
Ymazes_sample<-sample(total_means[,2],size=48, replace=TRUE)
redT_sample<-sample(total_means[,3],size=48, replace=TRUE)
yellowT_sample<-sample(total_means[,4],size=48, replace=TRUE)

sampled_phenotypes<-cbind(Joyst_sample,Ymazes_sample,redT_sample,yellowT_sample)

disc_sample_phenotype<-matrix(NA,nrow(total_means),2)

for (i in 1:nrow(sampled_phenotypes)){
  disc_sample_phenotype[i,]<-c(sum(sampled_phenotypes[i,]>0,na.rm=TRUE),sum(sampled_phenotypes[i,]<0,na.rm=TRUE))
}

hist(disc_sample_phenotype,breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5))

## Since the data is very sparse because many lines were not tested in all the setups we need to fill the gaps by interpolation with DINEOF
library(devtools)
#install_github("marchtaylor/sinkr")
library(sinkr)

### Make data
#color palette
pal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))

### Interpolation, EOF, and Reconstruction ###
#Interpolation with DINEOF

set.seed(1)
din <- dineof(total_means)
Xa <- din$Xa 
image(Xa, col=pal(100))

# EOF
#Et <- eof(Xp) # true
El <- eof(total_means) # obs, lseof (Empirical Orthogonal Function Analysis)
Er <- eof(total_means, recursive=TRUE) # obs, rseof. This one should be in theory better that the previous one (non recursive one)
Ed <- eof(Xa) # obs, dineof + lseof . This is doing the interpolation with dineof

###Reconstruction
VALS <- which(total_means == 0)
#Rt <- eofRecon(Et)
Rl <- eofRecon(El)  # Reconstructing the original field from an EOF object. LSEOF
Rr <- eofRecon(Er)  # RSEOF
Rd <- eofRecon(Ed)  # DINEOF + LSEOF

###Plot to see how the data looks after applying the techniques for data with gaps
#png(file="eof_interpolation.png", width=7, height=4.5, res=400, units="in", type="cairo")
op <- par(mfrow=c(2,2), mar=c(3,3,2,2), ps=10, bg="white")
#image(Xt, col=pal(100))
#mtext("True", side=3, line=0.5)
#image(Xp, col=pal(100))
#mtext("True + noise", side=3, line=0.5)
image(total_means, col=pal(100))
mtext("Observed", side=3, line=0.5)
image(Rl, col=pal(100))
mtext("LSEOF recon", side=3, line=0.5)
image(Rr, col=pal(100))
mtext("RSEOF recon", side=3, line=0.5)
image(Rd, col=pal(100))
mtext("DINEOF recon", side=3, line=0.5)
par(op)
#dev.off()

## Now check by means of all of the phenotypes. But for this we might need to whiten it.

# load whitening library
library("whitening")

# estimate covariance
S = cov(Rd)

# ZCA-cor whitening matrix
W.ZCAcor = whiteningMatrix(S, method="ZCA-cor")

# directly compute whitened data from X
Z.ZCAcor.2 = whiten(Rd, method="ZCA-cor")
zapsmall( cov(Z.ZCAcor.2) )

mean_phenotype<-rowMeans(total_means,na.rm = TRUE)
mean_phenotype_white<-rowMeans(Z.ZCAcor.2)

boxplot(mean_phenotype~total_lines,las=2,main="Without withening")
boxplot(mean_phenotype_white~total_lines,las=2,main="With withening")

### Start the PCA analysis
# Now that data is full we can apply the PCA to the data. We decided to use the data processed with DINEOF because according to literature it yields better results
dineof.pca <- prcomp(Rd,
                 center = TRUE,
                 scale. = TRUE) 

# plot method
#plot(dineof.pca, type = "l")
#screeplot(dineof.pca, main="Scree Plot", xlab="Components")
screeplot(dineof.pca, main="Scree Plot", type="line" )

# summary method
summary(dineof.pca)

# Predict PCs
#predict(dineof.pca)

#install_github("ggbiplot", "vqv")

#library(ggbiplot)
#g <- ggbiplot(dineof.pca, obs.scale = 1, var.scale = 1, 
#               ellipse = TRUE, 
#              circle = TRUE)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', 
#               legend.position = 'top')
#print(g)

library(lattice)
levelplot(dineof.pca$rotation)  ## From this result it seems like Joystick, Ymazes and Yellow Tmaze seems to be correlated. Maybe an artifact for the number of interpolated blanks from dineof
## Then Joystick and red Tmaze seem to be more similar together and Ymazes results are more similar to yellow Tmaze

# Now draw the BiPlot
biplot(dineof.pca, cex=c(0.5, 0.7))

pca.scores<-NULL
scores<-NULL
for (i in 1:ncol(dineof.pca$rotation)){
  scores<- apply(Rd,1,function(x)sum(x*dineof.pca$rotation[,i]))
  pca.scores<-cbind(pca.scores,scores)
}

pca.scores<-as.data.frame(pca.scores)
colnames(pca.scores)<-c("PC1","PC2","PC3","PC4")

# Plot the driver lines in the three first PCs
plot3d(pca.scores[,1:3], col=length(rownames(Rd)),size=10)

text3d(pca.scores[,1:3],texts=rownames(Rd),cex=0.7,font=2)
text3d(dineof.pca$rotation[,1:3], texts=attributes(dineof.pca$rotation)$dimnames[[1]], col="red",cex=0.7,font=2)

coords <- NULL
for (i in 1:nrow(dineof.pca$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),dineof.pca$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=1)


## Correlation table from the results from the setups. There does not seem to be strong correlations, thus different phenotypes in different setups. Joystick is slightly positively correlated to Ymazes and yellow Tmaze

my.abs     <- cor(Rd)
#my.colors  <- dmat.color(my.abs)
my.colors2  <- dmat.color(my.abs, colors = cm.colors(10, alpha = 1))
my.ordered <- order.single(cor(Rd))
cpairs(Rd, my.ordered, panel.colors=my.colors2, gap=0.5)
levelplot(my.abs)

#heat.colors(n, alpha = 1)
#terrain.colors(n, alpha = 1)
#topo.colors(n, alpha = 1)
#cm.colors(n, alpha = 1)
#c("darkgreen","white","red")
# Lets see what the Rsquared is, to make sure that there is no correlation. There is nothing to say, so it seems that it is all just uncorrelated noise

joyst_ymazes<-lm(Rd[,1]~Rd[,2])
summary(joyst_ymazes)    ## 0.14 Rsquared

joyst_yellowT<-lm(Rd[,1]~Rd[,4])
summary(joyst_yellowT)   ## 0.07 Rsquared



### Screen normal barplots

barCenters <- barplot(height = total_means[,1],
                      names.arg = as.character(total_lines),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Screen Joystick effectsize with yellow light",
                      ylab = "Reinforcement (PI training - PI pretest)",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#     adj = 1, labels = line_names_clean$V1, xpd = TRUE)

segments(barCenters, total_means[,1] - total_se[,1], barCenters,
         total_means[,1] + total_se[,1], lwd = 1.5)

### Barplot in groups. Here we see how TH-D1 and TH-D' are the most interesting
library(lattice)
barchart(total_means, 
         scales=list(x=list(rot=90,cex=0.8)))

legend(1,0,c("Joystick","Ymazes","red_Tmaze","yellow_Tmaze"))
### Now the biplots: but for that I need each individual experiment, not just the means

scores2<-as.data.frame(cbind(total_means[,1],total_means[,2]))
scores2$se1<-total_se[,1]
scores2$se2<-total_se[,2]

scores<-scores2[(!is.na(scores2$se1))&(!is.na(scores2$se2)),]

ggplot(data = scores,aes(x = V1,y = V2)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = V1-se1,xmax = V1-se1)) +
  geom_errorbar(aes(ymin = V2-se2,ymax = V2+se2))





