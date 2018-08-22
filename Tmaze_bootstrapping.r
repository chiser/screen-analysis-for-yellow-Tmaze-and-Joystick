combined$line <- combined$names
combined$line <- gsub("orange 1", "TH", combined$line)
combined$line <- gsub("orange 2", "mb304b", combined$line)
combined$line <- gsub("orange 3", "mb109b", combined$line)
combined$line <- gsub("orange 4", "58E02", combined$line)
combined$line <- gsub("blue 1", "TH-D'", combined$line)
combined$line <- gsub("blue 2", "TH-D1", combined$line)
combined$line <- gsub("blue 3", "TH-D4", combined$line)
combined$line <- gsub("blue 4", "TH-F3", combined$line)
combined$line <- gsub("light-yellow 1", "Gr28bd+TrpA1", combined$line)
combined$line <- gsub("light-yellow 2", "NP5272", combined$line)
combined$line <- gsub("light-yellow 3", "MZ840", combined$line)
combined$line <- gsub("light-yellow 4", "TH-C'", combined$line)
combined$line <- gsub("dark-green 1", "mb315c", combined$line)
combined$line <- gsub("dark-green 2", "TH-D1", combined$line)
combined$line <- gsub("dark-green 3", "mb299b", combined$line)
combined$line <- gsub("red 1", "5htr1b", combined$line)
combined$line <- gsub("red 2", "TH-G80+TH-G4", combined$line)
combined$line <- gsub("red 3", "mb025b", combined$line)
combined$line <- gsub("red 4", "mb060b", combined$line)
combined$line <- gsub("light-green 1", "mb056b", combined$line)
combined$line <- gsub("light-green 2", "mb312b", combined$line)
combined$line <- gsub("light-green 3", "mb032b", combined$line)
combined$line <- gsub("light-green 4", "mb065b", combined$line)
combined$line <- gsub("dark-yellow 1", "NP47", combined$line)
combined$line <- gsub("dark-yellow 2", "NP1528", combined$line)
combined$line <- gsub("dark-yellow 3", "TH-C1+TH-F1", combined$line)
combined$line <- gsub("dark-yellow 4", "DDC(HL9)", combined$line)
combined$line <- gsub("pink 1", "MZ19", combined$line)
combined$line <- gsub("pink 2", "TH-F1", combined$line)
combined$line <- gsub("pink 3", "TH-F2", combined$line)
combined$line <- gsub("pink 4", "DDC(HL8)", combined$line)
combined$line <- gsub("white 1", "TH-G1", combined$line)
combined$line <- gsub("white 2", "NP6510", combined$line)

plot(combined$waverage,combined$waverageGAIA)
text(combined$waverage,combined$waverageGAIA,combined$line)
corr<-lm(combined$waverage ~combined$waverageGAIA)
summary(corr)
text(0,-0.6,"adj Rsq 0.0086")
abline(0,100000000000000000000000000000000000000000000000000000000000000000000000000000000)


gaia <- Gaia...Foglio1[,1:4]

PI_1 <- matrix(NA,10,nrow(idGroup2))

for (i in seq_len(nrow(idGroup2))) { 
    PI_1[1:length(Tmaze$PI[idGroup2$Group[i]==Tmaze$Fly.line]),i] <- Tmaze$PI[idGroup2$Group[i]==Tmaze$Fly.line]
}


mean_PI <- matrix(NA,10,nrow(idGroup2))

for (oo in 1:nrow(mean_PI)){

resampled_PI_1 <- matrix(NA,3,nrow(idGroup2))
for (i in seq_len(nrow(idGroup2))) { 
  resampled_PI_1[,i] <- sample(PI_1[!is.na(PI_1[,i]),i],3)
}

mean_PI[oo,] <- colMeans(resampled_PI_1)

}

mean_bootstrapped_PI <- colMeans(mean_PI)
sd_bootstrapped_PI <- apply(mean_PI,2,sd)
se_bootstrapped_PI <- sd_bootstrapped_PI/sqrt(nrow(mean_PI))

barCenters <- barplot(height = mean_bootstrapped_PI,
                      names.arg = as.character(idGroup2$Group),
                      beside = true, las = 2,ylim=c(-1,1),
                      cex.names = 0.75,
                      main = "Screen T-maze bootstrapped",
                      ylab = "PI",
                      border = "black", axes = TRUE)
segments(barCenters, mean_bootstrapped_PI - se_bootstrapped_PI, barCenters,
         mean_bootstrapped_PI + se_bootstrapped_PI, lwd = 1.5)


# Bootstrap 95% CI for R-Squared
#library(boot)
# function to obtain R-Squared from the data
#rsq <- function(formula, data, indices) {
#  d <- data[indices,] # allows boot to select sample
#  fit <- lm(formula, data=d)
#  return(summary(fit)$r.square)
#}
# bootstrapping with 1000 replications
#results <- boot(data=mtcars, statistic=rsq,
#                R=1000, formula=mpg~wt+disp)
# view results
#results
#plot(results)
# get 95% confidence interval
#boot.ci(results, type="bca")

