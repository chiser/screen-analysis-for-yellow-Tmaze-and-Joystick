library(pwr)

##### Import the csv file into R
Tmaze <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "," )

### Select the PIs from the Tmaze dataframe
fly_line1 <- Tmaze$PI[idGroup$Group[1]==Tmaze$Fly.line]
fly_line2 <- Tmaze$PI[idGroup$Group[2]==Tmaze$Fly.line]

### Get the means to calculate the effect size (d)

#mu1 <- mean(fly_line1)
#mu2 <- mean(fly_line2)

mu1 <- mean(white_effect,na.rm = TRUE)
mu2 <- mean(yellow_effect,na.rm = TRUE)

#both <- c(fly_line1,fly_line2)
both <- c(white_effect,yellow_effect)

n <- length(both)/2

###Cohen suggests that d values of 0.2, 0.5, and 0.8 represent small, medium, and large effect sizes respectively. 

d <- abs(mu1 - mu2)/ sd(both,na.rm = TRUE)


## Perform power test. Accept 0.2 Type II error, and 0.05 Type I error

pwr.t.test(d=d,n=NULL,power=0.8,sig.level=0.05,type="two.sample",alternative="greater")

