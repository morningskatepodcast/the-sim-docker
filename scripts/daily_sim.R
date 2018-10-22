#!/usr/bin/env Rscript

library(plyr)
library(dplyr)
library(jsonlite)
library(readr)

# Load all models from folder
setwd("/src/scripts")
pOwn <- readRDS('models/own_projection_step_1.rds')
pOwnc <- readRDS('models/own_projection_step_2.rds')
m.p0 <- readRDS('models/score_zero.rds')
m.p1 <- readRDS('models/score_one.rds')
m.p2 <- readRDS('models/score_two.rds')
m.p3 <- readRDS('models/score_three.rds')
m.p4 <- readRDS('models/score_four.rds')
m.p5 <- readRDS('models/score_five.rds')
m.p6 <- readRDS('models/score_six.rds')
m.p7 <- readRDS('models/score_seven.rds')
m.exceed <- readRDS('models/points_above_ex.rds')
m.leverage <- readRDS('models/m.leverage.rds')

# Read today's data
today <- read.csv(paste("../output/fl-latest.csv", sep = ""), header = FALSE)
colnames(today) <- c('Name','Team','Salary','Pos','PP.','A30','A365','iC30','iC365','G30','G365','Min30','Min365','PPShots30','PPShots365', 'Total','Games')
today <- subset(today, !today$Pos %in% c("1G","2G"))
today$PPLow <- ifelse(substring((today$Pos),1,1)>2 & today$PP. > 0, 1, 0)
today$Line <- substring((today$Pos),1,1)
today$CWD <- substring((today$Pos),2,2)

# Fix all the empty categories
today$A30 <- ifelse(is.na(today$A30), today$A365, (today$A30*0.5) + (today$A365*0.5))
today$iC30 <- ifelse(is.na(today$iC30), today$iC365, (today$iC30*0.5) + (today$iC365*0.5))
today$G30 <- ifelse(is.na(today$G30), today$G365, (today$G30*0.5) + (today$G365*0.5))
today$Min30 <- ifelse(is.na(today$Min30), today$Min365, today$Min30)
today$PPShots30 <- ifelse(is.na(today$PPShots30), today$PPShots365, (today$PPShots30*0.5) + (today$PPShots365*0.5))
today$Team <- ifelse(today$Team=="VGK","VAN",as.character(today$Team))
today$Games <- ifelse(today$Games == 28, 26, today$Games)

## Predict Ownership
today$pOwn <- predict.lm(pOwn,today)
today$pOwnc <- predict.lm(pOwnc,today)
today$pOwnc <- ifelse(today$pOwnc < 0, 0, today$pOwnc)

# Save this and rerun to add VGK into the lists
x <- today$pOwnc

# Read today's data
today <- read.csv(paste("../output/fl-latest.csv", sep = ""), header = FALSE)
colnames(today) <- c('Name','Team','Salary','Pos','PP.','A30','A365','iC30','iC365','G30','G365','Min30','Min365','PPShots30','PPShots365', 'Total','Games')
today <- subset(today, !today$Pos %in% c("1G","2G"))
today$PPLow <- ifelse(substring((today$Pos),1,1)>2 & today$PP. > 0, 1, 0)
today$Line <- substring((today$Pos),1,1)
today$CWD <- substring((today$Pos),2,2)


# Fix all the empty categories
today$A30 <- ifelse(is.na(today$A30), today$A365, (today$A30*0.5) + (today$A365*0.5))
today$iC30 <- ifelse(is.na(today$iC30), today$iC365, (today$iC30*0.5) + (today$iC365*0.5))
today$G30 <- ifelse(is.na(today$G30), today$G365, (today$G30*0.5) + (today$G365*0.5))
today$Min30 <- ifelse(is.na(today$Min30), today$Min365, today$Min30)
today$PPShots30 <- ifelse(is.na(today$PPShots30), today$PPShots365, (today$PPShots30*0.5) + (today$PPShots365*0.5))

#Now add the predictions including VGK
today$pOwnc <- x
## Predict Ownership with sum = 800%
cown <- sum(subset(today, !is.na(today$pOwnc) & today$CWD == 'C')$pOwnc)
wown <- sum(subset(today, !is.na(today$pOwnc) & today$CWD == 'W')$pOwnc)
down <- sum(subset(today, !is.na(today$pOwnc) & today$CWD == 'D')$pOwnc)
today$pOwnc <- ifelse(today$CWD == 'C',today$pOwnc*233/cown,ifelse(today$CWD == 'W',today$pOwnc*333/wown,today$pOwnc*233/down))
today$pOwn <- NULL

## Point dist projections
today$pp0 <- predict.lm(m.p0,today) #Chance of scoring 0
today$pp1 <- predict.lm(m.p1,today) #Chance of scoring 1+
today$pp2 <- predict.lm(m.p2,today)
today$pp3 <- predict.lm(m.p3,today)
today$pp4 <- predict.lm(m.p4,today)
today$pp5 <- predict.lm(m.p5,today)
today$pp6 <- predict.lm(m.p6,today)
today$pp7 <- predict.lm(m.p7,today)

# Magnitude Up
today$Mag <- predict.lm(m.exceed,today)+ today$Salary/1000*0.6

# Freq Calc
scurve <- function(a,b,c) {
  output <- -a*(exp(-b*exp(-c*d$y)))+1
  return(output)
}

# Initialize the curve function
t <- today[1,]
x <- c(t[,"pp1"],t[,"pp2"],t[,"pp3"],t[,"pp4"],t[,"pp5"],t[,"pp6"],t[,"pp7"])
y <- c(1,2,3,4,5,6,7)
d <- data.frame(y,x)
m.curve <- tryCatch(nls(x~scurve(a,b,c),data=d,start = c(a=1,b = 5, c = 0.5)), error = function(err) NA)
sal <- t[,"Salary"]/1000.0*0.6

# Save curve function
curve <- function(t) {
  x <- c(t[,"pp1"],t[,"pp2"],t[,"pp3"],t[,"pp4"],t[,"pp5"],t[,"pp6"],t[,"pp7"])
  y <- c(1,2,3,4,5,6,7)
  d <- data.frame(y,x)
  m.curve <- tryCatch(nls(x~scurve(a,b,c),data=d,start = c(a=1,b = 5, c = 0.5)), error = function(err) NA)
  sal <- t[,"Salary"]/1000.0*0.6
  ifelse(is.na(m.curve)[[1]],NA,return(-coef(m.curve)[[1]]*(exp(-coef(m.curve)[[2]]*exp(-coef(m.curve)[[3]]*sal)))+1))
}

#Apply curve to all players to determine frequency
today <- adply(today,1,curve)
colnames(today)[31] <- "Freq"
today$Freq <- ifelse(is.na(today$Freq),min(subset(today,!is.na(today$Freq))$Freq),today$Freq)

# Leverage
today$PICL<- today$Mag * today$Freq^0.7684264
today$Payoff <- ((today$pp1*1)+(today$pp1*2)+(today$pp1*3)+(today$pp1*4)+(today$pp1*5)+(today$pp1*6)+(today$pp1*7))/8
today$Leverage <- today$Payoff/predict.lm(m.leverage,today)

# Get the output df we want
output <- today[,c("Name","Team","Salary",'CWD',"Line",'PP.','pOwnc','Mag','Freq','Payoff','PICL','Leverage')]
output <- cbind(output[,1:6],round(output[,7:12],3))
colnames(output) <- c("Name","Team","Salary",'Pos',"Line",'PP','Own%','Mag','Freq','Payoff','PICL','Leverage')
head(output)

# Write to json file
output %>%
  toJSON() %>%
  write_lines(paste("../output/latest.json",sep =""))
