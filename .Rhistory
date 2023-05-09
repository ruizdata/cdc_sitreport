=
knitr::opts_chunk$set(echo = TRUE)=
plot(pressure)
#plot(pressure)
jpeg('Cumulative_Vaccine.jpeg', width=1024, height=600)
helo
print('hello')
print("hello")
demo()
|
demo()
print(active.case)
last.day <- 10
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
library(openxlsx)
library(dplyr)
library(gridExtra)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(gggenes)
library(zoo)
library(tidyr)
library(lubridate)
a0 <- read.xlsx("C:/Users/EpiTeam/Desktop/R-code SIT Report/Line list.xlsx",sheet=1)
a0.NN <- a0[a0$residence != "Bordertown", ]
a0.NN.Deaths <- a0.NN[a0.NN$deceased == "1", ]
a0.NN.Deaths[is.na(a0.NN.Deaths)]= 0
a0.NN[is.na(a0.NN$testdate),"testdate"] <- a0.NN[is.na(a0.NN$testdate),"date"]
a0.SU <- a0.NN %>% count(residence)
NN.total <- sum(a0.SU$n)
print(NN.total)
a0.NN <- a0.NN %>% count(residence, testdate)
NN.woBT <- a0.NN %>% group_by(testdate) %>% summarise(case = sum(n))
last.day <- 1
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
New.case <- NC.function(NN.woBT$case)
print(New.case)
a0.Deaths <- a0.NN.Deaths %>% group_by(residence) %>% summarise(total = sum(deceased))
NN.Death.Total <- sum(a0.Deaths$total)
print(NN.Death.Total)
last.day <- 10
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
print(active.case)
print(NN.Death.Total)
last.day <- 10
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
Recover.Total <- NN.total - NN.Death.Total - active.case
print(Recover.Total)
last.day <- 11
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
Recover.Total <- NN.total - NN.Death.Total - active.case
print(Recover.Total)
a0.Deaths <- a0.NN.Deaths %>% group_by(residence) %>% summarise(total = sum(deceased))
NN.Death.Total <- sum(a0.Deaths$total)
print(NN.Death.Total)
last.day <- 9
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
Recover.Total <- NN.total - NN.Death.Total - active.case
print(Recover.Total)
active.days <- 9
NC.function <- function (a){
m <- tail(a, n=active.days)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
Recover.Total <- NN.total - NN.Death.Total - active.case
print(Recover.Total)
run
library(dplyr)
library(gridExtra)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(gggenes)
library(zoo)
library(tidyr)
library(lubridate)
a0 <- read.xlsx("C:/Users/EpiTeam/Desktop/R-code SIT Report/Line list.xlsx",sheet=1)
a0.NN <- a0[a0$residence != "Bordertown", ]
a0.NN.Deaths <- a0.NN[a0.NN$deceased == "1", ]
a0.NN.Deaths[is.na(a0.NN.Deaths)]= 0
a0.NN[is.na(a0.NN$testdate),"testdate"] <- a0.NN[is.na(a0.NN$testdate),"date"]
a0.SU <- a0.NN %>% count(residence)
NN.total <- sum(a0.SU$n)
print(NN.total)
a0.NN <- a0.NN %>% count(residence, testdate)
NN.woBT <- a0.NN %>% group_by(testdate) %>% summarise(case = sum(n))
last.day <- 1
NC.function <- function (a){
m <- tail(a, n=last.day)
b <- sum(m)
return(b)
}
New.case <- NC.function(NN.woBT$case)
print(New.case)
a0.Deaths <- a0.NN.Deaths %>% group_by(residence) %>% summarise(total = sum(deceased))
NN.Death.Total <- sum(a0.Deaths$total)
print(NN.Death.Total)
active.days <- 9
NC.function <- function (a){
m <- tail(a, n=active.days)
b <- sum(m)
return(b)
}
active.case <- NC.function(NN.woBT$case)
Recover.Total <- NN.total - NN.Death.Total - active.case
print(Recover.Total)
##############################
####### service area #########
##############################
bor <- sum(a0$residence == "Bordertown")
Chin <- sum(a0$residence == "Chinle")
Crown <- sum(a0$residence == "Crownpoint")
Kay <- sum(a0$residence == "Kayenta")
Gall <- sum(a0$residence == "Gallup")
FD <- sum(a0$residence == "Fort Defiance")
Ship <- sum(a0$residence =="Shiprock")
Tuba <- sum(a0$residence == "Tuba City")
Win <- sum(a0$residence == "Winslow")
Unk <- sum(a0$residence == "Unknown")
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.total<- c(Chin, Crown, FD, Gall, Kay, Ship, Tuba, Win, Unk)
Total <- data.frame(SU, SU.total)
NN <- c("Navajo Nation")
NN.tot <- c(NN.total)
NN.Death <- c(NN.Death.Total)
NN.New.Case <- c(New.case)
NN.Overall <- data.frame(NN, NN.tot, NN.New.Case, NN.Death)
print(Total)
print(NN.Overall)
##############################
####### service area #########
##############################
bor <- sum(a0$residence == "Bordertown")
Chin <- sum(a0$residence == "Chinle")
Crown <- sum(a0$residence == "Crownpoint")
Kay <- sum(a0$residence == "Kayenta")
Gall <- sum(a0$residence == "Gallup")
FD <- sum(a0$residence == "Fort Defiance")
Ship <- sum(a0$residence =="Shiprock")
Tuba <- sum(a0$residence == "Tuba City")
Win <- sum(a0$residence == "Winslow")
Unk <- sum(a0$residence == "Unknown")
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.total<- c(Chin, Crown, FD, Gall, Kay, Ship, Tuba, Win, Unk)
Total <- data.frame(SU, SU.total)
NN <- c("Navajo Nation")
NN.tot <- c(NN.total)
NN.Death <- c(NN.Death.Total)
NN.New.Case <- c(New.case)
Total.Recoverd <- c(Recover.Total)
NN.Overall <- data.frame(NN, NN.tot, NN.New.Case, NN.Death, Total.Recoverd )
print(Total)
print(NN.Overall)
##############################
####### service area #########
##############################
bor <- sum(a0$residence == "Bordertown")
Chin <- sum(a0$residence == "Chinle")
Crown <- sum(a0$residence == "Crownpoint")
Kay <- sum(a0$residence == "Kayenta")
Gall <- sum(a0$residence == "Gallup")
FD <- sum(a0$residence == "Fort Defiance")
Ship <- sum(a0$residence =="Shiprock")
Tuba <- sum(a0$residence == "Tuba City")
Win <- sum(a0$residence == "Winslow")
Unk <- sum(a0$residence == "Unknown")
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.total<- c(Chin, Crown, FD, Gall, Kay, Ship, Tuba, Win, Unk)
Total <- data.frame(SU, SU.total)
NN <- c("Navajo Nation")
NN.tot <- c(NN.total)
NN.Death <- c(NN.Death.Total)
NN.New.Case <- c(New.case)
Total.Recoverd <- c(Recover.Total)
NN.Overall <- data.frame(NN, NN.tot, NN.New.Case, Total.Recoverd, NN.Death )
print(Total)
print(NN.Overall)
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.1 < C("Service Unit")
Chinle <- c(Chin)
Crownpoint <- c(Crown)
Fort_Defiance <- C(FD)
Unk <- sum(a0$residence == "Unknown")
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.1 <- C("Service Unit")
Chinle <- c(Chin)
Crownpoint <- c(Crown)
Fort_Defiance <- C(FD)
Gallup <- c(Gall)
Kayenta <- c(Kay)
Shiprock <- c(Ship)
Tuba_City <- c(Tuba)
Winslow <- c(Win)
Unknown <- c(Unk)
serive.unit <- date.frame(SU.1,Chinle, Crownpoint,Fort_Defiance,Gallup, Kayenta,Shiprock, Tuba_City, Winslow, Unknown )
print(serive.unit)
SU.total<- c(Chin, Crown, FD, Gall, Kay, Ship, Tuba, Win, Unk)
SU <- c("Chinle", "Crownpoint","Fort Defiance","Gallup", "Kayenta","Shiprock", "Tuba City", "Winslow", "Unknown")
SU.1 <- c("Service Unit")
Chinle <- c(Chin)
Crownpoint <- c(Crown)
Fort_Defiance <- C(FD)
Gallup <- c(Gall)
Kayenta <- c(Kay)
Chinle <- c(Chin)
Crownpoint <- c(Crown)
Fort_Defiance <- c(FD)
Gallup <- c(Gall)
Kayenta <- c(Kay)
Shiprock <- c(Ship)
Tuba_City <- c(Tuba)
Winslow <- c(Win)
Unknown <- c(Unk)
service.unit <- date.frame(SU.1,Chinle, Crownpoint,Fort_Defiance,Gallup, Kayenta,Shiprock, Tuba_City, Winslow, Unknown )
print(service.unit)
Shiprock <- c(Ship)
Tuba_City <- c(Tuba)
Winslow <- c(Win)
Unknown <- c(Unk)
service.unit <- data.frame(SU.1,Chinle, Crownpoint,Fort_Defiance,Gallup, Kayenta,Shiprock, Tuba_City, Winslow, Unknown )
print(service.unit)
source('C:/Users/EpiTeam/Desktop/R-code SIT Report/R-code/sitreport.R')
