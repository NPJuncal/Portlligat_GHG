################################################################################
# Article: GHG Portlligat
# Greehouse gas fluxes estimation from chamber data (1/n)
# V 1.0
# Year: 2026
################################################################################



#load libraries 

library(tidyverse)
library(ggpubr)
library(cowplot)
library(BlueCarbon)

#create folder to save tables and graphs

dir.create("results")

# load and transform data -------------------------------------------------

File<-"data/chambers.csv"

A<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
A<-as.data.frame(A)


#concentration data mg m3,volumen in l, time in h
# tranform concentration data to mg l (/1000)

A[,c(3:8)]<-A[,c(3:8)]/1000


# flux estimation ---------------------------------------------------------

#(mg final time - mg initial time) / time / area
# chamber anchor area 0.0314 m2

A$CH4_flux<-((A$CH4.Tf*A$vf)-(A$CH4.Ti*A$vi))/(A$time*0.0314)
A$CO2_flux<-((A$CO2.Tf*A$vf)-(A$CO2.Ti*A$vi))/(A$time*0.0314)
A$N2O_flux<-((A$N2O.Tf*A$vf)-(A$N2O.Ti*A$vi))/(A$time*0.0314)

#transform from mg m2 h to mmol m2 h

A$CO2_flux<-A$CO2_flux*0.0227
A$N2O_flux<-A$N2O_flux*0.0227

A$CH4_flux<-A$CH4_flux*0.062


# built final table -------------------------------------------------------

#load data

File<-"data/portlligat_2021.csv"

temp<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
temp<-as.data.frame(temp)

A<-cbind(A, temp)
