################################################################################
# Article: GHG Portlligat
# xxx xxx (1/n)
# V 1.0
# Year: 2026
################################################################################


#load libraries 

library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)

#create folder to save tables and graphs

dir.create("results")

#load data

File<-"data/portlligat_2021.csv"

A<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
A<-as.data.frame(A)



# summary table -----------------------------------------------------------

summary_table<-as.data.frame(aggregate(CO2 ~ Station, data = A, mean)[,1])
summary_table$CO2<-as.data.frame(aggregate(CO2 ~ Station, data = A, mean)[,2])
summary_table$CO2_sd<-as.data.frame(aggregate(CO2 ~ Station, data = A, sd)[,2])


summary_table$CH4<-as.data.frame(aggregate(CH4 ~ Station, data = A, mean)[,2])
summary_table$CH4_sd<-as.data.frame(aggregate(CH4 ~ Station, data = A, sd)[,2])


summary_table$N2O<-as.data.frame(aggregate(N2O ~ Station, data = A, mean)[,2])
summary_table$N2O_sd<-as.data.frame(aggregate(N2O ~ Station, data = A, sd)[,2])


colnames(summary_table)<-c("station", "CO2", "C2O_sd", "CH4", "CH4_sd","N2O", "N2O_sd")

write.csv(summary_table, file.path("results","summary_flux.csv"), row.names = FALSE)


# differences day night ----------------------------------------------------


my_comparisons <- list(
  c("Dead_D", "Living_D"),
  c("Dead_N", "Living_N")
)

my_comparisons2 <- list(
  c("Dead_D", "Dead_N"),
  c("Living_D", "Living_N")
)


p1<-
  ggplot(A,aes(Station, CO2))+
  ylab(expression(paste("C",O[2], " (micromol"," ", m^-2, h^-1,")")))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  geom_hline(yintercept=0)+
  stat_compare_means(comparisons = my_comparisons,
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif")+
    stat_compare_means(comparisons = my_comparisons2,
                       method = "wilcox.test",
                       p.adjust.method = "holm",
                       label = "p.signif",
                       label.y = c(-2.75, -3),
                       tip.length = -0.025)
  

p2<-
  ggplot(A,aes(Station, CH4))+
  ylab(expression(paste("C",H[4], " (micromol"," ", m^-2, h^-1,")")))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  geom_hline(yintercept=0)+
  ylim(-0.04,0.1)+
  stat_compare_means(comparisons = my_comparisons,
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif",
                     label.y = c(0.06, 0.075),
                     tip.length = 0.005)+
    stat_compare_means(comparisons = my_comparisons2,
                       method = "wilcox.test",
                       p.adjust.method = "holm",
                       label = "p.signif",
                       label.y = c(-0.04, -0.05),
                       tip.length = -0.005)


p3<-
  ggplot(A,aes(Station, N2O))+
  ylab(expression(paste(N[2],"O (micromol"," ", m^-2, h^-1,")")))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  geom_hline(yintercept=0) +
  stat_compare_means(comparisons = my_comparisons,
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif")+
    stat_compare_means(comparisons = my_comparisons2,
                       method = "wilcox.test",
                       p.adjust.method = "holm",
                       label = "p.signif",
                       label.y = c(-0.004, -0.005),
                       tip.length = -0.03)
  



plot_fluxes<-plot_grid(p1, p2, p3, 
                     #align = "v", 
                     nrow = 1
                     #,rel_widths = c( 1/4, 1/4,2/4)
)

plot_fluxes


ggsave(file.path("results","fluxes.png"), width = 24, height = 9, units = "cm")



#sediment data


ggplot(A[c(1:12),],aes(Station, OM))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)



plot(A$CH4, A$OM)
plot(A$CH4, A$Rhizoms)
