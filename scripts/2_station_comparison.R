################################################################################
# Article: GHG Portlligat
# Station comparison (2/n)
# V 1.0
# Year: 2026
################################################################################


# summary table -----------------------------------------------------------

summary_table <- aggregate(CO2_flux ~ Station, data = A, mean)
summary_table$CO2_sd<-aggregate(CO2_flux ~ Station, data = A, sd)[,2]

summary_table$CH4<-aggregate(CH4_flux ~ Station, data = A, mean)[,2]
summary_table$CH4_sd<-aggregate(CH4_flux ~ Station, data = A, sd)[,2]


summary_table$N2O<-aggregate(N2O_flux ~ Station, data = A, mean)[,2]
summary_table$N2O_sd<-aggregate(N2O_flux ~ Station, data = A, sd)[,2]

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
  ggplot(A,aes(Station, CO2_flux))+
  ylab(expression(paste("C",O[2], " (mmol"," ", m^-2, h^-1,")")))+
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
  ggplot(A,aes(Station, CH4_flux))+
  ylab(expression(paste("C",H[4], " (mmol"," ", m^-2, h^-1,")")))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  geom_hline(yintercept=0)+
  ylim(-0.03,0.065)+
  stat_compare_means(comparisons = my_comparisons,
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif",
                     label.y = c(0.035, 0.05),
                     tip.length = 0.005)+
    stat_compare_means(comparisons = my_comparisons2,
                       method = "wilcox.test",
                       p.adjust.method = "holm",
                       label = "p.signif",
                       label.y = c(-0.025, -0.03),
                       tip.length = -0.005)


p3<-
  ggplot(A,aes(Station, N2O_flux))+
  ylab(expression(paste(N[2],"O (mmol"," ", m^-2, h^-1,")")))+
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


# daily emissions and CO2 eq ----------------------------------------------

# in mg m2 d

summary_table[1,2]*12+summary_table[2,2]*12

full_day<-data.frame(station=c("living", "dead"))

full_day[1,"CO2"]<-(summary_table[3,2]*12+summary_table[4,2]*12)/0.0227
full_day[2,"CO2"]<-(summary_table[1,2]*12+summary_table[2,2]*12)/0.0227

full_day[1,"CH4"]<-(summary_table[3,4]*12+summary_table[4,4]*12)/0.062
full_day[2,"CH4"]<-(summary_table[1,4]*12+summary_table[2,4]*12)/0.062

full_day[1,"N2O"]<-(summary_table[3,6]*12+summary_table[4,6]*12)/0.0227
full_day[2,"N2O"]<-(summary_table[1,6]*12+summary_table[2,6]*12)/0.0227

# it must be as mass and not mols before multiplying by the global warming potentials

full_day[1,"CO2eq"]<-full_day[1,"CO2"]+(full_day[1,"CH4"]*27)
                      +(full_day[1,"N2O"]*273)

full_day[2,"CO2eq"]<-full_day[2,"CO2"]+(full_day[2,"CH4"]*27)
                      +(full_day[2,"N2O"]*273)

write.csv(full_day, file.path("results","full_day.csv"), row.names = FALSE)



  
# comparison with published data ------------------------------------------

File<-"data/literature.csv"

literature<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
literature<-as.data.frame(literature)








