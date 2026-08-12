################################################################################
# Article: GHG Portlligat
# soil data (2/n)
# V 1.0
# Year: 2026
################################################################################


#load data

File<-"data/geochem.csv"

B<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
B<-as.data.frame(B)



# soil density ---------------------------------------

#get superficial samples for each core
#comparison between dbd

sup_sample<-B[!duplicated(B$core),]

mean(B$dbd)
sd(B$dbd)

aggregate(B$dbd, list(B$core), mean)
aggregate(B$dbd, list(B$core), sd)

aggregate(B$dbd, list(B$status), mean)
aggregate(B$dbd, list(B$status), sd)

p1<-ggplot(B,aes(status, dbd))+
  ylab(expression(paste("Dry Bulk Density (g"," ", cm^-3,")")))+
  geom_boxplot()+
  geom_point()+
  stat_compare_means(comparisons = list(c("dead", "living")),
                                 method = "wilcox.test",
                                 p.adjust.method = "holm",
                                 label = "p.signif",
                                 label.y = c(0.6),
                                 tip.length = 0.05)


# om-oc equation ---------------------------------------

#load data from the Spanish and Portuguese inventory
File<-"data/spanish_portuguese_inventory.csv"

inventory<-read.csv(File, header=T, sep=";", dec=".", fileEncoding="latin1")
inventory<-as.data.frame(inventory)

#select data from P. oceanica
inventory<-subset(inventory, inventory$Dominant.Species=="Posidonia oceanica")

inventory$status<-"living"

  
inventory <- within(inventory, status[inventory$Site.ID == "CA.S" 
                                       |inventory$Site.ID == "CA.SC"
                                       |inventory$Site.ID == "RO.SC" 
                                       |inventory$Site.ID == "TE.IC"] 
                                       <- "dead")


aggregate(inventory$Organic.carbon...., list(inventory$status), mean, na.rm=T)
aggregate(inventory$Organic.carbon...., list(inventory$status), sd, na.rm=T)


ggplot(inventory, aes(inventory$Organic.matter...., 
                      inventory$Organic.carbon....))+
  geom_point(aes(color=inventory$status), alpha=0.3)


# get equation linear regresion between om and oc to predicto oc
om_oc<-lm(inventory$Organic.carbon.... ~ inventory$Organic.matter...., inventory)

summary(om_oc)

as.formula(
  paste0("y ~ ", round(coefficients(om_oc)[1],2), " + ", 
         paste(sprintf("%.2f * %s", 
                       coefficients(om_oc)[-1],  
                       names(coefficients(om_oc)[-1])), 
               collapse=" + ")
  )
)


#Predict oc from the lm model

B$oc <- 0.25 + (0.38*B$Total.OM.)


p2<-
  ggplot(B,aes(status, oc))+
  ylab("OC %")+
  geom_boxplot()+
  geom_point()+
  ylim(2, 17)+
  stat_compare_means(comparisons = list(c("dead", "living")),
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif"
                     ,label.y = c(16),
                     tip.length = 0.05
  )



# organic carbon stocks ---------------------------------------

#stock stimation in the first 50 cm ( ~ rhizophere depth by 
#https://www.sciencedirect.com/science/article/pii/S0146638020301753)

stocks50 <- estimate_oc_stock(
                      df = B,
                      depth = 50,
                      core = "core",
                      mind = "d_min_depth",
                      maxd = "d_max_depth",
                      dbd = "dbd",
                      oc = "oc")


# units g cm-2
stocks50

#add status to stocks data.frame

stocks50<-cbind(stocks50, sup_sample$status)
colnames(stocks50)[6]<-"status"

mean(stocks50$stock)
sd(stocks50$stock)

aggregate(stocks50$stock, list(stocks50$status), mean, na.rm=T)
aggregate(stocks50$stock, list(stocks50$status), sd, na.rm=T)


p3<-ggplot(stocks50,aes(status, stock))+
  ylab(expression(paste("OC stock 50 cm depth (g"," ", cm^-2,")")))+
  geom_boxplot()+
  geom_point()+
  ylim(0.4, 1.2)+
  stat_compare_means(comparisons = list(c("dead", "living")),
                     method = "wilcox.test",
                     p.adjust.method = "holm",
                     label = "p.signif"
                     ,label.y = c(1.1),
                     tip.length = 0.05
                     )



# organic carbon figure  --------------------------------------------------

plot_geochem<-plot_grid(p1, p2, p3, 
                       #align = "v", 
                       nrow = 1
                       #,rel_widths = c( 1/4, 1/4,2/4)
)

plot_geochem

ggsave(file.path("results","soil_carbon.png"), width = 24, height = 9, units = "cm")



# average compresion Posidonia --------------------------------------------

temp<-inventory[!duplicated(inventory$Core.ID),]

mean(temp$Core.Compression)
sd(temp$Core.Compression)
