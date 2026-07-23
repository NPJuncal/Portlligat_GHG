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


# organic carbon stocks ---------------------------------------








