###############################################################################################################
#####   Plot to show all years EXO deployment at all 3 stations   #####
#######################################################################

library(ggplot2)

Date_plot = data.frame(Station = c("Beach Haven","Beach Haven","Beach Haven","Beach Haven",
                                   "Seaside","Seaside","Seaside",
                                   "Mantoloking","Mantoloking","Mantoloking"),
                       StartDate = c("2016-11-21", "2017-01-12", "2018-03-16","2019-04-01",
                                     "2017-04-20","2018-03-09","2019-04-09",
                                     "2017-04-21","2018-03-09","2019-04-09"), 
                       EndDate = c("2016-12-14", "2018-01-03","2019-01-02", "2019-12-12",   
                                   "2018-01-03","2019-01-02", "2019-12-16",
                                   "2018-01-03", "2019-01-02", "2019-12-16"), 
                       Color = c("lightblue","lightblue","lightblue","lightblue",
                                 "lightgreen","lightgreen","lightgreen",
                                 "lightyellow","lightyellow","lightyellow"),
                       group   = c(rep("Beach Haven", 4), rep("Seaside", 3), rep("Mantoloking", 3)),
                       type    = c(rep("range", 10)))                      


#Change date from factor to Date#
Date_plot$StartDate<-as.Date(Date_plot$StartDate)
Date_plot$EndDate<-as.Date(Date_plot$EndDate)


g2 <- ggplot() +
  geom_segment(data=Date_plot, aes(x=StartDate, xend=EndDate, y=Station, yend=Station, color=Color), linetype=1, size=20) +
  scale_colour_brewer(palette = "Dark2")+
  xlab("")+
  ylab("")+
  ggtitle("BBP Continuous Water Quality Monitoring", subtitle = "EXO-2 Deployments")+
  
#x-axis label setup 
  scale_x_date(date_labels = "%b'%y", 
     limits = c(as.Date("2016-11-01"), as.Date("2020-01-01")),
   breaks = seq(as.Date("2016-11-01"), as.Date("2020-01-01"), "month"))+
  
    theme_bw() + theme(panel.grid.minor = element_blank()) + theme(aspect.ratio = .3)
g2 + theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) ##angle x-axis labels


###################################################################################################################
#        For Karen/annual report - only 2019 all 3 stations
##################################################################

library(ggplot2)

Date_plot = data.frame(Station = c("Beach Haven","Beach Haven",
                                   "Seaside","Seaside",
                                   "Mantoloking","Mantoloking"),
                       StartDate = c("2019-01-01","2019-04-01",
                                     "2019-01-01","2019-04-09",
                                     "2019-01-01","2019-04-09"), 
                       EndDate = c("2019-01-02", "2019-12-12",   
                                   "2019-01-02", "2019-12-31",
                                   "2019-01-02", "2019-12-31"), 
                       Color = c("lightblue","lightblue",
                                 "lightgreen","lightgreen",
                                 "lightyellow","lightyellow"),
                       group   = c(rep("Beach Haven", 2), rep("Seaside", 2), rep("Mantoloking", 2)),
                       type    = c(rep("range", 6)))                      


#Change date from factor to Date#
Date_plot$StartDate<-as.Date(Date_plot$StartDate)
Date_plot$EndDate<-as.Date(Date_plot$EndDate)

g2 <- ggplot() +
  geom_segment(data=Date_plot, aes(x=StartDate, xend=EndDate, y=Station, yend=Station, color=Date_plot$Color), linetype=1, size=10) +
  scale_colour_brewer(palette = "Dark2")+
  xlab("")+
  ylab("")+
  ggtitle("BBP Continuous Water Quality Monitoring", subtitle = "2019 EXO-2 Deployments")+
  
#x-axis label setup 
scale_x_date(date_labels = "%b", 
             limits = c(as.Date("2019-01-01"), as.Date("2020-01-01")),
             breaks = seq(as.Date("2019-01-01"), as.Date("2020-01-01"), "month"))+
  
  theme_bw() + theme(panel.grid.minor = element_blank()) + theme(aspect.ratio = .3)
g2 + theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) ##angle x-axis labels
    

#########################################################################################################################
#         Coastal Acidification sensor deployments
####################################################################

library(ggplot2)

Date_plot = data.frame(Sensor = c("PCO2-Pro CV","SeaFET pH"),
                       StartDate = c("2019-07-10","2019-07-10"), 
                       EndDate = c("2019-12-12", "2019-10-14"), 
                       Color = c("lightblue","lightgreen"))
                       
#Change date from factor to Date#
Date_plot$StartDate<-as.Date(Date_plot$StartDate)
Date_plot$EndDate<-as.Date(Date_plot$EndDate)

g2 <- ggplot() +
  geom_segment(data=Date_plot, aes(x=StartDate, xend=EndDate, y=Sensor, yend=Sensor, color=Date_plot$Color), linetype=1, size=10) +
  scale_colour_brewer(palette = "Dark2")+
  xlab("")+
  ylab("")+
  ggtitle("BBP Continuous Water Quality Monitoring", subtitle = "2019 Coastal Acidification Deployments")+
  
  #x-axis label setup 
  scale_x_date(date_labels = "%b", 
               limits = c(as.Date("2019-01-01"), as.Date("2020-01-01")),
               breaks = seq(as.Date("2019-01-01"), as.Date("2020-01-01"), "month"))+
  
  theme_bw() + theme(panel.grid.minor = element_blank()) + theme(aspect.ratio = .3)
g2 + theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) ##angle x-axis labels


##############################################################################################################
#


