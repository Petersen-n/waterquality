
library(timeline)
library(ggplot2)
library(lubridate)
library(dplyer)

data<-read.csv("Z:/Shared/BBP Water Monitoring Network/Data/KOR data exports from sonde/Ran through MACRO/deployment dates.csv")

#ggplot(data,aes(x=data$start, y=data$station)) + geom_line()

p1 <- ggplot(data,aes(x=data$start, y=data$station)) +
  geom_segment(aes(x=data$start,xend=data$end,yend=data$station),size=15) +
  scale_colour_discrete(guide=guide_legend(override.aes=list(size=7))) #or legend will be too big
p1

#############################################################################
  

library(timevis)

data <- data.frame(
  id      = 1:13,
  content = c("","","","","","", "","Seaside", "SS","SS", "Mantoloking", "MT", "MT"),
  start   = c("2016-11-21", "2017-01-12", "2018-03-16", "2018-10-11", "2018-10-31","2018-11-29", "2019-04-01",   
              "2017-04-20","2018-03-09","2019-04-09",
              "2017-04-21","2018-03-09","2019-04-09"),
  end     = c("2016-12-14", "2018-01-03", "2018-10-09", "2018-10-29", "2018-11-27","2019-01-02", "2019-12-12",   
              "2018-01-03","2019-01-02", "2019-12-16",
              "2018-01-03", "2019-01-02", "2019-12-16"),
  group   = c(rep("Beach Haven", 7), rep("Seaside", 3), rep("Mantoloking", 3)),
  type    = c(rep("range", 13)),
  showZoom = FALSE
)

groups <- data.frame(
  id = c("Beach Haven", "Seaside" , "Mantoloking"),
  content = c("Beach Haven", "Seaside", "Mantoloking")
)
timevis(data, groups)

###################################################################################
### This one works - need to adjust size of plot and x axis labels.###############
###################################################################################
library(ggplot2)

Date_plot = data.frame(Station = c("Beach Haven","Beach Haven","Beach Haven","Beach Haven","Beach Haven","Beach Haven","Beach Haven",
                                   "Seaside","Seaside","Seaside",
                                   "Mantoloking","Mantoloking","Mantoloking"),
                       StartDate = c("2016-11-21", "2017-01-12", "2018-03-16", "2018-10-11", "2018-10-31","2018-11-29", "2019-04-01",
                                     "2017-04-20","2018-03-09","2019-04-09",
                                     "2017-04-21","2018-03-09","2019-04-09"), 
                       EndDate = c("2016-12-14", "2018-01-03", "2018-10-09", "2018-10-29", "2018-11-27","2019-01-02", "2019-12-12",   
                                   "2018-01-03","2019-01-02", "2019-12-31",
                                   "2018-01-03", "2019-01-02", "2019-12-31"), 
                       Color = c("lightblue","lightblue","lightblue","lightblue","lightblue","lightblue","lightblue",
                                 "lightgreen","lightgreen","lightgreen",
                                 "lightyellow","lightyellow","lightyellow"),
                       group   = c(rep("Beach Haven", 7), rep("Seaside", 3), rep("Mantoloking", 3)),
                       type    = c(rep("range", 13)))                      



Date_plot$StartDate<-as.Date(Date_plot$StartDate)
Date_plot$EndDate<-as.Date(Date_plot$EndDate)


g2 <- ggplot() +
  geom_segment(data=Date_plot, aes(x=StartDate, xend=EndDate, y=Station, yend=Station, color=Color), linetype=1, size=20) +
  scale_colour_brewer(palette = "Dark2")+
  xlab("")+
  ylab("")+
  
  
  ##tried to adjust x axis base off what Ceili did in the scatter plot script####
##Didn't work####
  scale_x_date(date_labels = "%b'%y", 
     limits = c(as.Date("2016-11-01"), as.Date("2020-01-01")),
   breaks = seq(as.Date("2016-11-01"), as.Date("2020-01-01"), "month"))+
  

  theme_bw() + theme(panel.grid.minor = element_blank()) + theme(aspect.ratio = .3)
g2 + theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#,panel.grid.major = element_blank() 




###2019 only'
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



Date_plot$StartDate<-as.Date(Date_plot$StartDate)
Date_plot$EndDate<-as.Date(Date_plot$EndDate)


g2 <- ggplot() +
  geom_segment(data=Date_plot, aes(x=StartDate, xend=EndDate, y=Station, yend=Station, color=Color), linetype=1, size=20) +
  scale_colour_brewer(palette = "Dark2")+
  xlab("")+
  ylab("")+
  ggtitle("BBP Continuous Monitoring Network",subtitle = "2019")+
  
  
  ##tried to adjust x axis base off what Ceili did in the scatter plot script####
##Didn't work####
scale_x_date(date_labels = "%b'%y", 
             limits = c(as.Date("2019-01-01"), as.Date("2020-01-01")),
             breaks = seq(as.Date("2019-01-01"), as.Date("2020-01-01"), "month"))+
  
  
  theme_bw() + theme(panel.grid.minor = element_blank()) + theme(aspect.ratio = .3)
g2 + 
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust=1, face = "bold"))+
  theme(axis.title.y = element_blank(), #margin and position of left axis labels
        axis.text.y = element_text(size = 12, face = "bold")) #size of temp#'s - Y axis label

          

