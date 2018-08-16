library(ggplot2)
library(grid)


#Pull the data from csv files
EFWBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")
EFSDGData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_SDG_2017_CLUM.csv")

color.codes<-as.character(c("#3399FF", "#FF0000", "#000000"))

#subetted data to plot
EFWBData11 <- subset(EFWBData,EFWBData$year == 2011)
EFSDGData11 <- subset(EFSDGData,EFWBData$year == 2011)

EFWBData11.Food <- subset(EFWBData11, EFWBData11$clum7_name == "Food")
EFWBData11.Goods <- subset(EFWBData11, EFWBData11$clum7_name == "Goods")
EFWBData11.Government <- subset(EFWBData11, EFWBData11$clum7_name == "Government")
EFWBData11.Housing <- subset(EFWBData11, EFWBData11$clum7_name == "Housing")
EFWBData11.PersonalTransportation <- subset(EFWBData11, EFWBData11$clum7_name == "Personal Transportation")
EFWBData11.Services <- subset(EFWBData11, EFWBData11$clum7_name == "Services")

EFSDGData11.Food <- subset(EFSDGData11, EFSDGData11$clum7_name == "Food")
EFSDGData11.Goods <- subset(EFSDGData11, EFSDGData11$clum7_name == "Goods")
EFSDGData11.Government <- subset(EFSDGData11, EFSDGData11$clum7_name == "Government")
EFSDGData11.GrossFixedCapitalFormation <- subset(EFSDGData11, EFSDGData11$clum7_name == "Gross Fixed Capital Formation")
EFSDGData11.Housing <- subset(EFSDGData11, EFSDGData11$clum7_name == "Housing")
EFSDGData11.PersonalTransportation <- subset(EFSDGData11, EFSDGData11$clum7_name == "Personal Transportation")
EFSDGData11.Services <- subset(EFSDGData11, EFSDGData11$clum7_name == "Services")

#Plot for all the WB CLUM categories
p <- ggplot(EFWBData11.Food, aes(ZScore_Index, total)) + geom_point() +
    geom_text(label = EFWBData11.Food$GTAP_name,hjust=-.1) +
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Food$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


p <- ggplot(EFWBData11.Goods, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFWBData11.Goods$GTAP_name,hjust=-.1)+
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Goods$clum7_name), y = "EF per capita") +
theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFWBData11.Government, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFWBData11.Government$GTAP_name,hjust=-.1)+
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Government$clum7_name), y = "EF per capita") +
theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFWBData11.Housing, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFWBData11.Housing$GTAP_name,hjust=-.1) +
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Housing$clum7_name), y = "EF per capita") +
theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFWBData11.PersonalTransportation, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFWBData11.PersonalTransportation$GTAP_name,hjust=-.1) +
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.PersonalTransportation$clum7_name), y = "EF per capita") +
theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFWBData11.Services, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFWBData11.Services$GTAP_name,hjust=-.1) +
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Services$clum7_name), y = "EF per capita") +
theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


p <- ggplot(EFSDGData11.Food, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.Food$GTAP_name,hjust=-.1) +
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.Food$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


p <- ggplot(EFSDGData11.Goods, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.Goods$GTAP_name,hjust=-.1)+
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.Goods$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFSDGData11.Government, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.Government$GTAP_name,hjust=-.1)+
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.Government$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFSDGData11.Housing, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.Housing$GTAP_name,hjust=-.1) +
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.Housing$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFSDGData11.GrossFixedCapitalFormation, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.GrossFixedCapitalFormationd$GTAP_name,hjust=-.1) +
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.GrossFixedCapitalFormation$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFSDGData11.PersonalTransportation, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.PersonalTransportation$GTAP_name,hjust=-.1) +
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.PersonalTransportation$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

p <- ggplot(EFSDGData11.Services, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = EFSDGData11.Services$GTAP_name,hjust=-.1) +
  labs(title = paste("SDG Data, CLUM category:",EFSDGData11.Services$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm")) 
gt <- ggplot_gtable(ggplot_build(p)) 
gt$layout$clip[gt$layout$name == "panel"] <- "off" 
grid.draw(gt)

plotfunc <- function(data, title){p <- ggplot(data, aes(ZScore_Index, total)) + geom_point() +
  geom_text(label = data$GTAP_name,hjust=-.1) +
  labs(title = paste(title,"CLUM category:",data$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(0,4,0,0),"cm")) 
gt <- ggplot_gtable(ggplot_build(p)) 
gt$layout$clip[gt$layout$name == "panel"] <- "off" 
grid.draw(gt)
}

plotfunc(EFSDGData11.Services,"SDG Data")
plotfunc(EFSDGData11.Goods,"SDG Data")






ggplot(EFWBData, aes(ZScore_Index, total)) + geom_point() + facet_grid(~ clum7_name, scales = "free")+
  geom_text( label = EFWBData$GTAP_name)
ggplot(EFWBData, aes(MaxMin_Index, total, colour = as.factor(year)
                     ))+ geom_point() + facet_grid(~ clum7_name) +
  scale_colour_manual(values = color.codes) #+
  #geom_text(label = EFWBData$GTAP_name)
ggplot(EFSDGData, aes(ZScore_Index, total)) + geom_point() + facet_grid(~ clum7_name)
ggplot(EFSDGData, aes(MaxMin_Index, total)) + geom_point() + facet_grid(~ clum7_name)

