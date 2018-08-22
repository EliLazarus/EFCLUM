library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)

#TO DO, next
# export for Powerpoint
#colours the same as EF
#Fix the alignment etc issues
# CLEAN UP!!

#DONE, as a table on the graphic!!!!
#add data indicators used (probably just in powerpoint slides)
# move to function for plots (started at bottom)
# filter by quality score
# CLEAN UP!! (but could do more (eg. spellings w/o loop...))
# color points (by continent?)

"Set the minimum QScore to be included in the data"
QScoreMin <- 4

#Pull the data from csv files
EFWBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")
EFSDGData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_SDG_2017_CLUM.csv")

color.codes<-as.character(c("#3399FF", "#FF0000", "#000000"))

#subetted data to plot
EFWBData11 <- subset(EFWBData,(EFWBData$year == 2011 & EFWBData$QScore > QScoreMin))
EFSDGData11 <- subset(EFSDGData,(EFSDGData$year == 2011 & EFSDGData$QScore > QScoreMin))

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

##TEST PLOT for Plot Function
myColors <- brewer.pal(8,"Set1")
names(myColors) <- levels(EFWBData11.Services$Continents)
colScale <- scale_colour_manual(name = "Continents",values = myColors)
#
p <- ggplot(EFWBData11.Services, aes(ZScore_Index, total,colour=Continents)) + geom_point(size = 5) +
  geom_text(label = EFWBData11.Services$GTAP_name,hjust=-.1, colour="grey") +
  colScale +
  theme(plot.margin = unit(c(1,4,1,1),"cm")) +
  # annotation_custom(textGrob(subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))) +
  # xmin=-.5,xmax=1,ymin=-2,ymax=-1) +
  #
  labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Services$clum7_name),
       y = "EF per capita"); p
# annotate("text", x = 0, y = -1:-17, hjust = 0,
#          label = subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))
# labs(caption = "text",subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
tb <- tableGrob(subset(SDGIndicatorsDownloaded[,2],SDGIndicatorsDownloaded$CLUM=="Services"),
                theme = ttheme_default(base_size=9, margin = theme(plot.margin = unit(c(2,2,2,2), "cm")),core = list(fg_params=list(hjust=0, x=0))))
#rowhead = list(fg_params=list(hjust=0, x=0))))
grid.arrange(gt, tb, nrow=2, heights = c(2,1))

# plotname <- paste(substr(EFWBData11.Services,1,5),substr(EFWBData11.Services,13,17),".pdf")
# pdf(plotname,width=6,height=4,paper='special')

#grid.draw(gt)
#subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services")

plotfunc <- function(data, title, CLUMcat, IndDL){
  myColors <- brewer.pal(8,"Set1")
  names(myColors) <- levels(data$Continents)
  colScale <- scale_colour_manual(name = "Continents",values = myColors)
  p <- ggplot(data, aes(ZScore_Index, total , colour = Continents)) +
    geom_point(size = 3) +
    geom_text(label = data$GTAP_name,hjust=-.2, alpha=0.6, colour="grey") +
    colScale +
    labs(title = paste(title,"CLUM category:",data$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(1,2,1,1),"cm"), legend.position = c(.1,.7),
        legend.text = element_text(colour="black", size=6),
        legend.key.size = unit(.05,"cm"),
        legend.background = element_rect(fill=alpha ("white",0.4))) 
gt <- ggplot_gtable(ggplot_build(p)) 
gt$layout$clip[gt$layout$name == "panel"] <- "off" 
tb <- tableGrob(subset(IndDL[,2],IndDL$CLUM==CLUMcat),
                theme = ttheme_default(base_size=8, 
                                       core = list(fg_params=list(hjust=0, x=0))))
#rowhead = list(fg_params=list(hjust=0, x=0))))
#tb <- grid.table(subset(IndDL[,2],IndDL$CLUM==CLUMcat))
grid.newpage()
grid.arrange(gt, tb, nrow=2, heights = c(1.5,.5))

# plotname <- paste(data,".pdf")
# pdf(plotname,width=6,height=4,paper='special')
# dev.off

#, main=textGrob("Total Data and Image", gp=gpar(cex=3)), ncol = 2,
           #  widths=unit.c(grobWidth(gt), unit(1,"npc") - grobWidth(tb)))
#grid.draw(gt)
}

# change if no pause(waitlength) between charts wanted
peruse <- "yes"
waitlength <- 6
if (peruse=="yes"){pause <- function(secs){Sys.sleep(secs)}
}   else {pause(waitlength) <- 0}

plotfunc(EFSDGData11.Services,"SDG Data", "Services", SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.Goods,"SDG Data", "Goods",SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.Government,"SDG Data", "Government", SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.Housing,"SDG Data", "Housing", SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.GrossFixedCapitalFormation,"SDG Data", "Gross Fixed Capital Formation", SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.PersonalTransportation,"SDG Data", "Personal Transportation", SDGIndicatorsDownloaded)
pause(waitlength)
plotfunc(EFSDGData11.Food,"SDG Data", "Food", SDGIndicatorsDownloaded)
pause(waitlength)


plotfunc(EFWBData11.PersonalTransportation,"World Bank Data", "Personal Transportation",IndicatorsDownloaded)
pause(waitlength)
plotfunc(EFWBData11.Housing,"World Bank Data", "Housing",IndicatorsDownloaded)
pause(waitlength)
plotfunc(EFWBData11.Government,"World Bank Data", "Government",IndicatorsDownloaded)
pause(waitlength)
plotfunc(EFWBData11.Food,"World Bank Data", "Food",IndicatorsDownloaded)
pause(waitlength)
plotfunc(EFWBData11.Services,"World Bank Data", "Services",IndicatorsDownloaded)

#Goods DOES NOT HAVE WB Data!!!!
plotfunc(EFWBData11.Goods,"World Bank Data", "Goods", IndicatorsDownloaded)
