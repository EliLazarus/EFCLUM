library(ggplot2)
library(grid)
library(gridExtra)

#DONE, as a table on the graphic!!!!
#add data indicators used (probably just in powerpoint slides)
# move to function for plots (started at bottom)
# filter by quality score
# CLEAN UP!! (but could do more (eg. spellings w/o loop...))
# color points (by continent?)
# export for Powerpoint
#colours the same as EF
#Stablisize the colors by continent

"Set the minimum QScore to be included in the data"
QScoreMin <- 4

#Pull the data from csv files
EFWBData <- read.csv("NFA_WB_2017_CLUM.csv")
EFSDGData <- read.csv("NFA_SDG_2017_CLUM.csv")
IndicatorsDownloaded <- read.csv("IndicatorsDLed.csv")
SDGIndicatorsDownloaded <- read.csv("SDGIndicatorsDownloaded.csv")

#Pull the data from csv files
# EFWBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")
# EFSDGData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_SDG_2017_CLUM.csv")
# IndicatorsDownloaded <- read.csv("./GFN_Data_Visualization/ScatterVisuals/IndicatorsDownloaded.csv")
# SDGIndicatorsDownloaded <- read.csv("./GFN_Data_Visualization/ScatterVisuals/SDGIndicatorsDownloaded.csv")

#color.codes<-as.character(c("#3399FF", "#FF0000", "#000000"))

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

if (file.exists("Plots/")){
} else {
  dir.create("Plots/")
}


plotfunc <- function(data, title, CLUMcat, IndDL, height1, height2){
  myColors <-   c("gold","black", "forestgreen", "royalblue", "darkorange2", "grey30", "grey70", "purple3")
  names(myColors) <- levels(data$Continents)
  colScale <- scale_colour_manual(name = "Continents",values = myColors)
  p <- ggplot(data, aes(ZScore_Index, total , colour = Continents)) +
    geom_point(size = 3) +
    geom_text(label = data$GTAP_name,hjust=-.2, alpha=0.55, colour="grey") +
    colScale +
    labs(title = paste(title,"CLUM category:",data$clum7_name), y = "EF per capita") +
  theme(plot.margin = unit(c(1,2,1,1),"cm"), legend.position = c(.1,.7),
        legend.text = element_text(colour="black", size=6),
        legend.key.size = unit(.05,"cm"),
        legend.background = element_rect(fill=alpha ("white",0.4))) 
gt <- ggplot_gtable(ggplot_build(p)) 
gt$layout$clip[gt$layout$name == "panel"] <- "off" 


tb <- tableGrob(subset(IndDL[,3],IndDL$CLUM==CLUMcat),
                theme = ttheme_default(base_size=8,
                                       core = list(fg_params=list(hjust=0, x=0))))

plotname <- paste("Plots/",deparse(substitute(data)),".pdf", sep ="")
pdf(plotname,width=12,height=9,onefile = FALSE)
dev.off
grid.newpage()
grid.arrange(gt, tb, nrow=2, heights = c(height1, height2))
dev.off()
}

# change if no pause(waitlength) between charts wanted
peruse <- "no"
waitlength <- 6
pause <- function (secs){
  if (peruse=="yes"){Sys.sleep(secs)
  } else {
  }
}

#Plot and output all SDG Indicators 
plotfunc(EFSDGData11.Food,"SDG Data", "Food", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Goods,"SDG Data", "Goods",SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Housing,"SDG Data", "Housing", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Services,"SDG Data", "Serivces", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.PersonalTransportation,"SDG Data", "Transport", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Government,"SDG Data", "Government", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.GrossFixedCapitalFormation,"SDG Data", "GFCF", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)

#Plot and output all WB Indicators
plotfunc(EFWBData11.Food,"World Bank Data", "Food",IndicatorsDownloaded, 1.5, .5)
pause(waitlength)
#Added manual line to IndicatorsDownloaded for WBGoods to work with function
plotfunc(EFWBData11.Goods,"World Bank Data", "Goods", IndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFWBData11.Services,"World Bank Data", "Services",IndicatorsDownloaded, 1, .8)
pause(waitlength)
plotfunc(EFWBData11.Housing,"World Bank Data", "Housing",IndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFWBData11.PersonalTransportation,"World Bank Data", "Personal Transportation",IndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFWBData11.Government,"World Bank Data", "Government",IndicatorsDownloaded, 1, 1)
pause(waitlength)