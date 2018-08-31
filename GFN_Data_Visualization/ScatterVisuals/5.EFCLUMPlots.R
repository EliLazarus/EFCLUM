library(ggplot2)
library(grid)
library(gridExtra)

#TO DO, next
#Stablisize the colors by continent
#Fix the alignment etc issues
# CLEAN UP!!

#DONE, as a table on the graphic!!!!
#add data indicators used (probably just in powerpoint slides)
# move to function for plots (started at bottom)
# filter by quality score
# CLEAN UP!! (but could do more (eg. spellings w/o loop...))
# color points (by continent?)
# export for Powerpoint
#colours the same as EF

"Set the minimum QScore to be included in the data"
QScoreMin <- 4

#Pull the data from csv files
EFWBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")
EFSDGData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_SDG_2017_CLUM.csv")
IndicatorsDownloaded <- read.csv("./GFN_Data_Visualization/ScatterVisuals/IndicatorsDownloaded.csv")
SDGIndicatorsDownloaded <- read.csv("./GFN_Data_Visualization/ScatterVisuals/SDGIndicatorsDownloaded.csv")

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

if (file.exists("./GFN_Data_Visualization/ScatterVisuals/Plots/")){
} else {
  dir.create("./GFN_Data_Visualization/ScatterVisuals/Plots/")
}

##TEST PLOT for Plot Function
# myColors <- #brewer.pal(8,"Set1")
#   c("yellow","darkorange2","black", "purple4", "forestgreen", "grey30", "blue", "grey70")
# names(myColors) <- levels(EFWBData11.Services$Continents)
# colScale <- scale_colour_manual(name = "Continents",values = myColors)
# #
# p <- ggplot(EFWBData11.Services, aes(ZScore_Index, total,colour=Continents)) + geom_point(size = 5) +
#   geom_text(label = EFWBData11.Services$GTAP_name,hjust=-.1, colour="grey") +
#   colScale +
#   theme(plot.margin = unit(c(1,4,1,1),"cm")) +
#   # annotation_custom(textGrob(subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))) +
#   # xmin=-.5,xmax=1,ymin=-2,ymax=-1) +
#   #
#   labs(title = paste("World Bank Data, CLUM category:",EFWBData11.Services$clum7_name),
#        y = "EF per capita"); p
# # annotate("text", x = 0, y = -1:-17, hjust = 0,
# #          label = subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))
# # labs(caption = "text",subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services"))
# gt <- ggplot_gtable(ggplot_build(p))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"
# tb <- tableGrob(subset(SDGIndicatorsDownloaded[,2],SDGIndicatorsDownloaded$CLUM=="Services"),
#                 theme = ttheme_default(base_size=9, margin = theme(plot.margin = unit(c(2,2,2,2), "cm")),core = list(fg_params=list(hjust=0, x=0))))
# #rowhead = list(fg_params=list(hjust=0, x=0))))
# grid.arrange(gt, tb, nrow=2, heights = c(2,1))
# 
# #plotname <- paste(substr(EFWBData11.Services,1,5),substr(EFWBData11.Services,13,17),".pdf")
# #dev.off()
# #grid.draw(gt)
# #subset(IndicatorsDownloaded[,2],IndicatorsDownloaded$CLUM=="Services")

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
tb <- tableGrob(subset(IndDL[,2],IndDL$CLUM==CLUMcat),
                theme = ttheme_default(base_size=8, 
                                       core = list(fg_params=list(hjust=0, x=0))))
plotname <- paste("./GFN_Data_Visualization/ScatterVisuals/Plots/",deparse(substitute(data)),".pdf", sep ="")
pdf(plotname,width=12,height=9,onefile = FALSE)
dev.off
grid.newpage()
grid.arrange(gt, tb, nrow=2, heights = c(height1, height2))
dev.off()
#, main=textGrob("Total Data and Image", gp=gpar(cex=3)), ncol = 2,
           #  widths=unit.c(grobWidth(gt), unit(1,"npc") - grobWidth(tb)))
#grid.draw(gt)
}

# change if no pause(waitlength) between charts wanted
peruse <- "no"
waitlength <- 6
pause <- function (secs){
  if (peruse=="yes"){Sys.sleep(secs)
  } else {
  }
}

# plotfunc(EFSDGData11.Services,"SDG Data", "Services", SDGIndicatorsDownloaded)
# plotname <- paste(as.character(substr(EFWBData11.Services,1,5),substr(EFWBData11.Services,13,17)),".pdf")
# pdf("Test.pdf", onefile = FALSE,width=12,height=9)#, paper = "special")
# dev.off()

#Plot and output all SDG Indicators 
plotfunc(EFSDGData11.Food,"SDG Data", "Food", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Goods,"SDG Data", "Goods",SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Housing,"SDG Data", "Housing", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Services,"SDG Data", "Services", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.PersonalTransportation,"SDG Data", "Personal Transportation", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.Government,"SDG Data", "Government", SDGIndicatorsDownloaded, 1.5, .5)
pause(waitlength)
plotfunc(EFSDGData11.GrossFixedCapitalFormation,"SDG Data", "Gross Fixed Capital Formation", SDGIndicatorsDownloaded, 1.5, .5)
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
