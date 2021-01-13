library(ggplot2)
library(grid)
library(gridExtra)

"Set the minimum QScore to be included in the data"
QScoreMin <- 4

#Pull the data from csv files
EFWBData <- read.csv("./private-localonly/2017_GTAPCLUM_WB.csv")
EFSDGData <- read.csv("./private-localonly/2017_GTAPCLUM_SDG.csv")
EFSDGWBData <- read.csv("./private-localonly/2017_GTAPCLUM_WBSDG.csv")
WBIndicatorsDLd_Inc <- read.csv("WBIndicatorsDownloaded-Included.csv")
SDGIndicatorsDld_Inc <- read.csv("SDGIndicatorsDownloaded-Included.csv")
WBSDGIndicatorsDld_Inc <- read.csv("WBSDGIndicatorsDownloaded-Included.csv")

#subet data by year and QScore to plot
Year = 2011
EFWBData11 <- subset(EFWBData, (EFWBData$year == Year & EFWBData$QScore > QScoreMin))
EFSDGData11 <- subset(EFSDGData,(EFSDGData$year == Year & EFSDGData$QScore > QScoreMin))
EFSDGWBData11 <- subset(EFSDGWBData, (EFSDGWBData$year == Year & EFSDGWBData$QScore > QScoreMin))


Food.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Food")
Goods.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Goods")
Government.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Government")
Housing.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Housing")
Transportation.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Personal Transportation")
Services.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Services")
GFCF.EFWBData11 <- subset(EFWBData11, EFWBData11$clum7_name == "Gross Fixed Capital Formation")


Food.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Food")
Goods.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Goods")
Government.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Government")
Housing.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Housing")
Transportation.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Personal Transportation")
Services.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Services")
GFCF.EFSDGData11 <- subset(EFSDGData11, EFSDGData11$clum7_name == "Gross Fixed Capital Formation")

Food.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Food")
Goods.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Goods")
Government.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Government")
Housing.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Housing")
Transportation.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Personal Transportation")
Services.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Services")
GFCF.EFSDGWBData11 <- subset(EFSDGWBData11, EFSDGWBData11$clum7_name == "Gross Fixed Capital Formation")

# Add a sub-folder to store the plots if there isn't one yet
if (file.exists("Plots")){print("Plots directory already exists")
} else {
  dir.create("Plots/")
}

#Redundant note: color.codes<-as.character(c("#3399FF", "#FF0000", "#000000"))

plotfunc <- function(data, title, CLUMcat, IndDL, height1, height2){
  myColors <-   c("gold","black", "forestgreen", "royalblue", "darkorange2", "grey30", "grey70", "purple3")
  names(myColors) <- levels(data$Continents)
  colScale <- scale_colour_manual(name = "Continents",values = myColors)
  p <- ggplot(data, aes(ZScore_Index, total , colour = Continents)) +
    geom_point(size = 3) +
    geom_text(label = data$GTAP_name,hjust=-.2, alpha=0.55, colour="grey") +
    colScale +
    #Remove the below line to get EF values on the y-axis
    scale_y_continuous(breaks=c(0,max(data$total, na.rm = TRUE)), labels=c("0", "High"), limits = c(0,max(data$total, na.rm = TRUE))) +
    labs(title = paste(title,"CLUM category:",data$clum7_name), y = "EF per capita", x = paste(data$clum7_name ,"z-score Index")) +
    theme(plot.margin = unit(c(1,2,1,1),"cm"), legend.position = c(.1,.7),
          legend.text = element_text(colour="black", size=6),
          legend.key.size = unit(.05,"cm"),
          legend.background = element_rect(fill=alpha ("white",0.4))) 
  gt <- ggplot_gtable(ggplot_build(p)) 
  gt$layout$clip[gt$layout$name == "panel"] <- "off" 
  
  tb <- tableGrob(cbind(as.character(IndDL[,5][IndDL$CLUM==CLUMcat]),
                        stringr::str_wrap(as.character(IndDL[,3][IndDL$CLUM==CLUMcat]),160)),
                  theme = ttheme_default(base_size=8,
                                         core = list(fg_params=list(hjust=0, x=0))))
  # tb <- tableGrob(cbind(subset(IndDL[,5],IndDL$CLUM==CLUMcat),subset(IndDL[,3],IndDL$CLUM==CLUMcat)),
  #                 theme = ttheme_default(base_size=8,
  #                                        core = list(fg_params=list(hjust=0, x=0))))
  # 
  plotname <- paste("Plots/",deparse(substitute(data)),".pdf", sep ="")
  pdf(plotname,width=12,height=13, onefile = FALSE)
  dev.off
  grid.newpage()
  grid.arrange(gt, tb, nrow=2, heights = c(height1, height2), vp=viewport(width=0.98, height=0.98))
  dev.off()
}

# change if no pause(waitlength) between charts wanted
peruse <- "no"
waitlength <- 0
pause <- function (secs){
  if (peruse=="yes"){Sys.sleep(secs)
  } else {
  }
}

#Plot and output all SDG Indicators 
plotfunc(Food.EFSDGData11,"SDG Data", "Food", SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Goods.EFSDGData11,"SDG Data", "Goods",SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Housing.EFSDGData11,"SDG Data", "Housing", SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Services.EFSDGData11,"SDG Data", "Services", SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
try(plotfunc(Transportation.EFSDGData11,"SDG Data", "Personal Transportation", SDGIndicatorsDld_Inc, 1.5, .5))
pause(waitlength)
plotfunc(Government.EFSDGData11,"SDG Data", "Government", SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(GFCF.EFSDGData11,"SDG Data", "GFCF", SDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)

#Plot and output all WB Indicators
plotfunc(Food.EFWBData11,"World Bank Data", "Food",WBIndicatorsDLd_Inc, 1.5, .5)
pause(waitlength)
#Added manual line to WBIndicatorsDLd_Inc for WBGoods to work with function
plotfunc(Goods.EFWBData11,"World Bank Data", "Goods", WBIndicatorsDLd_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Services.EFWBData11,"World Bank Data", "Services",WBIndicatorsDLd_Inc, .4, .3)
pause(waitlength)
plotfunc(Housing.EFWBData11,"World Bank Data", "Housing",WBIndicatorsDLd_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Transportation.EFWBData11,"World Bank Data", "Personal Transportation",WBIndicatorsDLd_Inc, 1.5, .5)
pause(waitlength)
try(plotfunc(GFCF.EFWBData11,"World Bank Data", "GFCF",WBIndicatorsDLd_Inc, 2, .5))
pause(waitlength)
plotfunc(Government.EFWBData11,"World Bank Data", "Government",WBIndicatorsDLd_Inc, 1.6, .8)
pause(waitlength)

#Plot and output all SDG and WB  Combination Indicators
plotfunc(Food.EFSDGWBData11,"SDG & World Bank Data", "Food", WBSDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
#Added manual line to WBIndicatorsDLd_Inc for WBGoods to work with function
plotfunc(Goods.EFSDGWBData11,"SDG & World Bank Data", "Goods", WBSDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Services.EFSDGWBData11,"SDG & World Bank Data", "Services",WBSDGIndicatorsDld_Inc, 1, 1.1)
pause(waitlength)
plotfunc(Housing.EFSDGWBData11,"SDG & World Bank Data", "Housing",WBSDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
plotfunc(Transportation.EFSDGWBData11,"SDG & World Bank Data", "Personal Transportation",WBSDGIndicatorsDld_Inc, 1.5, .5)
pause(waitlength)
try(plotfunc(GFCF.EFSDGWBData11,"SDG & World Bank Data", "GFCF",WBSDGIndicatorsDld_Inc, 1.5, .5))
pause(waitlength)
plotfunc(Government.EFSDGWBData11,"SDG & World Bank Data", "Government",WBSDGIndicatorsDld_Inc, 1.1, .9)
pause(waitlength)