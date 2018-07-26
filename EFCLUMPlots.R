library(ggplot2)

EFWBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")
EFSDGData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_SDG_2017_CLUM.csv")
ggplot(EFWBData, aes(ZScore_Index, total)) + geom_point() + facet_grid(~ clum7_name, scales = "free")+
  geom_text( label = EFWBData$GTAP_name)
ggplot(EFWBData, aes(MaxMin_Index, total)) + geom_point() + facet_grid(~ clum7_name)
ggplot(EFSDGData, aes(ZScore_Index, total)) + geom_point() + facet_grid(~ clum7_name)
ggplot(EFSDGData, aes(MaxMin_Index, total)) + geom_point() + facet_grid(~ clum7_name)

