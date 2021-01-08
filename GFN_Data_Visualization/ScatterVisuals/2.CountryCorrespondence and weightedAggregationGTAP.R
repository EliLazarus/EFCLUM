library(data.table)
#library(dplyr)
library(splitstackshape)
library(WDI)

"Set working directory to top level directory in console"
##eg. setwd("C:\\Users\\Eli\\GitFolders\\EFCLUM\\GFN_Data_Visualization\\ScatterVisuals")

#start timer
ptm <- proc.time()

"Runs whole script to correspond and aggregate to GTAP countries for each data source"
WB_SDG <- list("SDG", "WB", "WBSDG")
for (i in WB_SDG){
  #WB_SDG <- "SDG"
  #WB_SDG <- "WB"
  #WB_SDG <- "WBSDG"
  #read in previous World Bank and SDG Indicator data to get country list in case of unhandled countries
  # and for GTAP group weighted Aggregation (by population) and final output
  if(i =="SDG"){WBData <- read.csv("./IndicesDataSDG.csv", header=TRUE, colClasses=NA)}
  if(i =="WB"){WBData <- read.csv("./IndicesDataWB.csv", header=TRUE, colClasses=NA)}
  if(i =="WBSDG"){WBData <- read.csv("./IndicesDataWBSDG.csv", header=TRUE, colClasses=NA)}
  
  
  WBData$country <- as.character(WBData$country)
  WBData$CLUM_category <- as.character(WBData$CLUM_category)

  # get correspondence of GFN countries to GTAP countries
  GFNtoGTAP <- read.csv("./GFNtoGTAP.csv", header=TRUE, colClasses = NA)
  #Get GFN population to use for country aggregation weighting
  GFN_Pop <- read.csv("./GFN_Population.csv", header=TRUE, colClasses = NA)
  #Deal with wierd characters in population file
  GFN_Pop$GFN_Country <- as.character(GFN_Pop$GFN_Country)
  GFN_Pop$GFN_Country[grepl("oire",GFN_Pop$GFN_Country)] <- "Cote d'Ivoire"
  GFN_Pop$GFN_Country[grepl("R",GFN_Pop$GFN_Country) & grepl("union",GFN_Pop$GFN_Country)] <- "Reunion"
  #Subset population table for just years included in case
  years <- c(2004,2007,2011)
  GFN_yr_Pop <- GFN_Pop[GFN_Pop$Year %in% years,]
  
  
  # if(WB_SDG == "WB"){
  #   #Separate Goods bc already in GTAP codes
  #   GOODS_GTAP <- subset(WBData,WBData[,5]=="Goods")
  #   
  #   #Take GOODS (w GTAP codes) out of the WBData before it gets mixed up in the country names and aggregation
  #   #Replace GTAP codes in the Goods with GTAP names for eventual integration with the rest of the indicies
  #   WBData <- WBData[!(WBData[,5]=="Goods"),]
  #   for (i in 1:length(GOODS_GTAP[, 1])) {
  #     for (j in 1:length(GFNtoGTAP[,1])) {
  #       ifelse(GOODS_GTAP$country[i] == GFNtoGTAP$GTAP9_Code[j],
  #              GOODS_GTAP$country[i] <- as.character(GFNtoGTAP$GTAP_name[j]),
  #              "dunno")
  #     }
  #   }
  # }
  
  #### filter to end up with a list of country name not in GFN or drop ####
  # All names in the list need to have alt spelling added to the RespellDict in Script 1, or added to Region_drop list in Script 1
  # Filter out countries from either list that are already in drop list created in Script 1
  Region_drop <- read.csv("./DropTheseCountries.csv",
                          header=TRUE, colClasses=NA)
  colnames(Region_drop) <- "country"
  
    # List of countries from the WB download that are not already on the Region_drop list
  WBData_Countries <- unique(WBData$country[!(WBData$country %in% Region_drop[,1])])
  # Filter out countries that are in the GFN names
  WB_notGFNlist <- WBData_Countries[!(WBData_Countries %in% GFNtoGTAP$GFN_Name)]
  
  # Throw exception and list if any countries haven't been dealt with
  #  Otherwise go ahead to weighted aggregation
  ifelse(length(WB_notGFNlist) == 0 |
           #No idea why Korea, Dem. People's Rep. won't match properly to get it off the list,
           #but it's being handled
           grepl('Korea, Dem',WB_notGFNlist),
         print("All present and accounted for"),
         stop(print(c('Eli says:: Error:: These countries are not dealt with: either add them to the drop list in Script 1.
or add to the alt spelling code above, then re-run this script from beginning: \n',
                      cat(paste(shQuote(WB_notGFNlist), collapse=", "))),
                    "/n
            Then you potentially have to add to the Drop Countries list in script 1. /n
            OR add alternate spellings in RespellDict in sctipt 1 to match the GFN spellings
            AND...you have to start from Script 1. again")))
  
"Reminder for Eli: the function used to print singly country names to build list:
#as a string dput(WB_notGFNlist[1]) "
  
  "Let the weighted aggregation begin
This groups the countries' data from GFN names into GTAP 9 groupings"
  # Create Data subset with only GFN -> after all dropped and spelling correspondence!
  WB_filt <-WBData[WBData$country %in% GFNtoGTAP$GFN_Name,]
  #And get rid of 'World' because it's in the GFN list so it hasn't been dropped yet 
  WB_filt <- WB_filt[!WB_filt$country=="World",]
  
  #Table of WB countries straight correspondence to GTAP
  WBGFN_GTAP <- WB_filt[WB_filt$country %in% GFNtoGTAP$GTAP_name,]
  #Table of WB not 1:1 in GTAP via GFN name
  WBGFN_notGTAP <- WB_filt[!(WB_filt$country %in% GFNtoGTAP$GTAP_name),]
  
  "Country grouping of WB data to GTAP grouping weighted by population"
  # Initialize Population column and fill population column in WBGFN_notGTAP
  WBGFN_notGTAP$Population <- NA
  for (k in 1:length(WBGFN_notGTAP[, 1])) {
    for (j in 1:length(GFN_yr_Pop[, 1])) {
      ifelse(WBGFN_notGTAP$country[k] == GFN_Pop$GFN_Country[j] & WBGFN_notGTAP$year[k] == GFN_Pop$Year[j],
             WBGFN_notGTAP$Population[k] <- GFN_Pop$Population[j],
             "nope")
    }
  }
  
  # Check countries with no population/weighting data
  ZeroPopTest <- WBGFN_notGTAP$country[is.na(WBGFN_notGTAP$Population)]
  print(unique(ZeroPopTest))
  write.csv(ZeroPopTest, "./GFNCountries_w_0_Population")
  # Have to filter out countries with no population (or other weighting data)
  # Hopefully all small countries,etc. If not, do something, maybe get supplementary population data and add it. 
  WBGFN_notGTAP <- WBGFN_notGTAP[!is.na(WBGFN_notGTAP$Population),]
  
  
  #Initialize GTAP_Region column and add GTAP Regions to WBGFN and for Aggregating WBGFN_noGTAP
  WBGFN_notGTAP$GTAP_Region <- "R"
  for (k in 1:length(WBGFN_notGTAP[, 1])) {
    for (j in 1:length(GFNtoGTAP[,1])) {
      ifelse(WBGFN_notGTAP$country[k] == GFNtoGTAP$GFN_Name[j],
             WBGFN_notGTAP$GTAP_Region[k] <- as.character(GFNtoGTAP$GTAP_name[j]),
             "dunno")
    }
  }
  WBGFN_notGTAP <- subset(WBGFN_notGTAP, select= -X)
  #Create table of aggregated indicators by GTAP Region, na's omitted
  WBGTAP_weighted <- as.data.frame(t(sapply(split(WBGFN_notGTAP, list(WBGFN_notGTAP$GTAP_Region,WBGFN_notGTAP$CLUM_category,
                                                                      WBGFN_notGTAP$year)),
                                            function(x) apply(x[,c(4:6)], 2, weighted.mean, x$Population, na.rm = TRUE))))
  setDT(WBGTAP_weighted, keep.rownames = TRUE )[]
  colnames(WBGTAP_weighted)[1] <- "REgion_year_CLUM"
  WBGTAP_weighted <- cSplit(WBGTAP_weighted, "REgion_year_CLUM", ".")
  colnames(WBGTAP_weighted)[4:6] <- c("GTAP_Region", "CLUM_category", "year")
  WBGTAP_weighted <- WBGTAP_weighted[,c(4,5,6,1,2,3)]
  ## For when Goods was a separate dataset
  # if(WB_SDG == "WB"){
  # #Set up GFN-GTAP table for merge
  # GOODS_GTAP <- subset(GOODS_GTAP, select= -X)
  # colnames(GOODS_GTAP)[1] <- "GTAP_Region"
  # }
  
  WBGFN_GTAP <- subset(WBGFN_GTAP, select= -X)
  colnames(WBGFN_GTAP)[1] <- "GTAP_Region"
  # #Stick 'em together
  GTAP_WBweighted <- rbind(WBGTAP_weighted,WBGFN_GTAP)#,if(WB_SDG == "WB") GOODS_GTAP)
  
  write.csv(GTAP_WBweighted, "./WBIndicators_byGTAP.csv")
  
  NFA_CLUM <- read.csv("./NFA_2017_CLUM.csv", header=TRUE, colClasses=NA)
  
  NFA_CLUM_WB <- merge(NFA_CLUM, GTAP_WBweighted, by.x = c("year", "clum7_name", "GTAP_name"), 
                       by.y = c("year", "CLUM_category", "GTAP_Region"), sort = TRUE)
  
  CLUMQScore <- read.csv("./CLUM_QScore.csv")
  NFA_CLUM_WB$QScore <- CLUMQScore$NFA_GTAP_Qscore[match(NFA_CLUM_WB$GTAP_name,CLUMQScore$GTAP.Only)]
  NFA_CLUM_WB$Continents <- CLUMQScore$Continents[match(NFA_CLUM_WB$GTAP_name,CLUMQScore$GTAP.Only)]
  
  # file with indicator results (min-max and z-score) by GTAP country with all EF CLUM results
  if (i=="WB"){
    write.csv(NFA_CLUM_WB, "./2017_GTAPCLUM_WB.csv")
    print(paste(i, " this WB write to csv was triggered *****"))
  }
  # file with indicator results (min-max and z-score) by GTAP country with all EF CLUM results
  if (i=="SDG"){
    write.csv(NFA_CLUM_WB, "./2017_GTAPCLUM_SDG.csv")
    print(i)
  }
  
  if(i=="WBSDG"){
    write.csv(NFA_CLUM_WB, "./2017_GTAPCLUM_WBSDG.csv")
    print(i)
  }
  # If new country correspondence issues came up, repeat WB data pull sripts again once they're dealt with
  print("Success: No need to run data pull sript again")
  proc.time() - ptm
}
