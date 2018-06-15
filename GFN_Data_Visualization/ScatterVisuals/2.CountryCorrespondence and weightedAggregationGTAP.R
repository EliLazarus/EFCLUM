library(data.table)
library(dplyr)
library(splitstackshape)
library(WDI)

##Todo for Eli
# print warning to variable for saving non-downloaded to file

"Set working directory to top level directory in console"
##eg. setwd("C:\\Users\\Eli\\GitFolders\\EFCLUM")

#read in previous World Bank Data to get country list from WB Data in case of other countries
# and for GTAP group weighted Aggregation (by population) and final output
WBData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/IndicesData.csv", header=TRUE, colClasses=NA)
WBData$country <- as.character(WBData$country)
WBData$CLUM_category <- as.character(WBData$CLUM_category)
#deal with weird symbol in country name
WBData$country[grepl("Korea, Dem. Peopl",WBData$country)] <- "Korea, Democratic People's Republic of"

#Separate Goods bc already in GTAP codes
GOODS_GTAP <- subset(WBData,WBData[,5]=="Goods")
#Take GOODS (w GTAP codes) out of the WBData before it gets mixed up in the country names and aggregation
WBData <- WBData[!(WBData[,5]=="Goods"),]

#Get correspondence table and add fields for alt spellings
GFNtoGTAP <- read.csv("./GFN_Data_Visualization/ScatterVisuals/GFNtoGTAP.csv", header=TRUE, colClasses = NA)
GFNtoGTAP$AltGFN1 <- ""; GFNtoGTAP$AltGFN2 <- ""; GFNtoGTAP$AltGFN3 <- ""; GFNtoGTAP$AltGFN4 <- ""; GFNtoGTAP$AltGFN5 <- ""

#Replace GTAP codes in the Goods with GTAP names for eventual integration with the rest of the indicies
for (i in 1:length(GOODS_GTAP[, 1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {
    ifelse(GOODS_GTAP$country[i] == GFNtoGTAP$GTAP9_Code[j],
           GOODS_GTAP$country[i] <- as.character(GFNtoGTAP$GTAP_name[j]),
           "dunno")
  }
}

#Get GFN population to use for country aggregation weighting
GFN_Pop <- read.csv("./GFN_Data_Visualization/ScatterVisuals/GFN_Population.csv", header=TRUE, colClasses = NA)
#Deal with wierd characters in population file
GFN_Pop$GFN_Country <- as.character(GFN_Pop$GFN_Country)
GFN_Pop$GFN_Country[grepl("oire",GFN_Pop$GFN_Country)] <- "Cote d'Ivoire"
GFN_Pop$GFN_Country[grepl("R",GFN_Pop$GFN_Country) & grepl("union",GFN_Pop$GFN_Country)] <- "Reunion"

# Get general list of WB countries and groupings for correspondence in case
WBCountries <- as.data.frame(WDI_data[[2]][,3])
####filter to end up with a list of country name not in GFN or drop
# All names in the list need to have alt spelling added as below, or added to WB_drop

# Filter out countries from either list that are already in drop list
WB_drop <- read.csv("./GFN_Data_Visualization/ScatterVisuals/DropTheseCountries.csv",
                    header=TRUE, colClasses=NA)
colnames(WB_drop) <- "country"

#WB_drop <- as.character(WB_drop)

# List of countries in WB data that are not already on the WB_drop list
WBData_Countries <- unique(WBData$country[!(WBData$country %in% WB_drop[,1])])
# List of countries from the WB download that are not already on the WB_drop list
WBCountries <- filter(WBCountries,!(WBCountries[,1] %in% WB_drop[,1]))
# Any countries on the WB list that don't match the remaining list of GFN count
WBCountries <- filter(WBCountries,!(WBCountries[,1] %in% WBData_Countries))
# Filter out countries from WB general list
WB_notGFNlist <- WBCountries[!(WBCountries %in% GFNtoGTAP$GFN_Name)]

# Filter out anything from the data not already on the GFN country names list
WB_notGFNlist <- WBData$country[!(WBData$country %in% GFNtoGTAP$GFN_Name)]
# Filter out countries and aggreagations on the drop list
WB_notGFNlist <- WB_notGFNlist[!(WB_notGFNlist %in% WB_drop)]
# The list that is left are countries that need spelling updates
# AND Any countries or groupings that haven't been dealt with

#Update spellings to GFN, and then drop from the 'remainder' list
wb <- "Bahamas, The"; gfn <- "Bahamas" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cape Verde"; gfn <- "Cabo Verde" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Congo, Dem. Rep."; gfn <- "Congo, Democratic Republic of" 
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Congo, Rep."; gfn <- "Congo"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cote d'Ivoire"; gfn <- "Cote d'Ivoire"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cote dIvoire"; gfn <- "Cote d'Ivoire"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Cote D'Ivoire"; gfn <- "Cote d'Ivoire"
GFNtoGTAP$AltGFN3[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Curacao"; gfn <- "Curaçao"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Egypt, Arab Rep."; gfn <- "Egypt"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Micronesia, Fed. Sts."; gfn <- "Micronesia, Federated States of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Gambia, The"; gfn <- "Gambia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Hong Kong SAR, China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Hong Kong, Special Administrative Region of China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Hong Kong, China"; gfn <- "China Hong Kong SAR"
GFNtoGTAP$AltGFN3[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Iran, Islamic Rep."; gfn <- "Iran, Islamic Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Iran"; gfn <- "Iran, Islamic Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Kyrgyz Republic"; gfn <- "Kyrgyzstan"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Kitts and Nevis"; gfn <- "Saint Kitts and Nevis"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Korea, Rep."; gfn <- "Korea, Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "South Korea"; gfn <- "Korea, Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Korea, Dem. Rep."; gfn <- "Korea, Democratic People's Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Lao PDR"; gfn <- "Lao People's Democratic Republic"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Laos"; gfn <- "Lao People's Democratic Republic"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Lucia"; gfn <- "Saint Lucia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Libya"; gfn <- "Libyan Arab Jamahiriya"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Martin (French part)"; gfn <- "Saint-Martin (French Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Macedonia, FYR"; gfn <- "Macedonia TFYR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Macao SAR, China"; gfn <- "China, Macao SAR"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Korea, Dem. People’s Rep."; gfn <- "Korea, Democratic People's Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Slovak Republic"; gfn <- "Slovakia"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Sint Maarten (Dutch part)"; gfn <- "Sint Maarten (Dutch Part)"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Tanzania"; gfn <- "Tanzania, United Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Taiwan"; gfn <- "Taiwan, Republic of China"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Taiwan, Republic of China"; gfn <- "Taiwan, Republic of China"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Taiwan, China"; gfn <- "Taiwan, Republic of China"
GFNtoGTAP$AltGFN3[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "United States"; gfn <- "United States of America"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "St. Vincent and the Grenadines"; gfn <- "Saint Vincent and Grenadines"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Venezuela, RB"; gfn <- "Venezuela, Bolivarian Republic of"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Venezuela (Bolivarian Republic of)"; gfn <- "Venezuela, Bolivarian Republic of"
GFNtoGTAP$AltGFN2[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Virgin Islands (U.S.)"; gfn <- "US Virgin Islands"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Vietnam"; gfn <- "Viet Nam"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]
wb <- "Yemen, Rep." ; gfn <- "Yemen"
GFNtoGTAP$AltGFN1[GFNtoGTAP$GFN_Name == gfn] <- wb; WB_notGFNlist = WB_notGFNlist[WB_notGFNlist!=wb]

#Throw exception and list if any countries haven't been dealt with
#Otherwise go ahead to weighted aggregation
ifelse(length(WB_notGFNlist) == 0 |
         #No idea why Korea, Dem. People's Rep. won't match properly to get it off the list,
         #but it's being handled
         grepl('Korea, Dem',WB_notGFNlist),
       print("All present and accounted for"),
 stop(print(c('Eli says:: Error:: These countries are not dealt with: either add them to the drop list or
the alt spelling code above, then re-run this script from beginning: \n',WB_notGFNlist))))


"Reminder for Eli: the 2 functions used to print country names to build list:
#as a string dput(WB_notGFN[1]) 
#as the whole vector print(WB_notGFN)"


#loop through and update country names to GFN spellings 
for (i in 1:length(WBData[,1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {ifelse(WBData$country[i] == GFNtoGTAP$AltGFN1[j],
                                             WBData$country[i] <- as.character(GFNtoGTAP$GFN_Name[j]),
                                             WBData$country[i] <- WBData$country[i])
  }
}
#2nd loop for spellings that have 2nd alternate
for (i in 1:length(WBData[,1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {ifelse(WBData$country[i] == GFNtoGTAP$AltGFN2[j],
                                             WBData$country[i] <- as.character(GFNtoGTAP$GFN_Name[j]),
                                             WBData$country[i] <- WBData$country[i])
  }
}
#3nd loop for spellings that have 3nd alternate
for (i in 1:length(WBData[,1])) {
  for (j in 1:length(GFNtoGTAP[,1])) {ifelse(WBData$country[i] == GFNtoGTAP$AltGFN3[j],
                                             WBData$country[i] <- as.character(GFNtoGTAP$GFN_Name[j]),
                                             WBData$country[i] <- WBData$country[i])
  }
}

"Let the weighted aggregation begin"
# Create subset with only GFN
WB_filt <-WBData[WBData$country %in% GFNtoGTAP$GFN_Name,]
#And get rid of 'World' because it's in the GFN list so it hasn't been dropped yet 
WB_filt <- WB_filt[!WB_filt$country=="World",]

#Table of WB countries straight correspondence to GTAP
WBGFN_GTAP <- WB_filt[WB_filt$country %in% GFNtoGTAP$GTAP_name,]
#Table of WB not 1:1 in GTAP via GFN name
WBGFN_notGTAP <- WB_filt[!(WB_filt$country %in% GFNtoGTAP$GTAP_name),]

#Subset population table for just years included in case
years <- c(2004,2007,2011)
GFN_yr_Pop <- GFN_Pop[GFN_Pop$Year %in% years,]

"Country grouping of WB data to GTAP grouping weighted by population"
#Initialize Population column and fill population column in WBGFN_notGTAP
WBGFN_notGTAP$Population <- NA
for (i in 1:length(WBGFN_notGTAP[, 1])) {
for (j in 1:length(GFN_yr_Pop[, 1])) {
ifelse(WBGFN_notGTAP$country[i] == GFN_Pop$GFN_Country[j] & WBGFN_notGTAP$year[i] == GFN_Pop$Year[j],
WBGFN_notGTAP$Population[i] <- GFN_Pop$Population[j],
"nope")
}
}

# Check countries with no population/weighting data
ZeroPopTest <- WBGFN_notGTAP$country[is.na(WBGFN_notGTAP$Population)]
print(unique(ZeroPopTest))
#Have to filter out countries with no population (or other weighting data)
# Hopefully all small countries,etc. If not, do something, maybe get supplementary population data and add it. 
WBGFN_notGTAP <- WBGFN_notGTAP[!is.na(WBGFN_notGTAP$Population),]


#Initialize GTAP_Region column and add GTAP Regions to WBGFN and for Aggregating WBGFN_noGTAP
WBGFN_notGTAP$GTAP_Region <- "R"
for (i in 1:length(WBGFN_notGTAP[, 1])) {
for (j in 1:length(GFNtoGTAP[,1])) {
ifelse(WBGFN_notGTAP$country[i] == GFNtoGTAP$GFN_Name[j],
 WBGFN_notGTAP$GTAP_Region[i] <- as.character(GFNtoGTAP$GTAP_name[j]),
 "dunno")
}
}

#Create table of aggregated indicators by GTAP Region, na's omitted
WBGTAP_weighted <- as.data.frame(t(sapply(split(WBGFN_notGTAP, list(WBGFN_notGTAP$GTAP_Region, 
WBGFN_notGTAP$CLUM_category, WBGFN_notGTAP$year)),
function(x) apply(x[,c(4,6:7)], 2, weighted.mean, x$Population, na.rm = TRUE))))
setDT(WBGTAP_weighted, keep.rownames = TRUE )[]
colnames(WBGTAP_weighted)[1] <- "REgion_year_CLUM"
WBGTAP_weighted <- cSplit(WBGTAP_weighted, "REgion_year_CLUM", ".")
colnames(WBGTAP_weighted)[4] <- "GTAP_Region"
colnames(WBGTAP_weighted)[5] <- "CLUM_category"
colnames(WBGTAP_weighted)[6] <- "year"
#GTAP_WBweighted <- cbind(WBGTAP_weighted[,1], year = WBGTAP_weighted$year, WBGTAP_weighted[,2:length(WBGTAP_weighted)])
#GTAP_WBweighted$GTAP_Region <- "ph"; GTAP_WBweighted$year <- 1111; GTAP_WBweighted$CLUM_category <- 'nums' 
#cbind(c(WBGTAP_weighted$GTAP_Region <- "ph", WBGTAP_weighted$CLUM_category <- 'nums', WBGTAP_weighted$year <- 1111, WBGTAP_weighted))

#x <- strsplit(WBGTAP_weighted$REgion_year_CLUM, ".", fixed = TRUE) 

#Set up GFN-GTAP table for merge
GOODS_GTAP <- subset(GOODS_GTAP, select= -X)
colnames(GOODS_GTAP)[1] <- "GTAP_Region"
WBGFN_GTAP <- subset(WBGFN_GTAP, select= -X)
colnames(WBGFN_GTAP)[1] <- "GTAP_Region"

#Stick 'em together
GTAP_WBweighted <- rbind(WBGTAP_weighted,WBGFN_GTAP,GOODS_GTAP)

write.csv(GTAP_WBweighted, "./GFN_Data_Visualization/ScatterVisuals/WBIndicators_byGTAP.csv")

NFA_CLUM <- read.csv("./GFN_Data_Visualization/ScatterVisuals/NFA_2017_CLUM.csv", header=TRUE, colClasses=NA)

NFA_CLUM_WB <- merge(NFA_CLUM, GTAP_WBweighted, by.x = c("year", "clum7_name", "GTAP_name"), 
by.y = c("year", "CLUM_category", "GTAP_Region"), sort = TRUE)

write.csv(NFA_CLUM_WB, "./GFN_Data_Visualization/ScatterVisuals/NFA_WB_2017_CLUM.csv")

# If new country correspondence issues came up, repeat WB data pull sripts again once they're dealt with
print("Success: No need to run data pull sript again")
