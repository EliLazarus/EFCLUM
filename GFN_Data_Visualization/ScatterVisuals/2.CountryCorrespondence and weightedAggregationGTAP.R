library(data.table)
#library(dplyr)
library(splitstackshape)
library(WDI)

"Set working directory to top level directory in console"
##eg. 
setwd("C:\\Users\\Eli\\GitFolders\\EFCLUM\\GFN_Data_Visualization\\ScatterVisuals")

#read in previous World Bank Data to get country list from WB Data in case of other countries
# and for GTAP group weighted Aggregation (by population) and final output

if(WB_SDG =="WB"){WBData <- read.csv("./IndicesDataWB.csv", header=TRUE, colClasses=NA)
}
if(WB_SDG =="SDG"){WBData <- read.csv("./IndicesDataSDG.csv", header=TRUE, colClasses=NA)
}

#start timer
ptm <- proc.time()

WBData$country <- as.character(WBData$country)
WBData$CLUM_category <- as.character(WBData$CLUM_category)
#deal with weird symbol in country name
WBData$country[grepl("Korea, Dem. Peopl",WBData$country)] <- "Korea, Democratic People's Republic of"
WBData$country[grepl("oire",WBData$country)] <- "Cote d'Ivoire"

GFNtoGTAP <- read.csv("./GFNtoGTAP.csv", header=TRUE, colClasses = NA)
if(WB_SDG == "WB"){
  #Separate Goods bc already in GTAP codes
  GOODS_GTAP <- subset(WBData,WBData[,5]=="Goods")

  #Get correspondence table and add fields for alt spellings
  "Don't need this anymore, pretty sure"
  #GFNtoGTAP$AltGFN1 <- ""; GFNtoGTAP$AltGFN2 <- ""; GFNtoGTAP$AltGFN3 <- ""; GFNtoGTAP$AltGFN4 <- ""; GFNtoGTAP$AltGFN5 <- ""
  
  
    #Take GOODS (w GTAP codes) out of the WBData before it gets mixed up in the country names and aggregation
  WBData <- WBData[!(WBData[,5]=="Goods"),]
  for (i in 1:length(GOODS_GTAP[, 1])) {
    for (j in 1:length(GFNtoGTAP[,1])) {
      ifelse(GOODS_GTAP$country[i] == GFNtoGTAP$GTAP9_Code[j],
             GOODS_GTAP$country[i] <- as.character(GFNtoGTAP$GTAP_name[j]),
             "dunno")
    }
  }
}


#Replace GTAP codes in the Goods with GTAP names for eventual integration with the rest of the indicies


#Get GFN population to use for country aggregation weighting
GFN_Pop <- read.csv("./GFN_Population.csv", header=TRUE, colClasses = NA)
#Deal with wierd characters in population file
GFN_Pop$GFN_Country <- as.character(GFN_Pop$GFN_Country)
GFN_Pop$GFN_Country[grepl("oire",GFN_Pop$GFN_Country)] <- "Cote d'Ivoire"
GFN_Pop$GFN_Country[grepl("R",GFN_Pop$GFN_Country) & grepl("union",GFN_Pop$GFN_Country)] <- "Reunion"
#Subset population table for just years included in case
years <- c(2004,2007,2011)
GFN_yr_Pop <- GFN_Pop[GFN_Pop$Year %in% years,]

####filter to end up with a list of country name not in GFN or drop
# All names in the list need to have alt spelling added as below, or added to WB_drop

# Filter out countries from either list that are already in drop list
WB_drop <- read.csv("./DropTheseCountries.csv",
                    header=TRUE, colClasses=NA)
colnames(WB_drop) <- "country"

#WB_drop <- as.character(WB_drop)
"This whole section is redundant, but because it's not actually in the data and doesn't get used

# Get general list of WB countries and groupings for correspondence in case
WBCountries <- as.data.frame(WDI_data[[2]][,3])
# List of countries in WB DLed data that are not already on the WB_drop list
WBCountries <- filter(WBCountries,!(WBCountries[,1] %in% WB_drop[,1]))
# Filtered out countries that are in the GFN list
WBCountries <- filter(WBCountries,!(WBCountries[,1] %in% GFNtoGTAP$GFN_Name))
# So what remains is countries from the WB list, not in Drop, and not in GFN
"

# List of countries from the WB download that are not already on the WB_drop list
WBData_Countries <- unique(WBData$country[!(WBData$country %in% WB_drop[,1])])
# Filter out countries that are in the GFN names
WB_notGFNlist <- WBData_Countries[!(WBData_Countries %in% GFNtoGTAP$GFN_Name)]



# Any countries on the WB list that don't also match the remainder list from the WB
#WBCountries <- filter(WBCountries,!(WBCountries[,1] %in% WBData_Countries))

# Filter out countries from WB general list
#WB_notGFNlist <- WBData_Countries[!(WBData_Countries[,1] %in% GFNtoGTAP$GFN_Name),1]

#More than redundant
# #THIS IS OVERWRITING
# # Filter out anything from the data not already on the GFN country names list
# WB_notGFNlist <- unique(WBData$country[!(WBData$country %in% GFNtoGTAP$GFN_Name)])

# Redundant pretty sure
# # Filter out countries and aggreagations on the drop list
# WB_notGFNlist <- WB_notGFNlist[!(WB_notGFNlist %in% WB_drop)]

# The list that is left are countries that need spelling updates
# AND Any countries or groupings that haven't been dealt with

"Assign alternate spelling, link to GFN spelling;
update spellings to GFN, and then drop from the 'remainder' list"

Repl_Country_Spellings  <- function(wb,gfn){
  WBData$country[WBData$country==wb]<<-gfn
  WB_notGFNlist <<- WB_notGFNlist[WB_notGFNlist!=wb]
}

Repl_Country_Spellings("Bahamas, The","Bahamas")
Repl_Country_Spellings("Bolivia (Plurinational State of)", "Bolivia")
Repl_Country_Spellings("Czechia" , "Czech Republic")
Repl_Country_Spellings("Cape Verde", "Cabo Verde")
Repl_Country_Spellings("Congo, Dem. Rep.", "Congo, Democratic Republic of")
Repl_Country_Spellings("Democratic Republic of the Congo", "Congo, Democratic Republic of")
Repl_Country_Spellings("Congo, Rep.", "Congo")
Repl_Country_Spellings("Congo Republic", "Congo")
Repl_Country_Spellings("Cote dIvoire", "Cote d'Ivoire")
Repl_Country_Spellings("Cote D'Ivoire", "Cote d'Ivoire")
Repl_Country_Spellings("Curacao", "Curaçao")
Repl_Country_Spellings("CuraÃ§ao", "Curaçao")
Repl_Country_Spellings("Egypt, Arab Rep.", "Egypt")
Repl_Country_Spellings("Micronesia", "Micronesia, Federated States of")
Repl_Country_Spellings("Micronesia, Fed. Sts.", "Micronesia, Federated States of")
Repl_Country_Spellings("Gambia, The", "Gambia")
Repl_Country_Spellings("Hong Kong SAR, China", "China Hong Kong SAR")
Repl_Country_Spellings("Hong Kong, Special Administrative Region of China", "China Hong Kong SAR")
Repl_Country_Spellings("Hong Kong, China", "China Hong Kong SAR")
Repl_Country_Spellings("China, Hong Kong Special Administrative Region", "China Hong Kong SAR")
Repl_Country_Spellings("Iran, Islamic Rep.", "Iran, Islamic Republic of")
Repl_Country_Spellings("Iran", "Iran, Islamic Republic of")
Repl_Country_Spellings("Iran (Islamic Republic of)", "Iran, Islamic Republic of")
Repl_Country_Spellings("Kyrgyz Republic", "Kyrgyzstan")
Repl_Country_Spellings("St. Kitts and Nevis", "Saint Kitts and Nevis")
Repl_Country_Spellings("Korea, Rep.", "Korea, Republic of")
Repl_Country_Spellings("South Korea", "Korea, Republic of")
Repl_Country_Spellings("Republic of Korea", "Korea, Republic of")
Repl_Country_Spellings("Korea, Dem. Rep.", "Korea, Democratic People's Republic of")
Repl_Country_Spellings("Democratic People's Republic of Korea", "Korea, Democratic People's Republic of")
Repl_Country_Spellings("Lao PDR", "Lao People's Democratic Republic")
Repl_Country_Spellings("Laos", "Lao People's Democratic Republic")
Repl_Country_Spellings("St. Lucia", "Saint Lucia")
Repl_Country_Spellings("Libya", "Libyan Arab Jamahiriya")
Repl_Country_Spellings("St. Martin (French part)", "Saint-Martin (French Part)")
Repl_Country_Spellings("Macedonia, FYR", "Macedonia TFYR")
Repl_Country_Spellings("North Macedonia", "Macedonia TFYR")
Repl_Country_Spellings("The former Yugoslav Republic of Macedonia", "Macedonia TFYR")
Repl_Country_Spellings("Macao SAR, China", "China, Macao SAR")
Repl_Country_Spellings("Republic of Moldova", "Moldova")
Repl_Country_Spellings("Korea, Dem. People’s Rep.", "Korea, Democratic People's Republic of")
Repl_Country_Spellings("RÃ©union", "Réunion")
Repl_Country_Spellings("São Tomé and Principe", "Sao Tome and Principe")
Repl_Country_Spellings("Slovak Republic", "Slovakia")
Repl_Country_Spellings("eSwatini", "Swaziland")
Repl_Country_Spellings("Eswatini", "Swaziland")
Repl_Country_Spellings("Sint Maarten (Dutch part)", "Sint Maarten (Dutch Part)")
Repl_Country_Spellings("Sint Maarten (Dutch part)\t", "Sint Maarten (Dutch Part)")
Repl_Country_Spellings("Saint Martin (French Part)","Saint-Martin (French Part)")
Repl_Country_Spellings("Sudan [former]", "Sudan (former)")
Repl_Country_Spellings("St. Vincent and the Grenadines", "Saint Vincent and Grenadines")
Repl_Country_Spellings("Saint Vincent and the Grenadines", "Saint Vincent and Grenadines")
Repl_Country_Spellings("Taiwan", "Taiwan, Republic of China")
Repl_Country_Spellings("Taiwan, Republic of China", "Taiwan, Republic of China")
Repl_Country_Spellings("Taiwan, China", "Taiwan, Republic of China")
Repl_Country_Spellings("Tanzania", "Tanzania, United Republic of")
Repl_Country_Spellings("United Republic of Tanzania", "Tanzania, United Republic of")
Repl_Country_Spellings("United States", "United States of America")
Repl_Country_Spellings("United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
Repl_Country_Spellings("Venezuela, RB", "Venezuela, Bolivarian Republic of")
Repl_Country_Spellings("Venezuela (Bolivarian Republic of)", "Venezuela, Bolivarian Republic of")
Repl_Country_Spellings("Virgin Islands (U.S.)", "US Virgin Islands")
Repl_Country_Spellings("Vietnam", "Viet Nam")
Repl_Country_Spellings("Yemen, Rep.", "Yemen")

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
            "/n")))

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
write.csv(ZeroPopTest, "./GFNCountries_w_0_Population")
# Have to filter out countries with no population (or other weighting data)
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

if(WB_SDG == "WB"){
#Set up GFN-GTAP table for merge
GOODS_GTAP <- subset(GOODS_GTAP, select= -X)
colnames(GOODS_GTAP)[1] <- "GTAP_Region"
}
WBGFN_GTAP <- subset(WBGFN_GTAP, select= -X)
colnames(WBGFN_GTAP)[1] <- "GTAP_Region"

#Stick 'em together
GTAP_WBweighted <- rbind(WBGTAP_weighted,WBGFN_GTAP,if(WB_SDG == "WB") GOODS_GTAP)

write.csv(GTAP_WBweighted, "./WBIndicators_byGTAP.csv")

NFA_CLUM <- read.csv("./NFA_2017_CLUM.csv", header=TRUE, colClasses=NA)

NFA_CLUM_WB <- merge(NFA_CLUM, GTAP_WBweighted, by.x = c("year", "clum7_name", "GTAP_name"), 
by.y = c("year", "CLUM_category", "GTAP_Region"), sort = TRUE)

CLUMQScore <- read.csv("./CLUM_QScore.csv")
NFA_CLUM_WB$QScore <- CLUMQScore$NFA_GTAP_Qscore[match(NFA_CLUM_WB$GTAP_name,CLUMQScore$GTAP.Only)]
NFA_CLUM_WB$Continents <- CLUMQScore$Continents[match(NFA_CLUM_WB$GTAP_name,CLUMQScore$GTAP.Only)]


if(WB_SDG =="WB"){
  write.csv(NFA_CLUM_WB, "./NFA_WB_2017_CLUM.csv")
}
if(WB_SDG =="SDG"){
  write.csv(NFA_CLUM_WB, "./NFA_SDG_2017_CLUM.csv")
}

# If new country correspondence issues came up, repeat WB data pull sripts again once they're dealt with
print("Success: No need to run data pull sript again")
proc.time() - ptm