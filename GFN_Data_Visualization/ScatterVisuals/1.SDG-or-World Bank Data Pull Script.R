library(data.table)
# Updated Version of WDI package Installed is required to allow underscores in Indicator names
library(WDI)
library(dplyr)
library(httr)
library(jsonlite)
library(boxr)
library(readr)

###NOTES TO DO
# 2020-12-23 At this point, data series which have multiple values for the same year from the same
# data series (eg. split by sex, urban/rural/All) are just averaged

# Start the clock for the timing
ptm <- proc.time()

if (!grepl("ScatterVisuals",getwd())){
  print("Set working directory to top level directory in console")
  #setwd("C:\\Users\\Eli\\GitFolders\\EFCLUM\\GFN_Data_Visualization\\ScatterVisuals")
  stop()
}

# "Set to use WB (WB_yes_or_no<-1) or SDG (WB_yes_or_no<-0) Indicators"
# WB_yes_or_no <- 0
# 
# if(WB_yes_or_no==1) {
#   WB_SDG <- "WB"
# } else if (WB_yes_or_no == 0) {
#   WB_SDG <- "SDG"
# } else {
#   stop(
#     print(
#       "WB_yes_or_no must be set either to 1 (for WB Indicators) or 0 (for SDG Indicators)"
#     )
#   )
# }
# 'no' for first run, and then changes in the script after a successful download so other parts of script can re-run
if (exists("SDGDLed")){
  first_run <- "no"
} else {
  first_run <- "yes"
}

years <- c(2004, 2007, 2011) #for the World Bank data

# Array of all World Bank Indicator data
if(!exists("WBDLed")){
  WBIndicators <- WDIcache()
  WBIndicatorList <- as.data.frame(WBIndicators[[1]], stringAsFactors=FALSE)
  # Output full Indicator List to csv
  try(write.table(as.matrix(WBIndicatorList), "./AllWBiList.csv", sep=","))
  WBCountries <- as.data.frame(WBIndicators[[2]])
  WBDLed <- "yes"  
}

  # look at a table of indicators based on a search for the term in the "NAME"
  # to look in the description change column vector index from 2 to 3
  "food" -> word ;SearchWB <- WBIndicatorList[grep(word,WBIndicatorList[,2]),] 
  #View(SearchWB)
  
  ######Data Pull Function
# Array of UN SDGIndicators
if (!exists("SDGDLed")){
    " Back-up code to upload data from my Box account "
    #  Run to activate connection for the SDG data file on Box
    # box_auth()
    # SDGIndicators <- box_read("465756736238") #~50,000 less rows than the old one...

    " To download a total csv - **VERY SLOW**. Also can compare to API download "
# CSV_timer <- proc.time()
#  SDGdataCSV <-  read.csv("https://unstats.un.org/sdgs/indicators/database/archive/2019_Q2_AllData_Prior_20190708.csv") 
# CSV_time <- proc.time()- CSV_timer[3] 
  # SDGIndicatorsCSV <- data.frame(unique(cbind(SDGdataCSV[3:5],SDGdataCSV[9:10])))
  # 
  # # Change commas to semi-colons so they don't mess with the csv separators
  # SDGIndicatorsCSV[,3] <- gsub(",",";",SDGIndicatorsCSV[,3])
  # 
  # # Output full Indicator List to csv
  # try(write.table(as.matrix(SDGIndicatorsCSV), "./AllSDGiListCSV.csv", sep=","))
  
    # Get total elements and Fraaction of TotalElements for page size  
    page1 <-
    fromJSON(
      "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011",
      flatten = TRUE
    )
 
  fractionofTotalElements <- 10  
  perpage <- ceiling(page1$totalElements/fractionofTotalElements)
  # start clock for SDG download and recast
  DLtm <- proc.time()
  
  #Inititalise df and SDGpage$data with some nrows
  SDGdata <- data.frame()
  SDGpage <- list(data=1)
  SDGpage$data <- rbind(SDGpage$data,2); nrow(SDGpage$data)
  #SDGpage#SDGpage$data <- 1
  for (i in 1:fractionofTotalElements) {
    DLpagetime <- proc.time()
    # For last section that might not have exactly 1/fractionofTotalElements rows
    if (i==fractionofTotalElements){
      while (nrow(SDGpage$data)<page1$totalElements-length(SDGdata$value)){ 
        try(SDGpage <- fromJSON(paste0("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011&pageSize=",
                                       page1$totalElements-length(SDGdata$value),"&page=",i), flatten=TRUE))        
      }
    }
    #Otherwise download in Sections
    else {
      while (nrow(SDGpage$data)<perpage){ 
        try(SDGpage <- fromJSON(paste0("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/Data?timePeriod=2004&timePeriod=2007&timePeriod=2011&pageSize=",
                                       perpage,"&page=",i), flatten=TRUE))
      }
    }
    message("Page ", i,"/",fractionofTotalElements," : ", round(proc.time() - DLpagetime) [3], " seconds to retrieve.")
    SDGdata <- rbind(SDGdata, SDGpage$data[,1:16])
    SDGpage <- list(data=1)
    SDGpage$data <- rbind(SDGpage$data,2)
  }
  message("~", DLtime <-
            round((proc.time()[3] - DLtm[3]) / 60), " minutes to download")

    # If DL worked, mark it so it doesn't go again even if we need to re-run script
  if (nrow(SDGdata[1])==page1$totalElements){
    SDGDLed <- "yes"}
  
  # Deal with non-digits
  if (any(!grepl("^[<]|^[,]|^[N]|^[>]|^[0-9]|^[-]|^[.]|^[e]",SDGdata$value))){
    stop(print("check for un-dealt with characters in SDGdata$value: 
-> Use SDGdata$value[!grepl('^[<]|^[,]|^[N]|^[A]|^[0-9]|^[-]|^[.]|^[e]',SDGdata$value)] to find them"))
  }
  # Remove NaN's and Ns from data because they're not useful and cleaner than turning to NAs with warning.
  SDGdata <- subset(SDGdata,(!(SDGdata$value %in% c("NaN","N"))))  
  #  Turn all '<'  and '>' into just their ceiling number, remove ","s, and cast as numeric
  SDGdata$value <- as.numeric(gsub("[<]|[,]|[>]","",SDGdata$value))

  # start the clock for recast
  recasttime <- proc.time()
  # Looping through *First 9/10 rows* to re cast the 1st 3 columns so they look nice
  #  loop the loop the loop: by col, and by row per 1/10 rows
  for (k in 1:3) {
    j <- 0
    for (i in 0:9) {
      looprecasttime <- proc.time()
      l <- j + 1
      if (i==9){
        for (j in ((i * ceiling(nrow(SDGdata)/10)) + 1):nrow(SDGdata)) {
          SDGdata[j, k] <- stringr::str_c(SDGdata[[j, k]], collapse = " ")
        }
      }
      else{
        for (j in ((i * perpage) + 1):(perpage * (i + 1) + perpage%%2)) {
          SDGdata[j, k] <- stringr::str_c(SDGdata[[j, k]], collapse = " ")
        }
      }
      
      message(round((proc.time()[3] - looprecasttime[3])), " seconds to recast", i,"th/10 " , l, " to ", j, ", col ", k)
    }
  }
  looprecasttime <- proc.time()


SDGdata[grepl(")",SDGdata$goal),]      
 
  message(round((proc.time()[3] - recasttime[3]) / 60), " rediculous minutes to recast")
  
  # Making full Indicator List from DL, with years 
  SDGIndicators <- data.frame(unique(cbind(SDGdata[3:6],SDGdata[9])))
  
  # Change commas to semi-colons so they don't mess with the csv separators
  SDGIndicators[,3] <- gsub(",",";",SDGIndicators[,3])
  
  # Output full Indicator List to csv
  try(write.table(as.matrix(SDGIndicators), "./AllSDGiList.csv", sep=","))
}

#### List for dropping WB countries not used in correspondence before forming indicators ####
#  drop the known and obvious country groupings in the World Bank List
# This list gets updated after the process in script 2 if unhandled region names are found.
Region_drop <- c("Africa", "Andean Region", "East Asia & Pacific (IBRD-only countries)", 
             "Europe & Central Asia (IBRD-only countries)", "IBRD countries classified as high income", 
             "Latin America & the Caribbean (IBRD-only countries)", 
             "Middle East & North Africa (IBRD-only countries)", 
             "Latin America & Caribbean (IDA & IBRD)", "Middle East & North Africa (IDA & IBRD)", 
             "East Asia & Pacific (IDA & IBRD)", "Sub-Saharan Africa (IDA & IBRD)", 
             "Europe & Central Asia (IDA & IBRD)", 
             "Sub-Saharan Africa (IBRD-only countries)", "Sub-Saharan Africa (IFC classification)", 
             "Sub-Saharan Africa (developing only)", "Sub-Saharan Africa (all income levels)",
             "East Asia and the Pacific (IFC classification)", "Europe and Central Asia (IFC classification)", 
             "Latin America and the Caribbean (IFC classification)", "Middle East and North Africa (IFC classification)", 
             "South Asia (IFC classification)","East Asia & Pacific (IDA-eligible countries)", 
             "Europe & Central Asia (IDA-eligible countries)","IDA countries classified as Fragile Situations", 
             "Latin America & the Caribbean (IDA-eligible countries)", 
             "Middle East & North Africa (IDA-eligible countries)", 
             "IDA countries not classified", "South Asia (all income levels)",
             "Arab World", "East Asia & Pacific (excluding high income)", 
             "Europe & Central Asia (excluding high income)", "South Asia", 
             "Central Europe and the Baltics", "European Union", "Fragile and conflict affected situations", 
             "OECD members", "Small states", "Pacific island small states", 
             "Caribbean small states", "Other small states", "Middle East & North Africa (IDA & IBRD countries)", 
             "Latin America & the Caribbean (IDA & IBRD countries)", "East Asia & Pacific (IDA & IBRD countries)", 
             "South Asia (IDA & IBRD)", "Sub-Saharan Africa (IDA & IBRD countries)", 
             "Europe & Central Asia (IDA & IBRD countries)", "Pre-demographic dividend", 
             "Early-demographic dividend", "Late-demographic dividend", "Post-demographic dividend", 
             "Euro area", "High income", "Heavily indebted poor countries (HIPC)", 
             "IBRD only", "IDA total", "IDA blend", "IDA only", "Latin America & Caribbean (excluding high income)", 
             "Least developed countries: UN classification", "Low income", 
             "Lower middle income", "Low & middle income", "Middle income", 
             "Middle East & North Africa (excluding high income)", "Upper middle income", 
             "IDA only", "Not classified", "East Asia & Pacific", "Europe & Central Asia", 
             "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa", "Sub-Saharan Africa ", 
             "Latin America & Caribbean",  "Middle East & North Africa", "IDA & IBRD total", "North America",
             "Russian Federation - Moscow", "Russian Federation - Saint Petersburg","Pakistan - Karachi",
             "Pakistan - Lahore", "Nigeria - Kano", "Nigeria - Lagos","European Monetary Union",
             "Germany, Fed. Rep. (former)","High income: nonOECD" ,  "High income: OECD","Mexico - Mexico City",
             "Mexico - Monterrey", "India - Delhi","India - Mumbai", "China - Beijing","China - Shanghai",
             "Brazil - Rio de Janeiro","Brazil - SÃ£o Paulo", "Brazil - São Paulo", "Bangladesh - Chittagong","Bangladesh - Dhaka",
             "Japan - Osaka","Japan - Tokyo","North Africa" , "Sub-Saharan Africa excluding South Africa",
             "Sub-Saharan Africa excluding South Africa and Nigeria", "United States - Los Angeles",
             "United States - New York City"," All States in India", " All Union Territories in India",
             " Andaman and Nicobar Islands"," Andhra Pradesh"," Arunachal Pradesh", " Assam" ," Bihar" ,
             " Central Govt. Projects", " Chandigarh", " Chhattisgarh", " D.V.C.", " Dadra and Nagar Haveli",
             " Daman and Diu" , " Delhi" , " Goa" , " Gujarat" , " Haryana" , " Himachal Pradesh",
             " Jammu and Kashmir" , " Jharkhand" , " Karnataka" , " Kerala", " Lakshadweep", " Madhya Pradesh",
             " Maharashtra" , " Manipur" , " Meghalaya" , " Mizoram" , " Nagaland", " Odisha", " Others",
             " Pudducherry" , " Punjab", " Rajasthan" , " Sikkim", " Tamil Nadu", " Tripura" , " Uttar Pradesh" ,
             " Uttarakhand" , " West Bengal", "East Asia & Pacific (all income levels)",
             "Europe & Central Asia (all income levels)", "Latin America & Caribbean (all income levels)",
             "Middle East & North Africa (all income levels)", "IDA 18", "Sub-Saharan Africa (IBRD only)",
             "Sub-Saharan Africa (IDA total)", "South Asia (IDA total)", "South Asia (IBRD only)",
             "Middle East & North Africa (IBRD only)","Middle East & North Africa (IDA total)",
             "Latin America & Caribbean (IDA total)", "Latin America & Caribbean (IBRD only)",
             "Latin America & Caribbean Latin America and the Caribbean", "Latin America & Caribbean ",
             "Latin America and the Caribbean", "IDA countries not classified as fragile situations",
             "IDA countries classified as fragile situations", "East Asia & Pacific (IBRD only)",
             "East Asia & Pacific (IDA total)", "Europe & Central Asia (IBRD only)",
             "Europe & Central Asia (IDA total)", "Indonesia - Jakarta", "Indonesia - Surabaya", "Fragile Situations", 
             "IDA countries in Sub-Saharan Africa not classified as fragile situations ",
             "South Asia (IDA-eligible countries)", 
             "IDA countries in Sub-Saharan Africa classified as fragile situations ",
             "Sub-Saharan Africa (IDA-eligible countries)", 
             "IDA total, excluding Sub-Saharan Africa", 
             "IDA countries not classified as Fragile Situations",
             "IDA countries classified as fragile situations, excluding Sub-Saharan Africa", 
             "IBRD, including blend",  "Latin America & Caribbean Latin America and the Caribbean", 
             "Central America",  "Middle East (developing only)",
             "Non-resource rich Sub-Saharan Africa countries, of which landlocked", 
             "Non-resource rich Sub-Saharan Africa countries",
             "IDA countries not classified as fragile situations, excluding Sub-Saharan Africa", 
             "Resource rich Sub-Saharan Africa countries", "Southern Cone",
             "Sub-Saharan Africa", "Resource rich Sub-Saharan Africa countries, of which oil exporters",
             "Holy See", "United States Virgin Islands","Micronesia (Federated States of)",
             "Falkland Islands (Malvinas)", "Other non-specified areas in Eastern Asia", 
             "Global Partnership for Education", "Lending category not classified",
             "United Kingdom (England and Wales)", "United Kingdom (Scotland)", "United Kingdom (Northern Ireland)",
             "Iraq (Central Iraq)", "Iraq (Kurdistan Region)",
             # British 'dependencies/juristictions', semi-independent islands in the channel, near Normandy, France
             "Jersey", "Guernsey",
             # Carribean territories of the Netherlands
             "Bonaire, Sint Eustatius and Saba",
             # plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo", "Faeroe Islands",
             # plus Macoa bc is it really worth adding to China considering it requires a separate weighted aggreagation process
             "Macao, China", "China, Macao Special Administrative Region",
             # Plus country GFN has but we don't want
             "World",
             # And groupings: would have to be disaggregated 
             "Åland Islands", "Americas", "Asia", "Australia and New Zealand", "British Indian Ocean Territory", "Caribbean",
             "Caucasus and Central Asia", "Central and Southern Asia", "Central Asia",
             "Developed regions (Europe, Cyprus, Israel, Northern America, Japan, Australia & New Zealand)",
             "Developing regions", "Eastern Africa", "Eastern and South-Eastern Asia", "Eastern Asia",
             "Eastern Asia (excluding Japan and China)", "Eastern Asia (excluding Japan)", "Eastern Europe", "Europe",
             "Europe and Northern America", "French Southern Territories", "Heard Island and McDonald Islands",
             "Landlocked developing countries (LLDCs)", "Least Developed Countries (LDCs)", "Melanesia", "Middle Africa",
             "Northern Africa", "Northern Africa (exc. Sudan)", "Northern Africa and Western Asia", "Northern America",
             "Northern Europe", "Oceania", "Oceania (exc. Australia and New Zealand)", "Polynesia", "Saint Barthélemy",
             "Small island developing States (SIDS)", "South-Eastern Asia", "South America",
             "South Georgia and the South Sandwich Islands", "Southern Africa", "Southern Asia",
             "Southern Asia (excluding India)", "Southern Europe", "Sub-Saharan Africa (inc. Sudan)",
             "Svalbard and Jan Mayen Islands", "United States Minor Outlying Islands", "Western Africa", "Western Asia",
             "Western Asia (exc. Armenia, Azerbaijan, Cyprus, Israel and Georgia)", "Western Europe", "Latin America",
             "Other non-specified areas")
# Write to file for reading in country correspondence script
write.csv(Region_drop, file = "./DropTheseCountries.csv", row.names = F)

## WB pull and split section
{
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all", indicator = indicator_list, start = CLUM_startyear, 
                   end = CLUM_endyear, extra = FALSE, cache = NULL)
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  return(DataFrame)
}
#Single year pull function - Actually, better to just group in the minmax zscore
# WB_DataPull_Function <- function(indicator_list, Datayear){
#   DataFrame <- WDI(country = "all",
#                    indicator = indicator_list,
#                    start = Datayear, end = Datayear, extra = FALSE, cache = NULL) 
#   return(DataFrame)
# }

# Select Indicators and organising for World Bank data
#if(WB_SDG=="WB"){
  ######Food Section
  #Cereal Yield (Kg Per Hectare)
  WBFood_Indicators <- c(
    "AG.YLD.CREL.KG", #High is good, low is bad#
    # Agriculture Value Added Per Worker (Constant 2010 US$)
    "EA.PRD.AGRI.KD", #High is good, low is bad# #No download/data available 6/10/18
    # Agriculture Value Added Per Hectare Of Agricultural Land (Constant 1995 US$)
    "EA.PRD.LAND.KD", #High is good, low is bad# #No download/data available 6/10/18
    #Fertilizer Consumption (Kilograms Per Hectare Of Arable Land)
    "AG.CON.FERT.ZS", #High is good, low is bad#
    #Food Production Index (2004-2006 = 100) Food production index covers food crops that are considered edible and that contain nutrients.
    "AG.PRD.FOOD.XD", #High is good, low is bad#
    #Agricultural Machinery, Tractors Per 100 Sq. Km Of Arable Land
    "AG.LND.TRAC.ZS",  #High is good, low is bad#
    #Agricultural Irrigated Land (% Of Total Agricultural Land)
    "AG.LND.IRIG.AG.ZS" #High is good, low is bad#
  )
  
  #High is BAD#
  #how many calories would be needed to lift the undernourished from their status, everything else being constant.
  WBFood_Indicators_reverse <- c(
    "SN.ITK.DFCT", #High is BAD
    #Malnourished Children (Underweight, -2SD) (% Of Children Under 5): Q1 (Lowest)
    "SH.STA.MALN.ZS", #High is BAD
    # Fish Species, Threatened. number of species classified by the IUCN as endangered, vulnerable, rare, indeterminate, out of danger, or insufficiently known
    "EN.FSH.THRD.NO" #High is BAD  #Only 2017 data 6/10/18
  )
  WBFood_Indicators <- c(WBFood_Indicators,WBFood_Indicators_reverse)
  
  WBIndicatorsDownloaded <- subset(WBIndicatorList,WBIndicatorList$indicator %in% WBFood_Indicators)
  WBIndicatorsDownloaded$CLUM <- "Food"
  
  #Actually better to separate just for the minmax and z-score
    #for (i in years){nam <- paste("Food_Data", i, sep = "_"); assign(nam,
  #                                                                 WB_DataPull_Function(Food_Indicators, i))
  #Drop the countries from the Region_drop list
  #assign(print(paste("Food_Data", i, sep="_")),
  #       print(get(paste("Food_Data",i, sep="_")))[!(print(get(paste("Food_Data",
  #                                                       i, sep="_")))$country %in% Region_drop),])
  #}
  
  WBFood_Data <- WB_DataPull_Function(WBFood_Indicators, 2004, 2007, 2011)
  #Filter out regions from the compiled list of non-NFA countries/regions
  WBFood_Data <- WBFood_Data[!(WBFood_Data$country %in% Region_drop),]
  WBFood_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBFood_Indicators_reverse){
    # The if removes the warning from the max function for when vectors are all NA
    if(i %in% colnames(WBFood_Data)){
      if(!all(is.na(WBFood_Data[i]))){
        WBFood_Data[i] <- 0 - WBFood_Data[i] + max(WBFood_Data[i], na.rm = TRUE)
      }
    }
  }
  # Split into the 3 years
  for (i in years){
    nam <- paste(deparse(substitute(WBFood_Data)), i, sep = "_") 
    assign(nam, WBFood_Data[WBFood_Data$year==i,])
  }
  
  remove(WBFood_Indicators, WBFood_Indicators_reverse, WBFood_Data)
  
  ######Government Section
  WBGovernment_Indicators <- c(
    #School Enrollment, Primary And Secondary (Gross), Gender Parity Index (GPI) Gender parity index for gross enrollment ratio in primary and secondary education is the ratio of girls to boys enrolled at primary and secondary levels in public and private schools.
    "SE.ENR.PRSC.FM.ZS", #High is good, low is bad#
    #People Using Basic Sanitation Services (% Of Population)
    "SH.STA.BASS.ZS", #High is good, low is bad#
    #Gross Savings (% Of GDP). Gross savings are calculated as gross national income less total consumption, plus net transfers.
    "NY.GNS.ICTR.ZS", #High is good, low is bad#
    #Current Account Balance (% Of GDP). Current account balance is the sum of net exports of goods and services, net primary income, and net secondary income.
    "BN.CAB.XOKA.GD.ZS", #High is good (I reckon)
    #Expenditure On Education As % Of Total Government Expenditure (%)
    "SE.XPD.TOTL.GB.ZS", #High is good, low is bad#
    #Government Expenditure Per Primary Student (Constant PPP$)
    "UIS.XUNIT.PPPCONST.1.FSGOV", #High is good, low is bad#
    #Percentage Of Teachers In Primary Education Who Are Trained, Both Sexes (%)
    "SE.PRM.TCAQ.ZS", #High is good, low is bad#
    #Domestic Credit To Private Sector (% Of GDP)
    "FS.AST.PRVT.GD.ZS", #High is good, low is bad#
    #Health Expenditure, Public (% Of GDP)
    "SH.XPD.PUBL.ZS", #High is good, low is bad#  #No download/data availability 6/10/18
    #CPIA Economic Management Cluster Average (1=Low To 6=High). The economic management cluster includes macroeconomic management, fiscal policy, and debt policy.
    "IQ.CPA.ECON.XQ", #High is good, low is bad#
    #CPIA Public Sector Management And Institutions Cluster Average (1=Low To 6=High)
    "IQ.CPA.PUBS.XQ", #High is good, low is bad#
    #IDA Resource Allocation Index (1=Low To 6=High). IDA Resource Allocation Index is obtained by calculating the average score for each cluster and then by averaging those scores. For each of 16 criteria countries are rated on a scale of 1 (low) to 6 (high)
    "IQ.CPA.IRAI.XQ", #High is good, low is bad#
    #Proportion Of Seats Held By Women In National Parliaments (%)
    "SG.GEN.PARL.ZS", #High is good, low is bad#
    #CPIA Policies For Social Inclusion/Equity Cluster Average (1=Low To 6=High). The policies for social inclusion and equity cluster includes gender equality, equity of public resource use, building human resources, social protection and labor, and policies and institutions for environmental sustainability.
    "IQ.CPA.SOCI.XQ", #High is good, low is bad#
    #CPIA Structural Policies Cluster Average (1=Low To 6=High). The structural policies cluster includes trade, financial sector, and business regulatory environment.
    "IQ.CPA.STRC.XQ" #High is good, low is bad#
    #Central Government Debt, Total (% Of GDP). Debt is the entire stock of direct government fixed-term contractual obligations to others outstanding on a particular date. It includes domestic and foreign liabilities such as currency and money deposits, securities other...
  )
  WBGovernment_Indicators_reverse <- c(
    #High is BAD
    "GC.DOD.TOTL.GD.ZS", #High is BAD
    #Unemployment, Total (% Of Total Labor Force) (National Estimate)
    "SL.UEM.TOTL.NE.ZS", #High is BAD
    #Net Official Development Assistance Received (Constant 2014 US$)
    "DT.ODA.ODAT.KD", #High is BAD
    #Inflation, Consumer Prices (Annual %)
    "FP.CPI.TOTL.ZG" #High is BAD
  )  
  
  WBGovernment_Indicators <- c(WBGovernment_Indicators, WBGovernment_Indicators_reverse)
  
  WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBGovernment_Indicators),
                                      CLUM="Government"),WBIndicatorsDownloaded)
  WBGovernment_Data <- WB_DataPull_Function(WBGovernment_Indicators, 2004, 2007, 2011)
  WBBovernment_Data <- WBGovernment_Data[!(WBGovernment_Data$country %in% Region_drop),]
  WBGovernment_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBGovernment_Indicators_reverse){WBGovernment_Data[i] <- 0 - WBGovernment_Data[i] + max(WBGovernment_Data[i], na.rm = TRUE)
  }
  
  for (i in years){
    nam <- paste(deparse(substitute(WBGovernment_Data)), i, sep = "_") 
    assign(nam, WBGovernment_Data[WBGovernment_Data$year==i,])
  }
  
  remove(WBGovernment_Indicators, WBGovernment_Indicators_reverse, WBGovernment_Data)
  
  ##Services Metrics
  WBServices_Indicators <- c(
    # School enrollment, primary and secondary (gross), gender parity index (GPI) 
    "SE.ENR.PRSC.FM.ZS",
    # Hospital beds (per 1,000 people) .  Hospital beds include inpatient beds available in public, private, general, and specialized hospitals and rehabilitation centers. In most cases beds for both acute and chronic care are included. 
    "SH.MED.BEDS.ZS",
    # Fixed telephone subscriptions (per 100 people) .  Fixed telephone subscriptions refers to the sum of active number of analogue fixed telephone lines, voice-over-IP (VoIP) subscriptions, fixed wireless local loop (WLL) subscriptions, ISDN voice-channel equivalents and fixed public payphones. 
    "IT.MLT.MAIN.P2",
    # Fixed broadband subscriptions (per 100 people) .  Fixed broadband subscriptions refers to fixed subscriptions to high-speed access to the public Internet (a TCP/IP connection), at downstream speeds equal to, or greater than, 256 kbit/s. This includes cable modem, DSL, fiber-to-the-home/building, other fixed (wired)-broadband subscriptions, satellite broadband and terrestrial fixed wireless broadband. This total is measured irrespective of the method of payment. It excludes subscriptions that have access to data communications (including the Internet) via mobile-cellular networks. It should include fixed WiMAX and any other fixed wireless technologies. It includes both residential subscriptions and subscriptions for organizations. 
    "IT.NET.BBND.P2",
    # School enrollment, primary (% net) .  Net enrollment rate is the ratio of children of official school age who are enrolled in school to the population of the corresponding official school age. Primary education provides children with basic reading, writing, and mathematics skills along with an elementary understanding of such subjects as history, geography, natural science, social science, art, and music. 
    "SE.PRM.NENR",
    # Progression to secondary school (%) .  Progression to secondary school refers to the number of new entrants to the first grade of secondary school in a given year as a percentage of the number of students enrolled in the final grade of primary school in the previous year (minus the number of repeaters from the last grade of primary education in the given year). 
    "SE.SEC.PROG.ZS",
    # Persistence to last grade of primary, total (% of cohort) .  Persistence to last grade of primary is the percentage of children enrolled in the first grade of primary school who eventually reach the last grade of primary education. The estimate is based on the reconstructed cohort method. 
    "SE.PRM.PRSL.ZS",
    # Preprimary education, duration (years) .  Preprimary duration refers to the number of grades (years) in preprimary school. 
    "SE.PRE.DURS",
    # School enrollment, primary and secondary (gross), gender parity index (GPI) .  Gender parity index for gross enrollment ratio in primary and secondary education is the ratio of girls to boys enrolled at primary and secondary levels in public and private schools. 
    "SE.ENR.PRSC.FM.ZS",
    # Trained teachers in preprimary education (% of total teachers) .  Trained teachers in preprimary education are the percentage of preprimary school teachers who have received the minimum organized teacher training (pre-service or in-service) required for teaching in a given country. 
    "SE.PRE.TCAQ.ZS",
    # CPIA social protection rating (1=low to 6=high) .  Social protection and labor assess government policies in social protection and labor market regulations that reduce the risk of becoming poor, assist those who are poor to better manage further risks, and ensure a minimal level of welfare to all people. 
    "IQ.CPA.PROT.XQ",
    # Adequacy of social protection and labor programs (% of total welfare of beneficiary households) .  Adequacy of social protection and labor programs (SPL) is measured by the total transfer amount received by the population participating in social insurance, social safety net, and unemployment benefits and active labor market programs as a share of their total welfare. Welfare is defined as the total income or total expenditure of beneficiary households. Estimates include both direct and indirect beneficiaries. 
    "per_allsp.adq_pop_tot",
    # Adequacy of social insurance programs (% of total welfare of beneficiary households) .  Adequacy of social insurance programs is measured by the total transfer amount received by the population participating in social insurance programs as a share of their total welfare. Welfare is defined as the total income or total expenditure of beneficiary households. Social insurance programs include old age contributory pensions (including survivors and disability) and social security and health insurance benefits (including occupational injury benefits, paid sick leave, maternity and other social insurance). Estimates include both direct and indirect beneficiaries. 
    "per_si_allsi.adq_pop_tot",
    # Coverage of social insurance programs (% of population) .  Coverage of social insurance programs shows the percentage of population participating in programs that provide old age contributory pensions (including survivors and disability) and social security and health insurance benefits (including occupational injury benefits, paid sick leave, maternity and other social insurance). Estimates include both direct and indirect beneficiaries. 
    "per_si_allsi.cov_pop_tot",
    # Coverage in extreme poor (<$1.25 a day) (%) - All Social Insurance  .  NULL 
    "per_si_allsi.cov_ep_tot",
    # CPIA financial sector rating (1=low to 6=high) .  Financial sector assesses the structure of the financial sector and the policies and regulations that affect it. 
    "IQ.CPA.FINS.XQ"
  )
  WBServices_Indicators_reverse <- c(
    #High is BAD
    # Pupil-teacher ratio, preprimary .  Preprimary school pupil-teacher ratio is the average number of pupils per teacher in preprimary school. 
    "SE.PRE.ENRL.TC.ZS", #High is BAD
    # Pupil-teacher ratio, primary .  Primary school pupil-teacher ratio is the average number of pupils per teacher in primary school. 
    "SE.PRM.ENRL.TC.ZS" #High is BAD
  )
  
  WBServices_Indicators <- c(WBServices_Indicators, WBServices_Indicators_reverse)
  
  WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBServices_Indicators),
                                      CLUM="Services"),WBIndicatorsDownloaded)
  WBServices_Data <- WB_DataPull_Function(WBServices_Indicators, 2004, 2007, 2011)
  WBServices_Data <- WBServices_Data[!(WBServices_Data$country %in% Region_drop),]
  WBServices_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBServices_Indicators_reverse){WBServices_Data[i] <- 0 - WBServices_Data[i] + max(WBServices_Data[i], na.rm = TRUE)
  }
  
  for (i in years){
    nam <- paste(deparse(substitute(WBServices_Data)), i, sep = "_") 
    assign(nam, WBServices_Data[WBServices_Data$year==i,])
  }
  
  remove(WBServices_Indicators, WBServices_Indicators_reverse, WBServices_Data)
  
  ######Personal Transportation Section
  WBTransport_Indicators <- c(
    # Urban Road Density (KMs Per 1000 Population) .  Urban roads density is measured in KMs of Urban roads in the area (State, District) divided by population in thousands in that area (State, District). Urban roads are roads within a limits of a Municipality, Military Cantonment, Port o a Railway Authority. 
    "IN.TRANSPORT.URBNRD.DENSIT",
    # Rural Road Density (KMs/1000 Population) .  Rural roads density is measured in KMs of rural roads in the area (State, District) divided by population in thousands in that area (State, District). Rural roads are roads within a district for which the specifications are lower than for district roads.  
    "IN.TRANSPORT.RURLRD.DENSIT",
    # Access to an all-season road (% of rural population) .  Access to an all-season road is measured as the proportion of rural people who live within 2 kilometers (typically equivalent to a 20-minute walk) of an all-season road. An all-season road is a road that is motorable all year by the prevailing means of rural transport (often a pick-up or a truck which does not have four-wheel-drive). Predictable interruptions of short duration during inclement weather (e.g. heavy rainfall) are acceptable, particularly on low volume roads. The preferred approach to measuring this indicator is by analysis of household surveys that include appropriate questions about access to transport. 
    "IS.ROD.ALLS.ZS",
    # Railways, passengers carried (million passenger-km) .  Passengers carried by railway are the number of passengers transported by rail times kilometers traveled. 
    "IS.RRS.PASG.KM",
    # Railways, passenger-km (per PPP $ million of GDP) .   
    "IS.RRS.PASG.K2.PP.ZS",
    # Roads, passengers carried (million passenger-km) .  Passengers carried by road are the number of passengers transported by road times kilometers traveled. 
    "IS.ROD.PSGR.K6",
    # Roads, paved (% of total roads) .  Paved roads are those surfaced with crushed stone (macadam) and hydrocarbon binder or bituminized agents, with concrete, or with cobblestones, as a percentage of all the country's roads, measured in length. 
    "IS.ROD.PAVE.ZS",
    #No per cap so outlier for high population countries    # Air transport, passengers carried .  Air passengers carried include both domestic and international aircraft passengers of air carriers registered in the country. 
                # "IS.AIR.PSGR",
    # Mortality caused by road traffic injury (per 100,000 people) .  Mortality caused by road traffic injury is estimated road traffic fatal injury deaths per 100,000 population. 
    "SH.STA.TRAF.P5"
  )
  
  WBTransport_Indicators_reverse <- c(
  )
  
  WBTransport_Indicators <- c(WBTransport_Indicators,WBTransport_Indicators_reverse)
  
  WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBTransport_Indicators),
                                      CLUM="Personal Transportation"),WBIndicatorsDownloaded)
  WBTransport_Data <- WB_DataPull_Function(WBTransport_Indicators, 2004, 2007, 2011)
  
  WBTransport_Data <- WBTransport_Data[!(WBTransport_Data$country %in% Region_drop),]
  WBTransport_Data$iso2c <- NULL
  
  # Reverse the orders for High is BAD
  for (i in years){
    nam <- paste(deparse(substitute(WBTransport_Data)), i, sep = "_") 
    assign(nam, WBTransport_Data[WBTransport_Data$year==i,])
  }
  
  remove(WBTransport_Indicators, WBTransport_Data, WBTransport_Indicators_reverse)
  
  
  ####Housing Section
  WBHousing_Indicators <- c(
    ##Households With Water On The Premises (%)
    "SG.H2O.PRMS.HH.ZS", #High is good#
    ##Number Of Water Insufficiencies In A Typical Month
    "IC.FRM.INFRA.IN6", #High is bad#
    ##Distribution of households by availability of drinking water sources - within the premises
    "IN.POV.HH.DRKNGWATER.WITHIN", #High is goodNot sure what this looks like distribution?
    ##Average number of hours of power outages.
    "IC.ELC.OUTG.HR", #High is bad#
    ##People Using Safely Managed Sanitation Services (% Of Population)
    "SH.STA.SMSS.ZS", #High is good#
    ##Building Quality Control Index (0-15)
    "IC.DCP.BQCI", #High is good#
    ##Main Cooking Fuel: Electricity (% Of Households)
    "SG.COK.ELEC.ZS", #High is good#
    ##Main Cooking Fuel: LPG/Natural Gas/Biogas (% Of Households)
    "SG.COK.LPGN.ZS" #High is good, low is bad#
  )
  WBHousing_Indicators_reverse <- c(
  
    )
  
  WBHousing_Indicators <- c(WBHousing_Indicators,WBHousing_Indicators_reverse)
  
  WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBHousing_Indicators),
                                      CLUM="Housing"),WBIndicatorsDownloaded)
  WBHousing_Data <- WB_DataPull_Function(WBHousing_Indicators, 2004, 2007, 2011)
  #Housing_Data$country <- trimws(Housing_Data$country, which = c("both", "left", "right"))
  
  #Filter out regions on the non-NFA country/region list
  WBHousing_Data <- WBHousing_Data[!(WBHousing_Data$country %in% as.character(Region_drop)),]
  #Drop iso column
  WBHousing_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  
  for (i in years){
    nam <- paste(deparse(substitute(WBHousing_Data)), i, sep = "_") 
    assign(nam, WBHousing_Data[WBHousing_Data$year==i,])
  }
  
  remove(WBHousing_Indicators, WBHousing_Indicators_reverse, WBHousing_Data)
  
  ## Goods is now from different data source - in ass_pov_final.csv
  ##Goods Metrics (Prelminary from Scott) 
  # I'm tempted to use just 1 or two measures of material satisfaction and well-being
  # One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
  # linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
  # Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
  # Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"
  WBGoods_Data <- read.csv("./ass_pov_final.csv")
  # Reverse the orders for High is BAD
  WBGoods_Data$ass_pov_extr <- 0-WBGoods_Data$ass_pov_extr+max(WBGoods_Data$ass_pov_extr, na.rm = TRUE)
  for (i in years){
    nam <- paste(deparse(substitute(WBGoods_Data)), i, sep = "_") 
    assign(nam, WBGoods_Data[WBGoods_Data$year ==i,])
  }
  
  #Add a row for Goods Data, including that it's needed for the plot function
  WBGoodsDL <- cbind.data.frame(
    "No.Code",
    "Percentage of population experiencing 'asset poverty'",
    "People not owning a radio, TV, telephone, bicycle, motorbike, or
    refrigerator, and does not own a car or truck.",
    "Oxford Poverty & Human Development Initiative",
    "Oxford University",
    "Goods"
  )
  colnames(WBGoodsDL) <- colnames(WBIndicatorsDownloaded)
  WBIndicatorsDownloaded <- rbind(WBIndicatorsDownloaded,WBGoodsDL)
  
  # Collect Warnings (listing series not DLoaded) to include in file
  WBIndicators_Nodownloads <- as.data.frame(names(warnings()))
  
  #Output WB Indicators info, and list of what was not downloaded at all at the end
  write.csv(WBIndicatorsDownloaded,"./WBIndicatorsDLed.csv")
  cat("\n Not Downloaded (and other warnings) \n", file = "./WBIndicatorsDLed.csv", append = TRUE) 
  write.table(WBIndicators_Nodownloads,"./WBIndicatorsDLed.csv",
              append = TRUE, col.names=FALSE)
#From if WB  
#}
}
# SDG Data split section
{
### Funtion to select Indiactors and organise (for SDG Indicators data)
SDGCLUMsplit <- function(CLUMcat, Indicators, Indicators_rev){
  # Just making variable names for each CLUM category list of series
  assign(paste(CLUMcat,"reversed", sep = "_"), "")
  assign('All_Indicatorsforward', Indicators, envir = parent.frame())
  assign('All_Indicatorsreversed', Indicators_rev, envir = parent.frame())
  All_Indicators <- c(if(!is.null(All_Indicatorsforward)){All_Indicatorsforward},
                      if(!is.null(All_Indicatorsreversed)){All_Indicatorsreversed})
  Data <- subset(SDGdata,
           SDGdata$series %in% All_Indicators &
           #SDGdata$series %in% c(All_Indicatorsforward,All_Indicatorsreversed) &
             SDGdata$timePeriodStart %in% years)
  Data_cols <- c("series", "geoAreaName", "timePeriodStart", "value")
  Data <- Data[Data_cols]
  Data <- Data[!(Data$geoAreaName %in% Region_drop),]
  #reshape to make each indicator a column
  #setDT(Data)
  ## Deals with the mulitple values for each country for a series in a year (but unclear why they exist)
  Data <-
    try(reshape2::dcast(Data, geoAreaName + timePeriodStart ~ series, value.var = 'value', fun.aggregate=mean))
  # A try to give a message if none of the data series exist
  if(class(Data) == "try-error"){
    error_type <- attr(Data,"condition")
  #  print(class(error_type))
  #  print(error_type$message)
    if(error_type$message == "dims [product 1] do not match the length of object [0]"){
    #   cat("***None of the listed data series exist: you need to look for other data series with\n
    # View(SDGIndicators)\n
    #       or***\n")
      cat("***None of the listed data series exist: you need to look for other data series with\n View(SDGIndicators)\n or in the AllSDGiList.csv ***\n")
    }
  }
  #return(Data)
  # Reverse the orders for High is BAD
  if (paste(CLUMcat,"reversed", sep = "_")!="done"){
    for (i in Indicators_rev) {
      
      # The if removes the warning from the max function for when vectors are all NA
      if (!all(is.na(Data[[i]]))) {
        Data[i] <-
        #  0 - readr::parse_number(Data[[i]]) + readr::parse_number(max(Data[[i]], na.rm = TRUE))
          0 - (Data[[i]]) + (max(Data[[i]], na.rm = TRUE))
      }
    }
    for (i in years) {
      nam <- paste("SDG",CLUMcat,"Data", i, sep = "_")
      assign(nam, Data[Data$timePeriodStart == i,], envir = parent.frame())
    }
    assign(paste(CLUMcat,"reversed", sep = "_"), "done", envir = parent.frame())
  }
}

#if (WB_SDG == "SDG") {
# Initialise the dataframe for holding the data
  SDGIndicatorsDownloaded <- data.frame()
  
  # Organise data series by CLUM category with CLUMsplit function, and add to separate lists for reference
  ####
  
  SDGCLUMsplit("Food",
            c(),
            #Indicators to be Reversed
            c(
              # Proportion of children moderately or severely stunted (%)
              "SH_STA_STNT",
              "SH_STA_STUNT",
              #	Prevalence of undernourishment (%)
              "SN_ITK_DEFC",
              #	Proportion of children moderately or severely wasted (%)
              "SH_STA_WAST",
              #	Proportion of children moderately or severely overweight (%)
              "SN_STA_OVWGT",
              "SH_STA_OVRWGT"))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                          unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                          "Food", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                          unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                          "Food", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,
                                    if(ncol(IndicatorsListF)==4){IndicatorsListF}, 
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})

  
  SDGCLUMsplit("Government",c(
    #  Proportion of population covered by labour market programs (%)
    "SI_COV_LMKT",
          # Countries with procedures in law or policy for participation by service users/communities in planning program in rural drinking-water supply, by level of definition in procedures (10 = Clearly defined; 5 = Not clearly defined ; 0 = NA)
          #    "ER_H2O_PRDU", not in the data for those years
    # Countries that have conducted at least one population and housing census in the last 10 years (1 = YES; 0 = NO)
    "SG_REG_CENSUSN",
    # [World Bank] Proportion of population covered by social assistance programs (%)
    "SI_COV_SOCAST",
    # [World Bank] Proportion of population covered by social insurance programs (%)
    "SI_COV_SOCINS",
    #	Proportion of total government spending on essential services; education (%)
    "SD_XPD_ESED"),
    #Indicators to be Reversed
    c(
      #	Number of victims of intentional homicide per 100;000 population; by sex (victims per 100;000 population)
      "VC_IHR_PSRC",
      #	Proportion of population subjected to physical violence in the previous 12 months; by sex (%)
      "VC_VOV_PHYL",
      #	Proportion of population subjected to sexual violence in the previous 12 months; by sex (%)
      "VC_VOV_SEXL"
    ))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "Government", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "Government", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})
  
  SDGCLUMsplit("Services",c(
    # Health worker density, by type of occupation (per 1,000 population)
    "SH_MED_HEAWOR",
    #  Minimum proficiency in mathematics, by education level and sex (%)
    "SE_MAT_PROF",
    # Schools with access toÂ computers for pedagogical purposes, by education level (%)
    "SE_ACC_COMP",
    # Proportion of teachers who have received at least the minimum organized teacher training (e.g. pedagogical training) pre-service or in-service required for teaching at the relevant level in a given country, by education level (%)
    "SE_TRA_GRDL",
    # Proportion of population with access to electricity, by urban/rural (%)
    "EG_ELC_ACCS",
    # Proportion of population covered by a mobile network, by technology (%)
    "IT_MOB_NTWK",
    # Municipal Solid Waste collection coverage, by cities (%)
    "EN_REF_WASCOL",
    # Number of fixed Internet broadband subscriptions, by speed (number)
    # Removed bc not % #"IT_NET_BBN"),
    #	Fixed Internet broadband subscriptions per 100 inhabitants; by speed (per 100 inhabitants)
    "IT_NET_BBND",
    #	Installed renewable electricity-generating capacity (watts per capita)
    "EG_EGY_RNEW",
    #	Proportion of population using safely managed sanitation services; by urban/rural (%)
    "SH_SAN_SAFE",
    #	Proportion of population covered by at least a 2G mobile network (%)
    "IT_MOB_2GNTWK",
    #	Proportion of population covered by at least a 3G mobile network (%)
    "IT_MOB_3GNTWK",
    #	Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
    "SH_FPL_MTMM"),
    #Indicators to be Reversed
    c(
      # Proportion of population practicing open defecation, by urban/rural (%)
      "SH_SAN_DEFECT"
    ))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "Services", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "Services", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})
 
  SDGCLUMsplit("Goods",c(# Proportion of population using safely managed drinking water services, by urban/rural (%)
    "SH_H2O_SAFE",  # Annual growth rate of real GDP per capita (%)
    "NY_GDP_PCAP",  # Domestic material consumption per capita, by type of raw material (tonnes)
    "EN_MAT_DOMCMPC", # Internet users per 100 inhabitants
    "IT_USE_ii99"),
    #Indicators to be Reversed
    c())
  # Add SDG Indicators elected to the list by CLUM category

  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "Goods", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "Goods", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})
  
  SDGCLUMsplit("Housing", c("SP_ACS_BSRVH2O",
  #Proportion of population using basic drinking water services; by location (%)
  "SP_ACS_BSRVSAN",
  #Proportion of population using basic sanitation services; by location (%)
  "SH_SAN_HNDWSH"
  #Proportion of population with basic handwashing facilities on premises; by urban/rural (%)
  ),
  #Indicators to be Reversed
  c("SH_SAN_DEFECT"
  #Proportion of population practicing open defecation; by urban/rural (%)
  ))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "Housing", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "Housing", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})
  
  SDGCLUMsplit("Transport",
            c(
              #Proportion of the rural population who live within 2 km of an all-season road
              "SP_ROD_R2KM"
            ),
            c(
              # Death rate due to road traffic injuries (per 100,000 population)
              #####No longer available as of 12/20/2020 
              "SH_STA_TRAF"
  ))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "Transport", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "Transport", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})

  SDGCLUMsplit("GFCF", c(
    # Number of automated teller machines (ATMs) per 100,000 adults
    "FB_ATM_TOTL",
    #	Proportion of population with access to electricity; by urban/rural (%)
    "EG_ACS_ELEC",
    # Research and development expenditure as a proportion of GDP (%)
    "GB_XPD_RSDV"), 
    
    #Indicators to be reversed
    c(
    #Removed bc not a proportion
      # Direct agriculture loss attributed to disasters, by hazard type (millions of current United States dollars)
    #"VC_DSR_AGLH"
    ))
  # Add SDG Indicators selected to the list by CLUM category, but only if they are IN the CURRENT data
  IndicatorsListF <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsforward]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsforward]),
                           "GFCF", "Fwd")
  IndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%All_Indicatorsreversed]),
                           unique(SDGIndicators$seriesDescription[SDGIndicators$series%in%All_Indicatorsreversed]),
                           "GFCF", "Reversed")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,if(ncol(IndicatorsListF)==4){IndicatorsListF},
                                    if(ncol(IndicatorsListR)==4){IndicatorsListR})
  
  colnames(SDGIndicatorsDownloaded) <- c("indicator", "description", "CLUM", "Forw_Revd")
  
#From if SDG
#  }
}

######
## Get rid of indicators that have more NAs/NA_factor (eg. 1/2 if NA_factor is 2., .8 if NA_factor is 1.25 etc.
######
NARemove_Fun <- function(data, NA_factor){
  ##Count NAs by column to remove columns with lots of NAs
  na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_count_df <- data.frame(na_count)
  ##Remove columns where more than half of observations are NAs for all 3 years
  na_count_df <- subset(na_count_df, na_count < nrow(data)/NA_factor)
  rownames_tokeep <- rownames(na_count_df)
  ##Keep columns without big number of NAs
  Data_NoNAs <- data[rownames_tokeep]
  return(Data_NoNAs)
}

#TESTING
# data <- SDG_Government_Data_2004
# NA_factor <- 1.02
# na_count <-sapply(SDG_Government_Data_2004, function(y) sum(length(which(is.na(y)))))
# na_count_df <- data.frame(na_count)
# ##Remove columns where more than half of observations are NAs for all 3 years
# na_count_df <- subset(na_count_df, na_count < nrow(SDG_Government_Data_2004)/NA_factor)
# rownames_tokeep <- rownames(na_count_df)
# ##Keep columns without big number of NAs
# Data_NoNAs <- data[rownames_tokeep]
# return(Data_NoNAs)
# # 
# CLUMsplit("Food",
#           c(),
#           #Indicators to be Reversed
#           c("SN_ITK_DEFC","SH_STA_STUNT","SH_STA_OVRWGT"))

# Drop indicators that have more than 1/NA factor proportion of NAs for World Bank (and goods) data
WBFoodData_NoNAs_2004 <- NARemove_Fun(WBFood_Data_2004, 2); remove(WBFood_Data_2004)
WBFoodData_NoNAs_2007 <- NARemove_Fun(WBFood_Data_2007, 2); remove(WBFood_Data_2007)
WBFoodData_NoNAs_2011 <- NARemove_Fun(WBFood_Data_2011, 2); remove(WBFood_Data_2011)
WBGovernmentData_NoNAs_2004 <- NARemove_Fun(WBGovernment_Data_2004, 2); remove(WBGovernment_Data_2004)
WBGovernmentData_NoNAs_2007 <- NARemove_Fun(WBGovernment_Data_2007, 2); remove(WBGovernment_Data_2007)
WBGovernmentData_NoNAs_2011 <- NARemove_Fun(WBGovernment_Data_2011, 2); remove(WBGovernment_Data_2011)
WBServicesData_NoNAs_2004 <- NARemove_Fun(WBServices_Data_2004, 2); remove(WBServices_Data_2004)
WBServicesData_NoNAs_2007 <- NARemove_Fun(WBServices_Data_2007, 2); remove(WBServices_Data_2007)
WBServicesData_NoNAs_2011 <- NARemove_Fun(WBServices_Data_2011, 2); remove(WBServices_Data_2011)
WBTransportData_NoNAs_2004 <- NARemove_Fun(WBTransport_Data_2004, 1.25); remove(WBTransport_Data_2004)
WBTransportData_NoNAs_2007 <- NARemove_Fun(WBTransport_Data_2007, 1.25); remove(WBTransport_Data_2007)
WBTransportData_NoNAs_2011 <- NARemove_Fun(WBTransport_Data_2011, 1.25); remove(WBTransport_Data_2011)
WBHousingData_NoNAs_2004 <- NARemove_Fun(WBHousing_Data_2004, 1.25); remove(WBHousing_Data_2004)
WBHousingData_NoNAs_2007 <- NARemove_Fun(WBHousing_Data_2007, 1.25); remove(WBHousing_Data_2007)
WBHousingData_NoNAs_2011 <- NARemove_Fun(WBHousing_Data_2011, 1.25); remove(WBHousing_Data_2011)
WBGoods_Data_NoNAs_2004 <- NARemove_Fun(WBGoods_Data_2004, 2); remove(WBGoods_Data_2004)
WBGoods_Data_NoNAs_2007 <- NARemove_Fun(WBGoods_Data_2007, 2); remove(WBGoods_Data_2007)
WBGoods_Data_NoNAs_2011 <- NARemove_Fun(WBGoods_Data_2011, 2); remove(WBGoods_Data_2011)

# Drop indicators that have more than 1/NA factor proportion of NAs for SDG data
SDGFoodData_NoNAs_2004 <- NARemove_Fun(SDG_Food_Data_2004, 1.05) #; remove(SDG_Food_Data_2004)
SDGFoodData_NoNAs_2007 <- NARemove_Fun(SDG_Food_Data_2007, 1.05)#; remove(SDG_Food_Data_2007)
SDGFoodData_NoNAs_2011 <- NARemove_Fun(SDG_Food_Data_2011, 1.05)#; remove(SDG_Food_Data_2011)
SDGGovernmentData_NoNAs_2004 <- NARemove_Fun(SDG_Government_Data_2004, 1.025)#; remove(SDG_Government_Data_2004)
SDGGovernmentData_NoNAs_2007 <- NARemove_Fun(SDG_Government_Data_2007, 1.025)#; remove(SDG_Government_Data_2007)
SDGGovernmentData_NoNAs_2011 <- NARemove_Fun(SDG_Government_Data_2011, 1.025)#; remove(SDG_Government_Data_2011)
SDGServicesData_NoNAs_2004 <- NARemove_Fun(SDG_Services_Data_2004, 2)#; remove(SDG_Services_Data_2004)
SDGServicesData_NoNAs_2007 <- NARemove_Fun(SDG_Services_Data_2007, 2)#; remove(SDG_Services_Data_2007)
SDGServicesData_NoNAs_2011 <- NARemove_Fun(SDG_Services_Data_2011, 2)#; remove(SDG_Services_Data_2011)
SDGTransportData_NoNAs_2004 <- NARemove_Fun(SDG_Transport_Data_2004, 1.25)#; remove(SDG_Transport_Data_2004)
SDGTransportData_NoNAs_2007 <- NARemove_Fun(SDG_Transport_Data_2007, 1.25)#; remove(SDG_Transport_Data_2007)
SDGTransportData_NoNAs_2011 <- NARemove_Fun(SDG_Transport_Data_2011, 1.25)#; remove(SDG_Transport_Data_2011)
SDGHousingData_NoNAs_2004 <- NARemove_Fun(SDG_Housing_Data_2004, 1.25)#; remove(SDG_Housing_Data_2004)
SDGHousingData_NoNAs_2007 <- NARemove_Fun(SDG_Housing_Data_2007, 1.25)#; remove(SDG_Housing_Data_2007)
SDGHousingData_NoNAs_2011 <- NARemove_Fun(SDG_Housing_Data_2011, 1.25)#; remove(SDG_Housing_Data_2011)
SDGGoods_Data_NoNAs_2004 <- NARemove_Fun(SDG_Goods_Data_2004, 2)#; remove(SDG_Goods_Data_2004)
SDGGoods_Data_NoNAs_2007 <- NARemove_Fun(SDG_Goods_Data_2007, 2)#; remove(SDG_Goods_Data_2007)
SDGGoods_Data_NoNAs_2011 <- NARemove_Fun(SDG_Goods_Data_2011, 2)#; remove(SDG_Goods_Data_2011)
SDGGFCF_Data_NoNAs_2004 <- NARemove_Fun(SDG_GFCF_Data_2004, 2)#; remove(SDG_GFCF_Data_2004)
SDGGFCF_Data_NoNAs_2007 <- NARemove_Fun(SDG_GFCF_Data_2007, 2)#; remove(SDG_GFCF_Data_2007)
SDGGFCF_Data_NoNAs_2011 <- NARemove_Fun(SDG_GFCF_Data_2011, 2)#; remove(SDG_GFCF_Data_2011)

#
SDGIndicatorsDownloadedNoNAs <- SDGIndicatorsDownloaded[SDGIndicatorsDownloaded$indicator %in%
                                                     unique(c(colnames(SDGFoodData_NoNAs_2004[,-(1:2)]),
      colnames(SDGFoodData_NoNAs_2007[,-(1:2)]), colnames(SDGFoodData_NoNAs_2011[,-(1:2)]),
      colnames(SDGGovernmentData_NoNAs_2004[,-(1:2)]),      colnames(SDGGovernmentData_NoNAs_2007[,-(1:2)]),
      colnames(SDGGovernmentData_NoNAs_2011[,-(1:2)]),      colnames(SDGServicesData_NoNAs_2004[,-(1:2)]),
      colnames(SDGServicesData_NoNAs_2007[,-(1:2)]),      colnames(SDGServicesData_NoNAs_2011[,-(1:2)]),
      colnames(SDGTransportData_NoNAs_2004[,-(1:2)]),      colnames(SDGTransportData_NoNAs_2007[,-(1:2)]),
      colnames(SDGTransportData_NoNAs_2011[,-(1:2)]),      colnames(SDGHousingData_NoNAs_2004[,-(1:2)]),
      colnames(SDGHousingData_NoNAs_2007[,-(1:2)]),      colnames(SDGHousingData_NoNAs_2011[,-(1:2)]),
      colnames(SDGGoods_Data_NoNAs_2004[,-(1:2)]),      colnames(SDGGoods_Data_NoNAs_2007[,-(1:2)]),
      colnames(SDGGoods_Data_NoNAs_2011[,-(1:2)]),      colnames(SDGGFCF_Data_NoNAs_2004[,-(1:2)]),
      colnames(SDGGFCF_Data_NoNAs_2007[,-(1:2)]),      colnames(SDGGFCF_Data_NoNAs_2011[,-(1:2)]))),]

write.csv(SDGIndicatorsDownloadedNoNAs, "./SDGIndicatorsDownloaded.csv")
#Create a min-max range version of all remaining data to normalize btw 0 and 1, then aggregate with Averaging
####Max/Min function calculation####
MaxMin_Fun <- function(data, category){
  colnames_important <- as.data.frame(data[,-c(1:2)])
  datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- data[,i+2]/max(data[,i+2], na.rm = TRUE)
  }
  datamatrix <- as.data.frame(datamatrix)
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  colnames(datamatrix)[ncol(datamatrix)] <- "MaxMin_Index"
  datamatrix$CLUM_category <- category
  datamatrix <- cbind(data[,c(1:2)], datamatrix[,-1])
  datamatrix$NAPercent <- (rowSums(is.na(datamatrix))/max(rowSums(is.na(datamatrix))))*100
  datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-2):ncol(datamatrix))]
  return(datamatrix)
}

#DeBug
# colnames_important <- as.data.frame(FoodData_NoNAs_2004[,-c(1:2), drop = FALSE])
# datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(FoodData_NoNAs_2004))
# for(i in 1:ncol(colnames_important)){
#   datamatrix[,i] <- FoodData_NoNAs_2004[,i+2]/max(FoodData_NoNAs_2004[,i+2], na.rm = TRUE)
# }
# datamatrix <- as.data.frame(datamatrix)
# colnames(datamatrix) <- colnames(colnames_important)
# datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
# colnames(datamatrix)[ncol(datamatrix)] <- "MaxMin_Index"
# datamatrix$CLUM_category <- "Food"
# datamatrix <- cbind(FoodData_NoNAs_2004[,c(1:2)], datamatrix[,-1])
# datamatrix$NAPercent <- (rowSums(is.na(datamatrix))/max(rowSums(is.na(datamatrix))))*100
# datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-2):ncol(datamatrix))]

WBFoodData_MaxMin_2004 <- MaxMin_Fun(WBFoodData_NoNAs_2004, "Food")
WBFoodData_MaxMin_2007 <- MaxMin_Fun(WBFoodData_NoNAs_2007, "Food")
WBFoodData_MaxMin_2011 <- MaxMin_Fun(WBFoodData_NoNAs_2011, "Food")
WBFoodData_MaxMin <- rbind(WBFoodData_MaxMin_2004,WBFoodData_MaxMin_2007,WBFoodData_MaxMin_2011)
remove(WBFoodData_MaxMin_2004,WBFoodData_MaxMin_2007,WBFoodData_MaxMin_2011)
WBGovernmentData_MaxMin_2004 <- MaxMin_Fun(WBGovernmentData_NoNAs_2004, "Government")
WBGovernmentData_MaxMin_2007 <- MaxMin_Fun(WBGovernmentData_NoNAs_2007, "Government")
WBGovernmentData_MaxMin_2011 <- MaxMin_Fun(WBGovernmentData_NoNAs_2011, "Government")
WBGovernmentData_MaxMin <- rbind(WBGovernmentData_MaxMin_2004,WBGovernmentData_MaxMin_2007,WBGovernmentData_MaxMin_2011)
remove(WBGovernmentData_MaxMin_2004,WBGovernmentData_MaxMin_2007,WBGovernmentData_MaxMin_2011)
WBServicesData_MaxMin_2004 <- MaxMin_Fun(WBServicesData_NoNAs_2004, "Services")
WBServicesData_MaxMin_2007 <- MaxMin_Fun(WBServicesData_NoNAs_2007, "Services")
WBServicesData_MaxMin_2011 <- MaxMin_Fun(WBServicesData_NoNAs_2011, "Services")
WBServicesData_MaxMin <- rbind(WBServicesData_MaxMin_2004,WBServicesData_MaxMin_2007,WBServicesData_MaxMin_2011)
remove(WBServicesData_MaxMin_2004,WBServicesData_MaxMin_2007,WBServicesData_MaxMin_2011)
WBTransportData_MaxMin_2004 <- MaxMin_Fun(WBTransportData_NoNAs_2004, "Personal Transportation")
WBTransportData_MaxMin_2007 <- MaxMin_Fun(WBTransportData_NoNAs_2007, "Personal Transportation")
WBTransportData_MaxMin_2011 <- MaxMin_Fun(WBTransportData_NoNAs_2011, "Personal Transportation")
WBTransportData_MaxMin <- rbind(WBTransportData_MaxMin_2004,WBTransportData_MaxMin_2007, WBTransportData_MaxMin_2011)
remove(WBTransportData_MaxMin_2004,WBTransportData_MaxMin_2007, WBTransportData_MaxMin_2011)
#Single column version
######  Transport if there is only 1 column (eg. if the NA_Factor for Transport is 2)
# colnames(TransportData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
# TransportData_NoNAs$MaxMin_Index <- TransportData_NoNAs$MaxMin_Index/max(TransportData_NoNAs$MaxMin_Index, na.rm=TRUE)
# TransportData_NoNAs$CLUM_category <- "Personal Transportation"
# TransportData_NoNAs$NAPercent <- (rowSums(is.na(TransportData_NoNAs))/max(rowSums(is.na(TransportData_NoNAs))))*100
# TransportData_MaxMin <- TransportData_NoNAs[,c(2:6)]

##For WB Housing data, only one column and already 0 to 100
WBHousingData_MaxMin <- rbind(WBHousingData_NoNAs_2004,WBHousingData_NoNAs_2007,WBHousingData_NoNAs_2011)
colnames(WBHousingData_MaxMin) <- c("country", "year", "MaxMin_Index")
WBHousingData_MaxMin$MaxMin_Index <- WBHousingData_MaxMin$MaxMin_Index/100
WBHousingData_MaxMin$CLUM_category <- "Housing"
WBHousingData_MaxMin$NAPercent <- (rowSums(is.na(WBHousingData_MaxMin))/max(rowSums(is.na(WBHousingData_MaxMin))))*100
##For Goods Data, just rename column
WBGoods_Data_MaxMin <- WBGoods_Data
colnames(WBGoods_Data_MaxMin) <- c("country", "year", "MaxMin_Index")
WBGoods_Data_MaxMin$MaxMin_Index <- WBGoods_Data_MaxMin$MaxMin_Index/
  max(WBGoods_Data_MaxMin$MaxMin_Index, na.rm = TRUE)
WBGoods_Data_MaxMin$CLUM_category <- "Goods"
WBGoods_Data_MaxMin$NAPercent <- (rowSums(is.na(WBGoods_Data))/max(rowSums(is.na(WBGoods_Data))))*100

SDGFoodData_MaxMin_2004 <- MaxMin_Fun(SDGFoodData_NoNAs_2004, "Food")
SDGFoodData_MaxMin_2007 <- MaxMin_Fun(SDGFoodData_NoNAs_2007, "Food")
SDGFoodData_MaxMin_2011 <- MaxMin_Fun(SDGFoodData_NoNAs_2011, "Food")
SDGFoodData_MaxMin <- rbind(SDGFoodData_MaxMin_2004,SDGFoodData_MaxMin_2007, SDGFoodData_MaxMin_2011)
  remove(SDGFoodData_MaxMin_2004,SDGFoodData_MaxMin_2007,SDGFoodData_MaxMin_2011)
SDGGovernmentData_MaxMin_2004 <- MaxMin_Fun(SDGGovernmentData_NoNAs_2004, "Government")
SDGGovernmentData_MaxMin_2007 <- MaxMin_Fun(SDGGovernmentData_NoNAs_2007, "Government")
SDGGovernmentData_MaxMin_2011 <- MaxMin_Fun(SDGGovernmentData_NoNAs_2011, "Government")
SDGGovernmentData_MaxMin <- rbind(SDGGovernmentData_MaxMin_2004,SDGGovernmentData_MaxMin_2007, SDGGovernmentData_MaxMin_2011)
  remove(SDGGovernmentData_MaxMin_2004,SDGGovernmentData_MaxMin_2007,SDGGovernmentData_MaxMin_2011)
SDGServicesData_MaxMin_2004 <- MaxMin_Fun(SDGServicesData_NoNAs_2004, "Services")
SDGServicesData_MaxMin_2007 <- MaxMin_Fun(SDGServicesData_NoNAs_2007, "Services")
SDGServicesData_MaxMin_2011 <- MaxMin_Fun(SDGServicesData_NoNAs_2011, "Services")
SDGServicesData_MaxMin <- rbind(SDGServicesData_MaxMin_2004,SDGServicesData_MaxMin_2007,SDGServicesData_MaxMin_2011)
  remove(SDGServicesData_MaxMin_2004,SDGServicesData_MaxMin_2007,SDGServicesData_MaxMin_2011)
#Temp fix to ignore 2004 and 2007 Transport because there is not data right now
try(SDGTransportData_MaxMin_2004 <- MaxMin_Fun(SDGTransportData_NoNAs_2004, "Personal Transportation"))
try(SDGTransportData_MaxMin_2007 <- MaxMin_Fun(SDGTransportData_NoNAs_2007, "Personal Transportation"))
SDGTransportData_MaxMin_2011 <- MaxMin_Fun(SDGTransportData_NoNAs_2011, "Personal Transportation")
SDGTransportData_MaxMin <- rbind(if(exists("SDGTransportData_MaxMin_2004")){SDGTransportData_MaxMin_2004},
                                 if(exists("SDGTransportData_MaxMin_2007")){SDGTransportData_MaxMin_2007},
                                 SDGTransportData_MaxMin_2011)
  remove(SDGTransportData_MaxMin_2004,SDGTransportData_MaxMin_2007, SDGTransportData_MaxMin_2011)
SDGHousingData_MaxMin_2004 <- MaxMin_Fun(SDGHousingData_NoNAs_2004, "Housing")
SDGHousingData_MaxMin_2007 <- MaxMin_Fun(SDGHousingData_NoNAs_2007, "Housing")
SDGHousingData_MaxMin_2011 <- MaxMin_Fun(SDGHousingData_NoNAs_2011, "Housing")
SDGHousingData_MaxMin <- rbind(SDGHousingData_MaxMin_2004, SDGHousingData_MaxMin_2007, SDGHousingData_MaxMin_2011)
  remove(SDGHousingData_MaxMin_2004,SDGHousingData_MaxMin_2007, SDGHousingData_MaxMin_2011)
SDGGoods_Data_MaxMin_2004 <- MaxMin_Fun(SDGGoods_Data_NoNAs_2004, "Goods")
SDGGoods_Data_MaxMin_2007 <- MaxMin_Fun(SDGGoods_Data_NoNAs_2007, "Goods")
SDGGoods_Data_MaxMin_2011 <- MaxMin_Fun(SDGGoods_Data_NoNAs_2011, "Goods")
SDGGoods_Data_MaxMin <- rbind(SDGGoods_Data_MaxMin_2004,SDGGoods_Data_MaxMin_2007, SDGGoods_Data_MaxMin_2011)
  remove(SDGGoods_Data_MaxMin_2004,SDGGoods_Data_MaxMin_2007, SDGGoods_Data_MaxMin_2011)
SDGGFCF_Data_MaxMin_2004 <- MaxMin_Fun(SDGGFCF_Data_NoNAs_2004, "Gross Fixed Capital Formation")
SDGGFCF_Data_MaxMin_2007 <- MaxMin_Fun(SDGGFCF_Data_NoNAs_2007, "Gross Fixed Capital Formation")
SDGGFCF_Data_MaxMin_2011 <- MaxMin_Fun(SDGGFCF_Data_NoNAs_2011, "Gross Fixed Capital Formation")
SDGGFCF_Data_MaxMin <- rbind(SDGGFCF_Data_MaxMin_2004,SDGGFCF_Data_MaxMin_2007, SDGGFCF_Data_MaxMin_2011)
  remove(SDGGFCF_Data_MaxMin_2004,SDGGFCF_Data_MaxMin_2007, SDGGFCF_Data_MaxMin_2011)

##Binding Data together for single spreadsheet
WBMaxMinData <- rbind(WBFoodData_MaxMin, WBGovernmentData_MaxMin, WBServicesData_MaxMin, 
                      WBTransportData_MaxMin, WBHousingData_MaxMin, WBGoods_Data_MaxMin)
colnames(WBMaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NAPercent")

SDGMaxMinData <- rbind(SDGFoodData_MaxMin, SDGGovernmentData_MaxMin, SDGServicesData_MaxMin, 
                       SDGTransportData_MaxMin, SDGHousingData_MaxMin, SDGGoods_Data_MaxMin, 
                       SDGGFCF_Data_MaxMin)
colnames(SDGMaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NAPercent")

########Now for z-score stuff
#### Z-Zcore function calculation####
ZScore_Fun <- function(data, category){
  colnames_important <- as.data.frame(data[,-c(1:2)])
  datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- scale(data[,i+2])
  }
  datamatrix <- as.data.frame(datamatrix)
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  colnames(datamatrix)[ncol(datamatrix)] <- "ZScore_Index"
  datamatrix$CLUM_category <- category
  datamatrix <- cbind(data[,c(1:2)], datamatrix[,-1])
  datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-1):ncol(datamatrix))]
  return(datamatrix)
}

#DeBug
# colnames_important <- as.data.frame(FoodData_NoNAs_2004[,-c(1:2)])
# datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(FoodData_NoNAs_2004))
# for(i in 1:ncol(colnames_important)){
#   datamatrix[,i] <- scale(FoodData_NoNAs_2004[,i+2])
# }
# datamatrix <- as.data.frame(datamatrix)
# colnames(datamatrix) <- colnames(colnames_important)
# datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
# colnames(datamatrix)[ncol(datamatrix)] <- "ZScore_Index"
# datamatrix$CLUM_category <- "Food"
# datamatrix <- cbind(FoodData_NoNAs_2004[,c(1:2)], datamatrix[,-1])
# datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-1):ncol(datamatrix))]


WBFoodData_ZScore_2004 <- ZScore_Fun(WBFoodData_NoNAs_2004, "Food")
WBFoodData_ZScore_2007 <- ZScore_Fun(WBFoodData_NoNAs_2007, "Food")
WBFoodData_ZScore_2011 <- ZScore_Fun(WBFoodData_NoNAs_2011, "Food")
WBFoodData_ZScore <- rbind(WBFoodData_ZScore_2004, WBFoodData_ZScore_2007, WBFoodData_ZScore_2011)
remove(WBFoodData_ZScore_2004, WBFoodData_ZScore_2007, WBFoodData_ZScore_2011)
WBGovernmentData_ZScore_2004 <- ZScore_Fun(WBGovernmentData_NoNAs_2004, "Government")
WBGovernmentData_ZScore_2007 <- ZScore_Fun(WBGovernmentData_NoNAs_2007, "Government")
WBGovernmentData_ZScore_2011 <- ZScore_Fun(WBGovernmentData_NoNAs_2011, "Government")
WBGovernmentData_ZScore <- rbind(WBGovernmentData_ZScore_2004, WBGovernmentData_ZScore_2007, WBGovernmentData_ZScore_2011)
remove(WBGovernmentData_ZScore_2004, WBGovernmentData_ZScore_2007, WBGovernmentData_ZScore_2011)
WBTransportData_ZScore_2004 <- ZScore_Fun(WBTransportData_NoNAs_2004, "Personal Transportation")
WBTransportData_ZScore_2007 <- ZScore_Fun(WBTransportData_NoNAs_2007, "Personal Transportation")
WBTransportData_ZScore_2011 <- ZScore_Fun(WBTransportData_NoNAs_2011, "Personal Transportation")
WBTransportData_ZScore <- rbind(WBTransportData_ZScore_2004, WBTransportData_ZScore_2007, WBTransportData_ZScore_2011)
remove(WBTransportData_ZScore_2004, WBTransportData_ZScore_2007, WBTransportData_ZScore_2011)
WBServicesData_ZScore_2004 <- ZScore_Fun(WBServicesData_NoNAs_2004, "Services")
WBServicesData_ZScore_2007 <- ZScore_Fun(WBServicesData_NoNAs_2007, "Services")
WBServicesData_ZScore_2011 <- ZScore_Fun(WBServicesData_NoNAs_2011, "Services")
WBServicesData_ZScore <- rbind(WBServicesData_ZScore_2004, WBServicesData_ZScore_2007, WBServicesData_ZScore_2011)
remove(WBServicesData_ZScore_2004, WBServicesData_ZScore_2007, WBServicesData_ZScore_2011)
##For Housing data, only one column and already 0 to 1
###HousingData_ZScore <- ZScore_Fun(HousingData_NoNAs, "Housing")
WBZScore_Index <- scale(WBHousingData_MaxMin$MaxMin_Index)
WBHousingData_ZScore <- cbind(WBHousingData_MaxMin[,c(1,2)], WBZScore_Index, WBHousingData_MaxMin[,4])
colnames(WBHousingData_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")
##For Goods Data, just rename column
WBZScore_Index <- scale(WBGoods_Data_MaxMin$MaxMin_Index)
WBGoods_Data_ZScore <- cbind(WBGoods_Data_MaxMin[,c(1:2)], WBZScore_Index,WBGoods_Data_MaxMin[,4])
colnames(WBGoods_Data_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")


##Binding Data together for single spreadsheet
WBZScoreData <- rbind(WBFoodData_ZScore, WBGovernmentData_ZScore, WBServicesData_ZScore, 
                      WBTransportData_ZScore, WBHousingData_ZScore, WBGoods_Data_ZScore)
colnames(WBZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category")

##Combining MaxMin and Z-score datasets
WBIndicesData <- left_join(WBZScoreData, WBMaxMinData, by = c("country", "year", "CLUM_category"))

#write.csv(IndicesData, "./World Bank Data/IndicesData.csv")
write.csv(WBIndicesData, "./IndicesDataWB.csv")

#SDG Z-score and combine with MinMax
SDGFoodData_ZScore_2004 <- ZScore_Fun(SDGFoodData_NoNAs_2004, "Food")
SDGFoodData_ZScore_2007 <- ZScore_Fun(SDGFoodData_NoNAs_2007, "Food")
SDGFoodData_ZScore_2011 <- ZScore_Fun(SDGFoodData_NoNAs_2011, "Food")
SDGFoodData_ZScore <- rbind(SDGFoodData_ZScore_2004, SDGFoodData_ZScore_2007, SDGFoodData_ZScore_2011)
remove(SDGFoodData_ZScore_2004, SDGFoodData_ZScore_2007, SDGFoodData_ZScore_2011)
SDGGovernmentData_ZScore_2004 <- ZScore_Fun(SDGGovernmentData_NoNAs_2004, "Government")
SDGGovernmentData_ZScore_2007 <- ZScore_Fun(SDGGovernmentData_NoNAs_2007, "Government")
SDGGovernmentData_ZScore_2011 <- ZScore_Fun(SDGGovernmentData_NoNAs_2011, "Government")
SDGGovernmentData_ZScore <- rbind(SDGGovernmentData_ZScore_2004, SDGGovernmentData_ZScore_2007, SDGGovernmentData_ZScore_2011)
remove(SDGGovernmentData_ZScore_2004, SDGGovernmentData_ZScore_2007, SDGGovernmentData_ZScore_2011)
# Temp fix for no data in 2004 or 2007 as of 12/22/2020
try(SDGTransportData_ZScore_2004 <- ZScore_Fun(SDGTransportData_NoNAs_2004, "Personal Transportation"))
try(SDGTransportData_ZScore_2007 <- ZScore_Fun(SDGTransportData_NoNAs_2007, "Personal Transportation"))
SDGTransportData_ZScore_2011 <- ZScore_Fun(SDGTransportData_NoNAs_2011, "Personal Transportation")
SDGTransportData_ZScore <- rbind(
  if(exists("SDGTransportData_ZScore_2004")){SDGTransportData_ZScore_2004},
  if(exists("SDGTransportData_ZScore_2007")){SDGTransportData_ZScore_2007},
  SDGTransportData_ZScore_2011)
remove(SDGTransportData_ZScore_2004, SDGTransportData_ZScore_2007, SDGTransportData_ZScore_2011)
SDGServicesData_ZScore_2004 <- ZScore_Fun(SDGServicesData_NoNAs_2004, "Services")
SDGServicesData_ZScore_2007 <- ZScore_Fun(SDGServicesData_NoNAs_2007, "Services")
SDGServicesData_ZScore_2011 <- ZScore_Fun(SDGServicesData_NoNAs_2011, "Services")
SDGServicesData_ZScore <- rbind(SDGServicesData_ZScore_2004, SDGServicesData_ZScore_2007, SDGServicesData_ZScore_2011)
remove(SDGServicesData_ZScore_2004, SDGServicesData_ZScore_2007, SDGServicesData_ZScore_2011)
SDGHousingData_ZScore_2004 <- ZScore_Fun(SDGHousingData_NoNAs_2004, "Housing") # No data
SDGHousingData_ZScore_2007 <- ZScore_Fun(SDGHousingData_NoNAs_2007, "Housing")
SDGHousingData_ZScore_2011 <- ZScore_Fun(SDGHousingData_NoNAs_2011, "Housing")
SDGHousingData_ZScore <- rbind(SDGHousingData_ZScore_2004, SDGHousingData_ZScore_2007, SDGHousingData_ZScore_2011)
  remove(SDGHousingData_ZScore_2004, SDGHousingData_ZScore_2007, SDGHousingData_ZScore_2011)
SDGGoods_Data_ZScore_2004 <- ZScore_Fun(SDGGoods_Data_NoNAs_2004, "Goods")
SDGGoods_Data_ZScore_2007 <- ZScore_Fun(SDGGoods_Data_NoNAs_2007, "Goods")
SDGGoods_Data_ZScore_2011 <- ZScore_Fun(SDGGoods_Data_NoNAs_2011, "Goods")
SDGGoods_Data_ZScore <- rbind(SDGGoods_Data_ZScore_2004, SDGGoods_Data_ZScore_2007, SDGGoods_Data_ZScore_2011)
  remove(SDGGoods_Data_ZScore_2004, SDGGoods_Data_ZScore_2007, SDGGoods_Data_ZScore_2011)
SDGGFCF_Data_ZScore_2004 <- ZScore_Fun(SDGGFCF_Data_NoNAs_2004, "Gross Fixed Capital Formation")
SDGGFCF_Data_ZScore_2007 <- ZScore_Fun(SDGGFCF_Data_NoNAs_2007, "Gross Fixed Capital Formation")
SDGGFCF_Data_ZScore_2011 <- ZScore_Fun(SDGGFCF_Data_NoNAs_2011, "Gross Fixed Capital Formation")
SDGGFCF_Data_ZScore <- rbind(SDGGFCF_Data_ZScore_2004, SDGGFCF_Data_ZScore_2007, SDGGFCF_Data_ZScore_2011)
  remove(SDGGFCF_Data_ZScore_2004, SDGGFCF_Data_ZScore_2007, SDGGFCF_Data_ZScore_2011)

##Binding Data together for single spreadsheet
SDGZScoreData <- rbind(SDGFoodData_ZScore, SDGGovernmentData_ZScore, SDGServicesData_ZScore, 
                       SDGTransportData_ZScore, SDGHousingData_ZScore, SDGGFCF_Data_ZScore, SDGGoods_Data_ZScore)
colnames(SDGZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category")

##Combining MaxMin and Z-score datasets
SDGIndicesData <- left_join(SDGZScoreData, SDGMaxMinData, by = c("country", "year", "CLUM_category"))

write.csv(SDGIndicesData, "./IndicesDataSDG.csv")

cat('Looks good, Run 2.Country Correspondence to make sure all countries and groupings were dealt with\n 
    and do the GTAP weighted aggregation')

proc.time() - ptm
