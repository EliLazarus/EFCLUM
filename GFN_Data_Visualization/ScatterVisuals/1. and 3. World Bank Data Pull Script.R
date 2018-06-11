library(data.table)
# Updated Version of WDI package Installed is required to allow underscores in Indicator names
library(WDI)
library(dplyr)

# Eli to do
#Countries in the updated data are in the rankings bc ONLY WB_Drop countries are dropped
#But IndicesData is the 

#### Set wd [setwd("path")] to folder with the top level of repo for relative paths to work ####

###############WORLD BANK DATA PULL SCRIPT#############

#Array of all World Bank Indicator data
WBIndicators <- WDIcache()
IndicatorList <- as.data.frame(WBIndicators[[1]])
WBCountries <- as.data.frame(WBIndicators[[2]])

######Food Section
# Depth Of The Food Deficit (Kilocalories Per Person Per Day)
#how many calories would be needed to lift the undernourished from their status, everything else being constant.
FoodDeficit <- "SN.ITK.DFCT" #High is BAD
CerealYield <- "AG.YLD.CREL.KG" #High is good, low is bad#
# Agriculture Value Added Per Worker (Constant 2010 US$)
AgVAPerWorker <- "EA.PRD.AGRI.KD" #High is good, low is bad# #No download/data available 6/10/18
# Agriculture Value Added Per Hectare Of Agricultural Land (Constant 1995 US$)
AgVA <- "EA.PRD.LAND.KD" #High is good, low is bad# #No download/data available 6/10/18
FertilizerCons <- "AG.CON.FERT.ZS" #High is good, low is bad#
FoodProdIndex <- "AG.PRD.FOOD.XD" #High is good, low is bad#
TractorPer100SqKm <- "AG.LND.TRAC.ZS"  #High is good, low is bad#
AgIrrigatedLand <- "AG.LND.IRIG.AG.ZS" #High is good, low is bad#
# Fish Species, Threatened
#number of species classified by the IUCN as endangered, vulnerable, rare, indeterminate, out of danger, or insufficiently known
FishSpeciesThreatened <- "EN.FSH.THRD.NO" #High is BAD  #Only 2017 data 6/10/18

Food_Indicators <- c(FoodDeficit, CerealYield, AgVAPerWorker, AgVA, FertilizerCons, 
                     FoodProdIndex, TractorPer100SqKm, AgIrrigatedLand, FishSpeciesThreatened)

######Government Section
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS" #High is good, low is bad#
SanitationAccess <- "SH.STA.BASS.ZS" #High is good, low is bad#
FDI_NetInflows <- "BN.KLT.DINV.DRS.GDP.ZS" #High is good, low is bad#  #No download/data availability 6/10/18
GrossSavings <- "NY.GNS.ICTR.ZS" #High is good, low is bad#
EducationExpend <- "SE.XPD.TOTL.GB.ZS" #High is good, low is bad#
ExpendPerStudent <- "UIS.XUNIT.PPPCONST.1.FSGOV" #High is good, low is bad#
TeachersPrimaryEd_Trained <- "SE.PRM.TCAQ.ZS" #High is good, low is bad#
DomesticCredit_PrivateSector <- "FS.AST.PRVT.GD.ZS" #High is good, low is bad#
HealthExpend_Total <- "SH.XPD.PUBL.ZS" #High is good, low is bad#  #No download/data availability 6/10/18
CPIAEconManageClusterAvg <- "IQ.CPA.ECON.XQ" #High is good, low is bad#
CPIAPublicManageClusterAvg <- "IQ.CPA.PUBS.XQ" #High is good, low is bad#
IDAResourceAllocIndex <- "IQ.CPA.IRAI.XQ" #High is good, low is bad#
PropSeatsWomenParliament <- "SG.GEN.PARL.ZS" #High is good, low is bad#
CPIASocialInclusion <- "IQ.CPA.SOCI.XQ" #High is good, low is bad#
CPIAStructuralPolicies <- "IQ.CPA.STRC.XQ" #High is good, low is bad#
# Central Government Debt, Total (% Of GDP)
CentralGovtDebt <- "GC.DOD.TOTL.GD.ZS" #High is BAD
CurrentAccountBal <- "BN.CAB.XOKA.GD.ZS" #High is BAD
Unemployment <- "SL.UEM.TOTL.NE.ZS" #High is BAD
NetDevAssistanceReceived <- "DT.ODA.ODAT.KD" #High is BAD
Inflation <- "FP.CPI.TOTL.ZG" #High is BAD
  
Government_Indicators <- c(SchoolEnrollment_GPI, SanitationAccess, CentralGovtDebt, FDI_NetInflows,
                           GrossSavings, CurrentAccountBal, EducationExpend, ExpendPerStudent,
                           TeachersPrimaryEd_Trained, Unemployment, NetDevAssistanceReceived, Inflation,
                           DomesticCredit_PrivateSector, HealthExpend_Total, CPIAEconManageClusterAvg,
                           CPIAPublicManageClusterAvg, IDAResourceAllocIndex, PropSeatsWomenParliament,
                           CPIASocialInclusion, CPIAStructuralPolicies)


##Services Metrics
HospitalBeds <- "SH.MED.BEDS.ZS" #High is good, low is bad#
TelephoneSubscriptions <- "IT.MLT.MAIN.P2" #High is good, low is bad#
BroadbandSubscriptions <- "IT.NET.BBND.P2" #High is good, low is bad#
NetEnrollmentRate <- "SE.PRM.NENR" #High is good, low is bad#
TransitionRate_PrimarytoSecondary <- "SE.SEC.PROG.ZS" #High is good, low is bad#
Persistence_LastGradePrimary <- "SE.PRM.PRSL.ZS" #High is good, low is bad#
PrePrimaryEducation_Duration <- "SE.PRE.DURS" #High is good, low is bad#
#Gender parity of enrollment
SchoolEnrollment_GPI <- "SE.ENR.PRSC.FM.ZS" #High is good, low is bad#
TrainedTeachers_PrePrimary <- "SE.PRE.TCAQ.ZS" #High is good, low is bad#
CPIA_SocialProtection <- "IQ.CPA.PROT.XQ" #High is good, low is bad#
SocialProtectionAdequacy <- "per_allsp.adq_pop_tot" #High is good, low is bad#
SocialInsuranceAdequacy <- "per_si_allsi.adq_pop_tot" #High is good, low is bad#
Coverage_SocialInsurance <- "per_si_allsi.cov_pop_tot" #High is good, low is bad#
Coverage_SocialInsurance_LowestQuintile <- "per_si_allsi.cov_ep_tot" #High is good, low is bad#
CPIA_FinancialSector <- "IQ.CPA.FINS.XQ" #High is good, low is bad#
Pupil_Teacher_Ratio_PrePrimary <- "SE.PRE.ENRL.TC.ZS" #High is BAD
Pupil_Teacher_Ratio_Primary <- "SE.PRM.ENRL.TC.ZS" #High is BAD
  
Services_Indicators <- c(HospitalBeds, TelephoneSubscriptions, BroadbandSubscriptions, NetEnrollmentRate,
                         TransitionRate_PrimarytoSecondary, Persistence_LastGradePrimary, 
                         PrePrimaryEducation_Duration, Pupil_Teacher_Ratio_PrePrimary, Pupil_Teacher_Ratio_Primary, 
                         SchoolEnrollment_GPI, TrainedTeachers_PrePrimary, CPIA_SocialProtection,
                         SocialProtectionAdequacy, SocialInsuranceAdequacy, Coverage_SocialInsurance, 
                         Coverage_SocialInsurance_LowestQuintile, CPIA_FinancialSector)


######Personal Transportation Section
UrbanRoadDensity <- "IN.TRANSPORT.URBNRD.DENSIT" #High is good, low is bad#
RuralRoadDensity <- "IN.TRANSPORT.RURLRD.DENSIT" #High is good, low is bad#
RuralAccessRoads <- "IS.ROD.ALLS.ZS" #High is good, low is bad#
RailPassengers <- "IS.RRS.PASG.KM" #High is good, low is bad# #Need to normalize this by population and country size somehow
RailPassengers_2 <- "IS.RRS.PASG.K2.PP.ZS" #High is good, low is bad# # No download/data available 6/10/18
RoadPassengers <- "IS.ROD.PSGR.K6" #High is good, low is bad# #Need to normalize this by population and country size somehow
# % of roads that are paved
RoadsPaved <- "IS.ROD.PAVE.ZS" #High is good"
#Passengers *times* kms travelled
AirPassengers <- "IS.AIR.PSGR"    #High is good, low is bad#  #Need to normalize this by population and country size somehow (& this is probably a lot of tourists!)
#Traffic Mortality per 100,000 people
DeathsInTraffic <- "SH.STA.TRAF.P5" #High is BAD

Transport_Indicators <- c(UrbanRoadDensity, RuralRoadDensity, RuralAccessRoads, RailPassengers, 
                          RailPassengers_2, RoadPassengers, RoadsPaved, AirPassengers, DeathsInTraffic)

####Housing Section
"##Households With Water On The Premises (%)
SG.H2O.PRMS.HH.ZS #High is good#
##Number Of Water Insufficiencies In A Typical Month
IC.FRM.INFRA.IN6 #High is bad#
##Distribution of households by availability of drinking water sources - within the premises
IN.POV.HH.DRKNGWATER.WITHIN #High is goodNot sure what this looks like distribution?
##Average number of hours of power outages.
IC.ELC.OUTG.HR #High is bad#
##People Using Safely Managed Sanitation Services (% Of Population)
SH.STA.SMSS.ZS #High is good#
##Building Quality Control Index (0-15)
IC.DCP.BQCI #High is good#
##Main Cooking Fuel: Electricity (% Of Households)
SG.COK.ELEC.ZS #High is good#
##Main Cooking Fuel: LPG/Natural Gas/Biogas (% Of Households)
SG.COK.LPGN.ZS #High is good, low is bad#

# Leave out: Percentage of households who do their cooking inside the house # Looks like a gender-health indicator#
SG.COK.HOUS.ZS #High is NOT good Only 5 countries in dataset. 
"

Housing_Indicators <- c("SG.H2O.PRMS.HH.ZS", "IC.FRM.INFRA.IN6", "IN.POV.HH.DRKNGWATER.WITHIN", 
                        "IC.ELC.OUTG.HR", "SH.STA.SMSS.ZS", "IC.DCP.BQCI", "SG.COK.ELEC.ZS",
                        "SG.COK.LPGN.ZS")

## Goods is now from different data source - in ass_pov_final.csv
##Goods Metrics (Prelminary from Scott) 
# I'm tempted to use just 1 or two measures of material satisfaction and well-being
# One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
# linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
# Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
# Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"


######Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = indicator_list,
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL) 
  
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  
  return(DataFrame)
}


##Forming dataframes for each CLUM category
Food_Data <- WB_DataPull_Function(Food_Indicators, 2004, 2007, 2011)
Government_Data <- WB_DataPull_Function(Government_Indicators, 2004, 2007, 2011)
Services_Data <- WB_DataPull_Function(Services_Indicators, 2004, 2007, 2011)
Transport_Data <- WB_DataPull_Function(Transport_Indicators, 2004, 2007, 2011)
Housing_Data <- WB_DataPull_Function(Housing_Indicators, 2004, 2007, 2011)
GoodsData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/ass_pov_final.csv")

####Dropping WB countries not used in correspondence before forming indicators
#drop the known and obvious country groupings in the World Bank List
WB_drop <- c("Africa", "Andean Region", "East Asia & Pacific (IBRD-only countries)", 
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
             "IDA countries not classified", 
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
             "Brazil - Rio de Janeiro","Brazil - SÃ£o Paulo", "Bangladesh - Chittagong","Bangladesh - Dhaka",
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
             "Resource rich Sub-Saharan Africa countries", 
             "Southern Cone",
             "Sub-Saharan Africa",
             "Resource rich Sub-Saharan Africa countries, of which oil exporters", 
             #plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo", 
             #Plus country GFN has but we don't want
             "World")


Food_Data <- Food_Data[!(Food_Data$country %in% WB_drop),]
Government_Data <- Government_Data[!(Government_Data$country %in% WB_drop),]
Services_Data <- Services_Data[!(Services_Data$country %in% WB_drop),]
Transport_Data <- Transport_Data[!(Transport_Data$country %in% WB_drop),]
Housing_Data <- Housing_Data[!(Housing_Data$country %in% WB_drop),]

##NA Removal Function
NARemove_Fun <- function(data, NA_factor){
  ##Count NAs by column to remove columns with lots of NAs
  na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_count_df <- data.frame(na_count)
  ##Remove columns where more than half of observations are NAs
  na_count_df <- subset(na_count_df, na_count < nrow(data)/NA_factor)
  rownames_tokeep <- rownames(na_count_df)
  ##Keep columns without big number of NAs
  Data_NoNAs <- data[rownames_tokeep]
  return(Data_NoNAs)
}

FoodData_NoNAs <- NARemove_Fun(Food_Data, 2)
GovernmentData_NoNAs <- NARemove_Fun(Government_Data, 2)
ServicesData_NoNAs <- NARemove_Fun(Services_Data, 2)
TransportData_NoNAs <- NARemove_Fun(Transport_Data, 1.25)
HousingData_NoNAs <- NARemove_Fun(Housing_Data, 1.25)

####Indices Reversal (Manual)

##Food
Food_Data$SN.ITK.DFCT <- 0-Food_Data$SN.ITK.DFCT+max(Food_Data$SN.ITK.DFCT, na.rm = TRUE)
Food_Data$EN.FSH.THRD.NO <- 0-Food_Data$EN.FSH.THRD.NO+max(Food_Data$SN.ITK.DFCT, na.rm = TRUE)


##Government
Government_Data$GC.DOD.TOTL.GD.ZS <- 0-Government_Data$GC.DOD.TOTL.GD.ZS+
  max(Government_Data$GC.DOD.TOTL.GD.ZS, na.rm = TRUE)
Government_Data$BN.CAB.XOKA.GD.ZS <- 0-Government_Data$BN.CAB.XOKA.GD.ZS+
  max(Government_Data$BN.CAB.XOKA.GD.ZS, na.rm = TRUE)
Government_Data$SL.UEM.TOTL.NE.ZS <- 0-Government_Data$SL.UEM.TOTL.NE.ZS+
  max(Government_Data$SL.UEM.TOTL.NE.ZS, na.rm = TRUE)
Government_Data$DT.ODA.ODAT.KD <- 0-Government_Data$DT.ODA.ODAT.KD+
  max(Government_Data$DT.ODA.ODAT.KD, na.rm = TRUE)
Government_Data$FP.CPI.TOTL.ZG <- 0-Government_Data$FP.CPI.TOTL.ZG+
  max(Government_Data$FP.CPI.TOTL.ZG, na.rm = TRUE)

##Services
Services_Data$SE.PRE.ENRL.TC.ZS <- 0-Services_Data$SE.PRE.ENRL.TC.ZS+
  max(Services_Data$SE.PRE.ENRL.TC.ZS, na.rm = TRUE)
Services_Data$SE.PRM.ENRL.TC.ZS <- 0-Services_Data$SE.PRM.ENRL.TC.ZS+
  max(Services_Data$SE.PRM.ENRL.TC.ZS, na.rm = TRUE)

##Transport (none, no data in the indicator 6/10/18)
# Transport_Data$SH.STA.TRAF.P5 <- 0-Transport_Data$SH.STA.TRAF.P5+
#   max(Transport_Data$SH.STA.TRAF.P5, na.rm = TRUE)

##Housing (none)

##Goods
GoodsData$ass_pov_extr <- 0-GoodsData$ass_pov_extr+max(GoodsData$ass_pov_extr, na.rm = TRUE)


####Max/Min function calculation####
MaxMin_Fun <- function(data, category){
  colnames_important <- data[,-c(1:3)]
  datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- data[,i+3]/max(data[,i+3], na.rm = TRUE)
  }
  datamatrix <- as.data.frame(datamatrix)
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  colnames(datamatrix)[ncol(datamatrix)] <- "MaxMin_Index"
  datamatrix$CLUM_category <- category
  datamatrix <- cbind(data[,c(1:3)], datamatrix)
  datamatrix$NAPercent <- (rowSums(is.na(datamatrix))/max(rowSums(is.na(datamatrix))))*100
  datamatrix <- datamatrix[,c(2:3,(ncol(datamatrix)-2):ncol(datamatrix))]
  return(datamatrix)
}


FoodData_MaxMin <- MaxMin_Fun(FoodData_NoNAs, "Food")
GovernmentData_MaxMin <- MaxMin_Fun(GovernmentData_NoNAs, "Government")
ServicesData_MaxMin <- MaxMin_Fun(ServicesData_NoNAs, "Services")
TransportData_MaxMin <- MaxMin_Fun(TransportData_NoNAs, "Transport")

#Single column version

######  Transport if there is only 1 column (eg. if the NA_Factor for Transport is 2)
# colnames(TransportData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
# TransportData_NoNAs$MaxMin_Index <- TransportData_NoNAs$MaxMin_Index/max(TransportData_NoNAs$MaxMin_Index, na.rm=TRUE)
# TransportData_NoNAs$CLUM_category <- "Transport"
# TransportData_NoNAs$NAPercent <- (rowSums(is.na(TransportData_NoNAs))/max(rowSums(is.na(TransportData_NoNAs))))*100
# TransportData_MaxMin <- TransportData_NoNAs[,c(2:6)]

##For Housing data, only one column and already 0 to 100
HousingData_MaxMin <- HousingData_NoNAs[2:4]
colnames(HousingData_MaxMin) <- c("country", "year", "MaxMin_Index")
HousingData_MaxMin$MaxMin_Index <- HousingData_NoNAs$SH.STA.SMSS.ZS/100
HousingData_MaxMin$CLUM_category <- "Housing"
HousingData_MaxMin$NAPercent <- (rowSums(is.na(HousingData_MaxMin))/max(rowSums(is.na(HousingData_MaxMin))))*100

##For Goods Data, just rename column
colnames(GoodsData) <- c("country", "year", "MaxMin_Index")
GoodsData$MaxMin_Index <- GoodsData$MaxMin_Index/max(GoodsData$MaxMin_Index, na.rm = TRUE)
GoodsData$CLUM_category <- "Goods"
GoodsData$NAPercent <- (rowSums(is.na(GoodsData))/max(rowSums(is.na(GoodsData))))*100
GoodsData_MaxMin <- GoodsData

##Binding Data together for single spreadsheet
MaxMinData <- rbind(FoodData_MaxMin, GovernmentData_MaxMin, ServicesData_MaxMin, 
                    TransportData_MaxMin, HousingData_MaxMin, GoodsData_MaxMin)



########Now for z-score stuff
####Max/Min function calculation####
ZScore_Fun <- function(data, category){
  colnames_important <- data[,-c(1:3)]
  datamatrix <- matrix(ncol = ncol(colnames_important), nrow = nrow(data))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- scale(data[,i+3])
  }
  datamatrix <- as.data.frame(datamatrix)
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  colnames(datamatrix)[ncol(datamatrix)] <- "ZScore_Index"
  datamatrix$CLUM_category <- category
  datamatrix <- cbind(data[,c(1:3)], datamatrix)
  datamatrix <- datamatrix[,c(2:3,(ncol(datamatrix)-1):ncol(datamatrix))]
  return(datamatrix)
}

FoodData_ZScore <- ZScore_Fun(FoodData_NoNAs, "Food")
GovernmentData_ZScore <- ZScore_Fun(GovernmentData_NoNAs, "Government")
ServicesData_ZScore <- ZScore_Fun(ServicesData_NoNAs, "Services")
TransportData_ZScore <- ZScore_Fun(TransportData_NoNAs, "Transport")


##For Housing data, only one column and already 0 to 1
###HousingData_ZScore <- ZScore_Fun(HousingData_NoNAs, "Housing")
ZScore_Index <- scale(HousingData_MaxMin$MaxMin_Index)
HousingData_ZScore <- cbind(HousingData_MaxMin[,c(1,2)], ZScore_Index, HousingData_MaxMin[,4])
colnames(HousingData_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")

##For Goods Data, just rename column
ZScore_Index <- scale(GoodsData$MaxMin_Index)
GoodsData_ZScore <- cbind(GoodsData[,c(1:2)], ZScore_Index,GoodsData[,4])
colnames(GoodsData_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")

##Binding Data together for single spreadsheet
ZScoreData <- rbind(FoodData_ZScore, GovernmentData_ZScore, ServicesData_ZScore, 
                    TransportData_ZScore, HousingData_ZScore, GoodsData_ZScore)

##Combining MaxMin and Z-score datasets
IndicesData <- left_join(ZScoreData, MaxMinData, by = c("country", "year", "CLUM_category"))

#write.csv(IndicesData, "./World Bank Data/IndicesData.csv")
write.csv(IndicesData, "./GFN_Data_Visualization/ScatterVisuals/IndicesData.csv")

