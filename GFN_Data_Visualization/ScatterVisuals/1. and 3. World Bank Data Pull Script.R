library(data.table)
# Updated Version of WDI package Installed is required to allow underscores in Indicator names
library(WDI)
library(dplyr)

# Eli to do
# Finalise/triple check the filtering for country names

#### Set wd [setwd("path")] to folder with the top level of repo for relative paths to work ####

###############WORLD BANK DATA PULL SCRIPT#############

#Array of all World Bank Indicator data
WBIndicators <- WDIcache()
IndicatorList <- as.data.frame(WBIndicators[[1]], stringAsFactors=FALSE)
WBCountries <- as.data.frame(WBIndicators[[2]])

######Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = indicator_list,
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL) 
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  return(DataFrame)
}

######Food Section
#Cereal Yield (Kg Per Hectare)
"AG.YLD.CREL.KG" #High is good, low is bad#
  # Agriculture Value Added Per Worker (Constant 2010 US$)
  "EA.PRD.AGRI.KD" #High is good, low is bad# #No download/data available 6/10/18
  # Agriculture Value Added Per Hectare Of Agricultural Land (Constant 1995 US$)
  "EA.PRD.LAND.KD" #High is good, low is bad# #No download/data available 6/10/18
#Fertilizer Consumption (Kilograms Per Hectare Of Arable Land)
"AG.CON.FERT.ZS" #High is good, low is bad#
#Food Production Index (2004-2006 = 100) Food production index covers food crops that are considered edible and that contain nutrients.
"AG.PRD.FOOD.XD" #High is good, low is bad#
#Agricultural Machinery, Tractors Per 100 Sq. Km Of Arable Land
"AG.LND.TRAC.ZS"  #High is good, low is bad#
#Agricultural Irrigated Land (% Of Total Agricultural Land)
"AG.LND.IRIG.AG.ZS" #High is good, low is bad#

#High is BAD#
#how many calories would be needed to lift the undernourished from their status, everything else being constant.
"SN.ITK.DFCT" #High is BAD
  # Fish Species, Threatened. number of species classified by the IUCN as endangered, vulnerable, rare, indeterminate, out of danger, or insufficiently known
  "EN.FSH.THRD.NO" #High is BAD  #Only 2017 data 6/10/18

Food_Indicators <- c("SN.ITK.DFCT", "AG.YLD.CREL.KG", "EA.PRD.AGRI.KD", "EA.PRD.LAND.KD", "AG.CON.FERT.ZS", "
                     AG.PRD.FOOD.XD", "AG.LND.TRAC.ZS", "AG.LND.IRIG.AG.ZS", "EN.FSH.THRD.NO")
IndicatorsDownloaded <- subset(IndicatorList,IndicatorList$indicator %in% Food_Indicators)
IndicatorsDownloaded$CLUM <- "Food"
Food_Data <- WB_DataPull_Function(Food_Indicators, 2004, 2007, 2011)
remove(Food_Indicators)
# Reverse the orders for High is BAD
Food_Data$SN.ITK.DFCT <- 0-Food_Data$SN.ITK.DFCT+max(Food_Data$SN.ITK.DFCT, na.rm = TRUE)
Food_Data$EN.FSH.THRD.NO <- 0-Food_Data$EN.FSH.THRD.NO+max(Food_Data$SN.ITK.DFCT, na.rm = TRUE)


######Government Section
#School Enrollment, Primary And Secondary (Gross), Gender Parity Index (GPI) Gender parity index for gross enrollment ratio in primary and secondary education is the ratio of girls to boys enrolled at primary and secondary levels in public and private schools.
"SE.ENR.PRSC.FM.ZS" #High is good, low is bad#
#People Using Basic Sanitation Services (% Of Population)
"SH.STA.BASS.ZS" #High is good, low is bad#
#Gross Savings (% Of GDP). Gross savings are calculated as gross national income less total consumption, plus net transfers.
"NY.GNS.ICTR.ZS" #High is good, low is bad#
#Current Account Balance (% Of GDP). Current account balance is the sum of net exports of goods and services, net primary income, and net secondary income.
"BN.CAB.XOKA.GD.ZS" #High is good (I reckon)
#Expenditure On Education As % Of Total Government Expenditure (%)
"SE.XPD.TOTL.GB.ZS" #High is good, low is bad#
#Government Expenditure Per Primary Student (Constant PPP$)
"UIS.XUNIT.PPPCONST.1.FSGOV" #High is good, low is bad#
#Percentage Of Teachers In Primary Education Who Are Trained, Both Sexes (%)
"SE.PRM.TCAQ.ZS" #High is good, low is bad#
#Domestic Credit To Private Sector (% Of GDP)
"FS.AST.PRVT.GD.ZS" #High is good, low is bad#
#Health Expenditure, Public (% Of GDP)
"SH.XPD.PUBL.ZS" #High is good, low is bad#  #No download/data availability 6/10/18
#CPIA Economic Management Cluster Average (1=Low To 6=High). The economic management cluster includes macroeconomic management, fiscal policy, and debt policy.
"IQ.CPA.ECON.XQ" #High is good, low is bad#
#CPIA Public Sector Management And Institutions Cluster Average (1=Low To 6=High)
"IQ.CPA.PUBS.XQ" #High is good, low is bad#
#IDA Resource Allocation Index (1=Low To 6=High). IDA Resource Allocation Index is obtained by calculating the average score for each cluster and then by averaging those scores. For each of 16 criteria countries are rated on a scale of 1 (low) to 6 (high)
"IQ.CPA.IRAI.XQ" #High is good, low is bad#
#Proportion Of Seats Held By Women In National Parliaments (%)
"SG.GEN.PARL.ZS" #High is good, low is bad#
#CPIA Policies For Social Inclusion/Equity Cluster Average (1=Low To 6=High). The policies for social inclusion and equity cluster includes gender equality, equity of public resource use, building human resources, social protection and labor, and policies and institutions for environmental sustainability.
"IQ.CPA.SOCI.XQ" #High is good, low is bad#
#CPIA Structural Policies Cluster Average (1=Low To 6=High). The structural policies cluster includes trade, financial sector, and business regulatory environment.
"IQ.CPA.STRC.XQ" #High is good, low is bad#
#Central Government Debt, Total (% Of GDP). Debt is the entire stock of direct government fixed-term contractual obligations to others outstanding on a particular date. It includes domestic and foreign liabilities such as currency and money deposits, securities other...
"GC.DOD.TOTL.GD.ZS" #High is BAD
#Unemployment, Total (% Of Total Labor Force) (National Estimate)
"SL.UEM.TOTL.NE.ZS" #High is BAD
#Net Official Development Assistance Received (Constant 2014 US$)
"DT.ODA.ODAT.KD" #High is BAD
#Inflation, Consumer Prices (Annual %)
"FP.CPI.TOTL.ZG" #High is BAD
  
Government_Indicators <- c("SE.ENR.PRSC.FM.ZS", "SH.STA.BASS.ZS",  "NY.GNS.ICTR.ZS", "SE.XPD.TOTL.GB.ZS",
                           "UIS.XUNIT.PPPCONST.1.FSGOV", "SE.PRM.TCAQ.ZS","FS.AST.PRVT.GD.ZS", 
                           "SH.XPD.PUBL.ZS", "IQ.CPA.ECON.XQ", "IQ.CPA.PUBS.XQ", "IQ.CPA.IRAI.XQ", 
                           "SG.GEN.PARL.ZS", "DT.ODA.ODAT.KD", "IQ.CPA.SOCI.XQ", "GC.DOD.TOTL.GD.ZS",
                           "IQ.CPA.STRC.XQ", "SL.UEM.TOTL.NE.ZS", "FP.CPI.TOTL.ZG")
IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Government_Indicators),
                                 CLUM="Government"),IndicatorsDownloaded)
Government_Data <- WB_DataPull_Function(Government_Indicators, 2004, 2007, 2011)
remove(Government_Indicators)
# Reverse the orders for High is BAD
Government_Data$GC.DOD.TOTL.GD.ZS <- 0-Government_Data$GC.DOD.TOTL.GD.ZS+
  max(Government_Data$GC.DOD.TOTL.GD.ZS, na.rm = TRUE)
Government_Data$SL.UEM.TOTL.NE.ZS <- 0-Government_Data$SL.UEM.TOTL.NE.ZS+
  max(Government_Data$SL.UEM.TOTL.NE.ZS, na.rm = TRUE)
Government_Data$DT.ODA.ODAT.KD <- 0-Government_Data$DT.ODA.ODAT.KD+
  max(Government_Data$DT.ODA.ODAT.KD, na.rm = TRUE)
Government_Data$FP.CPI.TOTL.ZG <- 0-Government_Data$FP.CPI.TOTL.ZG+
  max(Government_Data$FP.CPI.TOTL.ZG, na.rm = TRUE)

##Services Metrics
# School enrollment, primary and secondary (gross), gender parity index (GPI) 
"SE.ENR.PRSC.FM.ZS"
# Hospital beds (per 1,000 people) .  Hospital beds include inpatient beds available in public, private, general, and specialized hospitals and rehabilitation centers. In most cases beds for both acute and chronic care are included. 
"SH.MED.BEDS.ZS"
# Fixed telephone subscriptions (per 100 people) .  Fixed telephone subscriptions refers to the sum of active number of analogue fixed telephone lines, voice-over-IP (VoIP) subscriptions, fixed wireless local loop (WLL) subscriptions, ISDN voice-channel equivalents and fixed public payphones. 
"IT.MLT.MAIN.P2"
# Fixed broadband subscriptions (per 100 people) .  Fixed broadband subscriptions refers to fixed subscriptions to high-speed access to the public Internet (a TCP/IP connection), at downstream speeds equal to, or greater than, 256 kbit/s. This includes cable modem, DSL, fiber-to-the-home/building, other fixed (wired)-broadband subscriptions, satellite broadband and terrestrial fixed wireless broadband. This total is measured irrespective of the method of payment. It excludes subscriptions that have access to data communications (including the Internet) via mobile-cellular networks. It should include fixed WiMAX and any other fixed wireless technologies. It includes both residential subscriptions and subscriptions for organizations. 
"IT.NET.BBND.P2"
# School enrollment, primary (% net) .  Net enrollment rate is the ratio of children of official school age who are enrolled in school to the population of the corresponding official school age. Primary education provides children with basic reading, writing, and mathematics skills along with an elementary understanding of such subjects as history, geography, natural science, social science, art, and music. 
"SE.PRM.NENR"
# Progression to secondary school (%) .  Progression to secondary school refers to the number of new entrants to the first grade of secondary school in a given year as a percentage of the number of students enrolled in the final grade of primary school in the previous year (minus the number of repeaters from the last grade of primary education in the given year). 
"SE.SEC.PROG.ZS"
# Persistence to last grade of primary, total (% of cohort) .  Persistence to last grade of primary is the percentage of children enrolled in the first grade of primary school who eventually reach the last grade of primary education. The estimate is based on the reconstructed cohort method. 
"SE.PRM.PRSL.ZS"
# Preprimary education, duration (years) .  Preprimary duration refers to the number of grades (years) in preprimary school. 
"SE.PRE.DURS"
# School enrollment, primary and secondary (gross), gender parity index (GPI) .  Gender parity index for gross enrollment ratio in primary and secondary education is the ratio of girls to boys enrolled at primary and secondary levels in public and private schools. 
"SE.ENR.PRSC.FM.ZS"
# Trained teachers in preprimary education (% of total teachers) .  Trained teachers in preprimary education are the percentage of preprimary school teachers who have received the minimum organized teacher training (pre-service or in-service) required for teaching in a given country. 
"SE.PRE.TCAQ.ZS"
# CPIA social protection rating (1=low to 6=high) .  Social protection and labor assess government policies in social protection and labor market regulations that reduce the risk of becoming poor, assist those who are poor to better manage further risks, and ensure a minimal level of welfare to all people. 
"IQ.CPA.PROT.XQ"
# Adequacy of social protection and labor programs (% of total welfare of beneficiary households) .  Adequacy of social protection and labor programs (SPL) is measured by the total transfer amount received by the population participating in social insurance, social safety net, and unemployment benefits and active labor market programs as a share of their total welfare. Welfare is defined as the total income or total expenditure of beneficiary households. Estimates include both direct and indirect beneficiaries. 
"per_allsp.adq_pop_tot"
# Adequacy of social insurance programs (% of total welfare of beneficiary households) .  Adequacy of social insurance programs is measured by the total transfer amount received by the population participating in social insurance programs as a share of their total welfare. Welfare is defined as the total income or total expenditure of beneficiary households. Social insurance programs include old age contributory pensions (including survivors and disability) and social security and health insurance benefits (including occupational injury benefits, paid sick leave, maternity and other social insurance). Estimates include both direct and indirect beneficiaries. 
"per_si_allsi.adq_pop_tot"
# Coverage of social insurance programs (% of population) .  Coverage of social insurance programs shows the percentage of population participating in programs that provide old age contributory pensions (including survivors and disability) and social security and health insurance benefits (including occupational injury benefits, paid sick leave, maternity and other social insurance). Estimates include both direct and indirect beneficiaries. 
"per_si_allsi.cov_pop_tot"
# Coverage in extreme poor (<$1.25 a day) (%) - All Social Insurance  .  NULL 
"per_si_allsi.cov_ep_tot"
# CPIA financial sector rating (1=low to 6=high) .  Financial sector assesses the structure of the financial sector and the policies and regulations that affect it. 
"IQ.CPA.FINS.XQ"
# Pupil-teacher ratio, preprimary .  Preprimary school pupil-teacher ratio is the average number of pupils per teacher in preprimary school. 
"SE.PRE.ENRL.TC.ZS"
# Pupil-teacher ratio, primary .  Primary school pupil-teacher ratio is the average number of pupils per teacher in primary school. 
"SE.PRM.ENRL.TC.ZS"

Services_Indicators <- c("SE.ENR.PRSC.FM.ZS", "SH.MED.BEDS.ZS", "IT.MLT.MAIN.P2", "IT.NET.BBND.P2",
                         "SE.PRM.NENR", "SE.SEC.PROG.ZS", "SE.PRM.PRSL.ZS", "SE.PRE.DURS", 
                         "SE.ENR.PRSC.FM.ZS", "SE.PRE.TCAQ.ZS", "IQ.CPA.PROT.XQ", "per_allsp.adq_pop_tot",
                         "per_si_allsi.adq_pop_tot", "per_si_allsi.cov_pop_tot", "per_si_allsi.cov_ep_tot",
                         "IQ.CPA.FINS.XQ", "SE.PRE.ENRL.TC.ZS", "SE.PRM.ENRL.TC.ZS")
IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Services_Indicators),
                                    CLUM="Services"),IndicatorsDownloaded)
Services_Data <- WB_DataPull_Function(Services_Indicators, 2004, 2007, 2011)
remove(Services_Indicators)
# Reverse the orders for High is BAD
Services_Data$SE.PRE.ENRL.TC.ZS <- 0-Services_Data$SE.PRE.ENRL.TC.ZS+
  max(Services_Data$SE.PRE.ENRL.TC.ZS, na.rm = TRUE)
Services_Data$SE.PRM.ENRL.TC.ZS <- 0-Services_Data$SE.PRM.ENRL.TC.ZS+
  max(Services_Data$SE.PRM.ENRL.TC.ZS, na.rm = TRUE)

######Personal Transportation Section
# Urban Road Density (KMs Per 1000 Population) .  Urban roads density is measured in KMs of Urban roads in the area (State, District) divided by population in thousands in that area (State, District). Urban roads are roads within a limits of a Municipality, Military Cantonment, Port o a Railway Authority. 
"IN.TRANSPORT.URBNRD.DENSIT"
# Rural Road Density (KMs/1000 Population) .  Rural roads density is measured in KMs of rural roads in the area (State, District) divided by population in thousands in that area (State, District). Rural roads are roads within a district for which the specifications are lower than for district roads.  
"IN.TRANSPORT.RURLRD.DENSIT"
# Access to an all-season road (% of rural population) .  Access to an all-season road is measured as the proportion of rural people who live within 2 kilometers (typically equivalent to a 20-minute walk) of an all-season road. An all-season road is a road that is motorable all year by the prevailing means of rural transport (often a pick-up or a truck which does not have four-wheel-drive). Predictable interruptions of short duration during inclement weather (e.g. heavy rainfall) are acceptable, particularly on low volume roads. The preferred approach to measuring this indicator is by analysis of household surveys that include appropriate questions about access to transport. 
"IS.ROD.ALLS.ZS"
# Railways, passengers carried (million passenger-km) .  Passengers carried by railway are the number of passengers transported by rail times kilometers traveled. 
"IS.RRS.PASG.KM"
# Railways, passenger-km (per PPP $ million of GDP) .   
"IS.RRS.PASG.K2.PP.ZS"
# Roads, passengers carried (million passenger-km) .  Passengers carried by road are the number of passengers transported by road times kilometers traveled. 
"IS.ROD.PSGR.K6"
# Roads, paved (% of total roads) .  Paved roads are those surfaced with crushed stone (macadam) and hydrocarbon binder or bituminized agents, with concrete, or with cobblestones, as a percentage of all the country's roads, measured in length. 
"IS.ROD.PAVE.ZS"
# Air transport, passengers carried .  Air passengers carried include both domestic and international aircraft passengers of air carriers registered in the country. 
"IS.AIR.PSGR"
# Mortality caused by road traffic injury (per 100,000 people) .  Mortality caused by road traffic injury is estimated road traffic fatal injury deaths per 100,000 population. 
"SH.STA.TRAF.P5"

Transport_Indicators <- c("IN.TRANSPORT.URBNRD.DENSIT","IN.TRANSPORT.RURLRD.DENSIT","IS.ROD.ALLS.ZS",
                          "IS.RRS.PASG.KM","IS.RRS.PASG.K2.PP.ZS","IS.ROD.PSGR.K6","IS.ROD.PAVE.ZS",
                          "IS.AIR.PSGR", "SH.STA.TRAF.P5")
IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Transport_Indicators),
                                    CLUM="Personal Transportation"),IndicatorsDownloaded)
Transport_Data <- WB_DataPull_Function(Transport_Indicators, 2004, 2007, 2011)
remove(Transport_Indicators)
# Reverse the orders for High is BAD
##Transport (none, no data in the indicator 6/10/18)
# Transport_Data$SH.STA.TRAF.P5 <- 0-Transport_Data$SH.STA.TRAF.P5+
#   max(Transport_Data$SH.STA.TRAF.P5, na.rm = TRUE)

####Housing Section
##Households With Water On The Premises (%)
"SG.H2O.PRMS.HH.ZS" #High is good#
##Number Of Water Insufficiencies In A Typical Month
"IC.FRM.INFRA.IN6" #High is bad#
##Distribution of households by availability of drinking water sources - within the premises
"IN.POV.HH.DRKNGWATER.WITHIN" #High is goodNot sure what this looks like distribution?
##Average number of hours of power outages.
"IC.ELC.OUTG.HR" #High is bad#
##People Using Safely Managed Sanitation Services (% Of Population)
"SH.STA.SMSS.ZS" #High is good#
##Building Quality Control Index (0-15)
"IC.DCP.BQCI" #High is good#
##Main Cooking Fuel: Electricity (% Of Households)
"SG.COK.ELEC.ZS" #High is good#
##Main Cooking Fuel: LPG/Natural Gas/Biogas (% Of Households)
"SG.COK.LPGN.ZS" #High is good, low is bad#

# Leave out: Percentage of households who do their cooking inside the house # Looks like a gender-health indicator#
"SG.COK.HOUS.ZS" #High is NOT good Only 5 countries in dataset. 

Housing_Indicators <- c("SG.H2O.PRMS.HH.ZS", "IC.FRM.INFRA.IN6", "IN.POV.HH.DRKNGWATER.WITHIN", 
                        "IC.ELC.OUTG.HR", "SH.STA.SMSS.ZS", "IC.DCP.BQCI", "SG.COK.ELEC.ZS",
                        "SG.COK.LPGN.ZS")
IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Housing_Indicators),
                                    CLUM="Housing"),IndicatorsDownloaded)
Housing_Data <- WB_DataPull_Function(Housing_Indicators, 2004, 2007, 2011)
remove(Housing_Indicators)
# Reverse the orders for High is BAD
##Housing (none)


## Goods is now from different data source - in ass_pov_final.csv
##Goods Metrics (Prelminary from Scott) 
# I'm tempted to use just 1 or two measures of material satisfaction and well-being
# One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
# linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
# Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
# Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"
GoodsData <- read.csv("./GFN_Data_Visualization/ScatterVisuals/ass_pov_final.csv")
# Reverse the orders for High is BAD
GoodsData$ass_pov_extr <- 0-GoodsData$ass_pov_extr+max(GoodsData$ass_pov_extr, na.rm = TRUE)

Indicators_Nodownloads <- as.data.frame(names(warnings()))

write.csv(IndicatorsDownloaded,"./GFN_Data_Visualization/ScatterVisuals/IndicatorsDLed.csv")
write.table(Indicators_Nodownloads,"./GFN_Data_Visualization/ScatterVisuals/IndicatorsDLed.csv", append = TRUE)

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
             "Resource rich Sub-Saharan Africa countries", "Southern Cone",
             "Sub-Saharan Africa", "Resource rich Sub-Saharan Africa countries, of which oil exporters", 
             #plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo", "Faeroe Islands",
             #plus Macoa bc is it really worth adding to China considering it requires a separate weighted aggreagation process
             "Macao, China",
             #Plus country GFN has but we don't want
             "World")
# Write to file for reading in country correspondence script
write.csv(WB_drop, file = "./GFN_Data_Visualization/ScatterVisuals/DropTheseCountries.csv", row.names = F)

Food_Data <- Food_Data[!(Food_Data$country %in% WB_drop),]
Government_Data <- Government_Data[!(Government_Data$country %in% WB_drop),]
Services_Data <- Services_Data[!(Services_Data$country %in% WB_drop),]
Transport_Data <- Transport_Data[!(Transport_Data$country %in% WB_drop),]
Housing_Data <- Housing_Data[!(Housing_Data$country %in% WB_drop),]

## Get rid of indicators that have more NAs/NA_factor (eg. 1/2 if NA_factor is 2., .8 if NA_factor is 1.25 etc.
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

# Drop indicators that have more than 1/NA factor proportion of NAs
FoodData_NoNAs <- NARemove_Fun(Food_Data, 2)
GovernmentData_NoNAs <- NARemove_Fun(Government_Data, 2)
ServicesData_NoNAs <- NARemove_Fun(Services_Data, 2)
TransportData_NoNAs <- NARemove_Fun(Transport_Data, 1.25)
HousingData_NoNAs <- NARemove_Fun(Housing_Data, 1.25)

#Create a min-max range version of all remaining data to normalize btw 0 and 1, then aggregate with Averaging
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
TransportData_MaxMin <- MaxMin_Fun(TransportData_NoNAs, "Personal Transport")

#Single column version
######  Transport if there is only 1 column (eg. if the NA_Factor for Transport is 2)
# colnames(TransportData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
# TransportData_NoNAs$MaxMin_Index <- TransportData_NoNAs$MaxMin_Index/max(TransportData_NoNAs$MaxMin_Index, na.rm=TRUE)
# TransportData_NoNAs$CLUM_category <- "Personal Transport"
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
TransportData_ZScore <- ZScore_Fun(TransportData_NoNAs, "Personal Transport")


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

print ('Looks good, Run 2.Country Correspondence to make sure all countries and groupings were dealt with
       and do the GTAP weighted aggregation')