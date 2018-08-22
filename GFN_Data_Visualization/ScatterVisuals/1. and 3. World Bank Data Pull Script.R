library(data.table)
# Updated Version of WDI package Installed is required to allow underscores in Indicator names
library(WDI)
library(dplyr)
library(httr)
library(jsonlite)
library(boxr)

# Run to activate connection for the SDG data file on Box
box_auth()

"Set working directory to top level directory in console"
##eg. setwd("C:\\Users\\Eli\\GitFolders\\EFCLUM")
"Set to use WB (WB_yes_or_no<-1) or SDG (WB_yes_or_no<-0) Indicators"
WB_yes_or_no <- 0

if(WB_yes_or_no==1) {
  WB_SDG <- "WB"
} else if (WB_yes_or_no == 0) {
  WB_SDG <- "SDG"
} else {
  stop(
    print(
      "WB_yes_or_no must be set either to 1 (for WB Indicators) or 0 (for SDG Indicators)"
    )
  )
}

years <- c(2004, 2007, 2011)


#Array of all World Bank Indicator data
if(WB_SDG =="WB"){
WBIndicators <- WDIcache()
IndicatorList <- as.data.frame(WBIndicators[[1]], stringAsFactors=FALSE)
WBCountries <- as.data.frame(WBIndicators[[2]])

# look at a table of indicators based on a search for the term in the "NAME"
# to look in the description change column vector index from 2 to 3
"food" -> word ;SearchWB <- IndicatorList[grep(word,IndicatorList[,2]),] 
#View(SearchWB)

######Data Pull Function
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all",
                   indicator = indicator_list,
                   start = CLUM_startyear, end = CLUM_endyear, extra = FALSE, cache = NULL)
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

}

if(WB_SDG =="SDG") {
  #Array of UN SDGIndicators
  SDGIndicators <-
   #For now, I have it set up to pull the file from my Box.com cloud storage...
      SDGIndicators <- box_read("303960310729")
    # I encountered an error from the fromJSON function - didn't like the !...so I went to downloads
  # SDG <- GET(url = 'https://unstats.un.org', path='/SDGAPI/v1/sdg/Indicator/Data')
  # SDG.raw.content <- rawToChar(SDG$content)
  # SDG.content <- fromJSON(SDG.raw.content)
  # SDG.df <- do.call(what = "rbind", args = lapply(SDG.content, as.data.frame))
  #
  # SDGlist <- GET(url = 'https://unstats.un.org', path='/v1/sdg/Indicator/List')
  # SDGlist.raw.content <- rawToChar(SDGlist$content)
  # SDGlist.content <- fromJSON(SDGlist.raw.content)
}

####List for dropping WB countries not used in correspondence before forming indicators
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
             "Holy See", "United States Virgin Islands","Micronesia (Federated States of)",
             "Falkland Islands (Malvinas)",
             #British 'dependencies/juristictions', semi-independent islands in the channel, near Normandy, France
             "Jersey", "Guernsey",
             #Carribean territories of the Netherlands
             "Bonaire, Sint Eustatius and Saba",
             #plus countries that GFN does not have
             "Monaco", "West Bank and Gaza", "San Marino", "Kosovo", "Faeroe Islands",
             #plus Macoa bc is it really worth adding to China considering it requires a separate weighted aggreagation process
             "Macao, China", "China, Macao Special Administrative Region",
             #Plus country GFN has but we don't want
             "World")
# Write to file for reading in country correspondence script
write.csv(WB_drop, file = "./GFN_Data_Visualization/ScatterVisuals/DropTheseCountries.csv", row.names = F)

if(WB_SDG=="WB"){
  
  ######Food Section
  #Cereal Yield (Kg Per Hectare)
  Food_Indicators <- c(
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
  Food_Indicators_reverse <- c(
    "SN.ITK.DFCT", #High is BAD
    #Malnourished Children (Underweight, -2SD) (% Of Children Under 5): Q1 (Lowest)
    "SH.STA.MALN.ZS", #High is BAD
    # Fish Species, Threatened. number of species classified by the IUCN as endangered, vulnerable, rare, indeterminate, out of danger, or insufficiently known
    "EN.FSH.THRD.NO" #High is BAD  #Only 2017 data 6/10/18
  )
  Food_Indicators <- c(Food_Indicators,Food_Indicators_reverse)
  
  IndicatorsDownloaded <- subset(IndicatorList,IndicatorList$indicator %in% Food_Indicators)
  IndicatorsDownloaded$CLUM <- "Food"
  
  #Actually better to separate just for the minmax and z-score
  
  #for (i in years){nam <- paste("Food_Data", i, sep = "_"); assign(nam,
  #                                                                 WB_DataPull_Function(Food_Indicators, i))
  #Drop the countries from the WB_Drop list
  #assign(print(paste("Food_Data", i, sep="_")),
  #       print(get(paste("Food_Data",i, sep="_")))[!(print(get(paste("Food_Data",
  #                                                       i, sep="_")))$country %in% WB_drop),])
  #}
  
  Food_Data <- WB_DataPull_Function(Food_Indicators, 2004, 2007, 2011)
  Food_Data <- Food_Data[!(Food_Data$country %in% WB_drop),]
  Food_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in Food_Indicators_reverse){
    # The if removes the warning from the max function for when vectors are all NA
    if(!all(is.na(Food_Data[i]))){
      Food_Data[i] <- 0 - Food_Data[i] + max(Food_Data[i], na.rm = TRUE)
    }
  }
  # Split into the 3 years
  for (i in years){
    nam <- paste(deparse(substitute(Food_Data)), i, sep = "_") 
    assign(nam, Food_Data[Food_Data$year==i,])
  }
  
  remove(Food_Indicators, Food_Indicators_reverse, Food_Data)
  
  ######Government Section
  Government_Indicators <- c(
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
  Government_Indicators_reverse <- c(
    #High is BAD
    "GC.DOD.TOTL.GD.ZS", #High is BAD
    #Unemployment, Total (% Of Total Labor Force) (National Estimate)
    "SL.UEM.TOTL.NE.ZS", #High is BAD
    #Net Official Development Assistance Received (Constant 2014 US$)
    "DT.ODA.ODAT.KD", #High is BAD
    #Inflation, Consumer Prices (Annual %)
    "FP.CPI.TOTL.ZG" #High is BAD
  )  
  
  Government_Indicators <- c(Government_Indicators, Government_Indicators_reverse)
  
  IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Government_Indicators),
                                      CLUM="Government"),IndicatorsDownloaded)
  Government_Data <- WB_DataPull_Function(Government_Indicators, 2004, 2007, 2011)
  Government_Data <- Government_Data[!(Government_Data$country %in% WB_drop),]
  Government_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in Government_Indicators_reverse){Government_Data[i] <- 0 - Government_Data[i] + max(Government_Data[i], na.rm = TRUE)
  }
  
  for (i in years){
    nam <- paste(deparse(substitute(Government_Data)), i, sep = "_") 
    assign(nam, Government_Data[Government_Data$year==i,])
  }
  
  remove(Government_Indicators, Government_Indicators_reverse, Government_Data)
  
  ##Services Metrics
  Services_Indicators <- c(
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
  Services_Indicators_reverse <- c(
    #High is BAD
    # Pupil-teacher ratio, preprimary .  Preprimary school pupil-teacher ratio is the average number of pupils per teacher in preprimary school. 
    "SE.PRE.ENRL.TC.ZS", #High is BAD
    # Pupil-teacher ratio, primary .  Primary school pupil-teacher ratio is the average number of pupils per teacher in primary school. 
    "SE.PRM.ENRL.TC.ZS" #High is BAD
  )
  
  Services_Indicators <- c(Services_Indicators, Services_Indicators_reverse)
  
  IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Services_Indicators),
                                      CLUM="Services"),IndicatorsDownloaded)
  Services_Data <- WB_DataPull_Function(Services_Indicators, 2004, 2007, 2011)
  Services_Data <- Services_Data[!(Services_Data$country %in% WB_drop),]
  Services_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in Services_Indicators_reverse){Services_Data[i] <- 0 - Services_Data[i] + max(Services_Data[i], na.rm = TRUE)
  }
  
  for (i in years){
    nam <- paste(deparse(substitute(Services_Data)), i, sep = "_") 
    assign(nam, Services_Data[Services_Data$year==i,])
  }
  
  remove(Services_Indicators, Services_Indicators_reverse, Services_Data)
  
  ######Personal Transportation Section
  Transport_Indicators <- c(
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
    # Air transport, passengers carried .  Air passengers carried include both domestic and international aircraft passengers of air carriers registered in the country. 
    "IS.AIR.PSGR",
    # Mortality caused by road traffic injury (per 100,000 people) .  Mortality caused by road traffic injury is estimated road traffic fatal injury deaths per 100,000 population. 
    "SH.STA.TRAF.P5"
  )
  
  # Transport_Indicators_reverse <- c(
  #)
  # Transport_Indicators <- c(Transport_Indicators,Transport_Indicators_reverse)
  
  IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Transport_Indicators),
                                      CLUM="Personal Transportation"),IndicatorsDownloaded)
  Transport_Data <- WB_DataPull_Function(Transport_Indicators, 2004, 2007, 2011)
  
  Transport_Data <- Transport_Data[!(Transport_Data$country %in% WB_drop),]
  Transport_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  #for (i in Government_Indicators_reverse){Government_Data[i] <- 0 - Government_Data[i] + max(Government_Data[i], na.rm = TRUE)
  #}
  
  for (i in years){
    nam <- paste(deparse(substitute(Transport_Data)), i, sep = "_") 
    assign(nam, Transport_Data[Transport_Data$year==i,])
  }
  
  remove(Transport_Indicators, Transport_Data)
  #,Transport_Indicators_reverse
  
  
  ####Housing Section
  Housing_Indicators <- c(
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
  # Leave out: Percentage of households who do their cooking inside the house # Looks like a gender-health indicator#
  "SG.COK.HOUS.ZS" #High is NOT good Only 5 countries in dataset. 
  
  IndicatorsDownloaded <- rbind(cbind(subset(IndicatorList,IndicatorList$indicator %in% Housing_Indicators),
                                      CLUM="Housing"),IndicatorsDownloaded)
  Housing_Data <- WB_DataPull_Function(Housing_Indicators, 2004, 2007, 2011)
  #Housing_Data$country <- trimws(Housing_Data$country, which = c("both", "left", "right"))
  Housing_Data <- Housing_Data[!(Housing_Data$country %in% as.character(WB_drop)),]
  Housing_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  ##Housing (none)
  
  for (i in years){
    nam <- paste(deparse(substitute(Housing_Data)), i, sep = "_") 
    assign(nam, Housing_Data[Housing_Data$year==i,])
  }
  
  remove(Housing_Indicators, Housing_Data)
  
  ## Goods is now from different data source - in ass_pov_final.csv
  ##Goods Metrics (Prelminary from Scott) 
  # I'm tempted to use just 1 or two measures of material satisfaction and well-being
  # One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
  # linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
  # Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
  # Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"
  Goods_Data <- read.csv("./GFN_Data_Visualization/ScatterVisuals/ass_pov_final.csv")
  # Reverse the orders for High is BAD
  Goods_Data$ass_pov_extr <- 0-Goods_Data$ass_pov_extr+max(Goods_Data$ass_pov_extr, na.rm = TRUE)
  for (i in years){
    nam <- paste(deparse(substitute(Goods_Data)), i, sep = "_") 
    assign(nam, Goods_Data[Goods_Data$year ==i,])
  }
  
  #Add a row for Goods Data, including that it's needed for the plot function
  GoodsDL <- cbind.data.frame(
    "No.Code",
    "Percentage of population experiencing 'asset poverty'",
    "People not owning a radio, TV, telephone, bicycle, motorbike, or
    refrigerator, and does not own a car or truck.",
    "Oxford Poverty & Human Development Initiative",
    "Oxford University",
    "Goods"
  )
  colnames(GoodsDL) <- colnames(IndicatorsDownloaded)
  IndicatorsDownloaded <- rbind(IndicatorsDownloaded,GoodsDL)
  
  Indicators_Nodownloads <- as.data.frame(names(warnings()))
  
  #Output WB Indicators info, and list of what was not downloaded at all at the end
  write.csv(IndicatorsDownloaded,"./GFN_Data_Visualization/ScatterVisuals/IndicatorsDLed.csv")
  cat("\n Not Downloaded (and other warnings) \n", file = "./GFN_Data_Visualization/ScatterVisuals/IndicatorsDLed.csv", append = TRUE) 
  write.table(Indicators_Nodownloads,"./GFN_Data_Visualization/ScatterVisuals/IndicatorsDLed.csv",
              append = TRUE, col.names=FALSE)
  
}

if (WB_SDG == "SDG") {
  Food_Indicators <- c()
  SDGIndicatorsDownloaded <- data.frame()
  #High is BAD#
  Food_Indicators_reverse <- c(
    # Prevalence of undernourishment (%)
    "SN_ITK_DEFC",
    #Proportion of children moderately or severely stunted (%)
    "SH_STA_STUNT",
    #Proportion of children moderately or severely overweight (%)
    "SH_STA_OVRWGT"
    )
  Food_Indicators <- c(Food_Indicators, Food_Indicators_reverse)
  
  Food_Data <-
    subset(
      SDGIndicators,
      SDGIndicators$SeriesCode %in% Food_Indicators &
        SDGIndicators$TimePeriod %in% years
    )
  Food_Data_cols <-
    c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
  Food_Data <- Food_Data[Food_Data_cols]
  Food_Data <- Food_Data[!(Food_Data$GeoAreaName %in% WB_drop),]
  Food_Data <-
    dcast(Food_Data, GeoAreaName + TimePeriod ~ SeriesCode, value.var = 'Value')
  # Reverse the orders for High is BAD
  for (i in Food_Indicators_reverse) {
    # The if removes the warning from the max function for when vectors are all NA
    if (!all(is.na(Food_Data[[i]]))) {
      Food_Data[i] <-
        0 - readr::parse_number(Food_Data[[i]]) + readr::parse_number(max(Food_Data[[i]], na.rm = TRUE))
    }
  }
  
  # Split into the 3 years
  for (i in years) {
    nam <- paste(deparse(substitute(Food_Data)), i, sep = "_")
    assign(nam, Food_Data[Food_Data$TimePeriod == i,])
  }
  
  Food_IndicatorsList <- cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Food_Indicators]),
                               unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Food_Indicators]),
                               "Food")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Food_IndicatorsList)
  remove(Food_Indicators, Food_Indicators_reverse, Food_Data_cols)
  
  Govt_Indicators1 <- c(
    #  Proportion of population covered by labour market programs (%)
    "SI_COV_LMKT",
    # Countries with procedures in law or policy for participation by service users/communities in planning program in rural drinking-water supply, by level of definition in procedures (10 = Clearly defined; 5 = Not clearly defined ; 0 = NA)
#    "ER_H2O_PRDU", not in the data for those years
    # Countries that have conducted at least one population and housing census in the last 10 years (1 = YES; 0 = NO)
    "SG_REG_CENSUSN")
  
  #High is BAD#
  Govt_Indicators_reverse <- c(
    # Number of victims of intentional homicide per 100,000 population (victims per 100,000 population)
    "VC_IHR_PSRC",
    # Unsentenced detainees as a proportion of overall prison population (%)
#    "VC_PRS_UNSEC",  Not in the data for those years
    # Bribery incidence (% of firms experiencing at least one bribe payment request)
    "IC_FRM_BRIB"
    )

  
    Govt_Indicators <- c(Govt_Indicators1, Govt_Indicators_reverse)
  
  Government_Data <-
    subset(
      SDGIndicators,
      SDGIndicators$SeriesCode %in% Govt_Indicators &
        SDGIndicators$TimePeriod %in% years
    )
  Govt_Data_cols <-
    c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
  Government_Data <- Government_Data[Govt_Data_cols]
  Government_Data <- Government_Data[!(Government_Data$GeoAreaName %in% WB_drop),]
  Government_Data <-
    dcast(Government_Data, GeoAreaName + TimePeriod ~ SeriesCode, value.var = 'Value')
  # Reverse the orders for High is BAD
  for (i in Govt_Indicators_reverse) {
    # The if removes the warning from the max function for when vectors are all NA
    if (!all(is.na(Government_Data[[i]]))) {
      Government_Data[i] <-
        0 - readr::parse_number(Government_Data[[i]]) + readr::parse_number(max(Government_Data[[i]], na.rm = TRUE))
    }
  }
  for (i in Govt_Indicators1) {
    Government_Data[i] <- readr::parse_number(Government_Data[[i]])
  }
  
    # Split into the 3 years
  for (i in years) {
    nam <- paste(deparse(substitute(Government_Data)), i, sep = "_")
    assign(nam, Government_Data[Government_Data$TimePeriod == i,])
  }
  
  Government_IndicatorsList <- 
    cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Govt_Indicators]),
          unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Govt_Indicators]),
          "Government")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Government_IndicatorsList)
  remove(Govt_Indicators, Govt_Indicators_reverse, Govt_Data_cols)
  
  Services_Indicators <- c(
    # Health worker density, by type of occupation (per 1,000 population)
    "SH_MED_HEAWOR",
    #  Minimum proficiency in mathematics, by education level and sex (%)
    "SE_MAT_PROF",
    # Schools with access toÂ computers for pedagogical purposes, by education level (%)
    "SE_ACC_COMP",
    # Proportion of teachers who have received at least the minimum organized teacher training (e.g. pedagogical training) pre-service or in-service required for teaching at the relevant level in a given country, by education level (%)
    "SE_TRA_GRDL",
    # Proportion of population practicing open defecation, by urban/rural (%)
    "SH_SAN_DEFECT",
    # Proportion of population with access to electricity, by urban/rural (%)
    "EG_ELC_ACCS",
    # Proportion of population covered by a mobile network, by technology (%)
    "IT_MOB_NTWK",
    # Municipal Solid Waste collection coverage, by cities (%)
    "EN_REF_WASCOL",
    # Number of fixed Internet broadband subscriptions, by speed (number)
    "IT_NET_BBN"
  )
  
  #High is BAD#
  Services_Indicators_reverse <- c()
  Services_Indicators <-
    c(Services_Indicators, Services_Indicators_reverse)
  
  Services_Data <-
    subset(
      SDGIndicators,
      SDGIndicators$SeriesCode %in% Services_Indicators &
        SDGIndicators$TimePeriod %in% years
    )
  Services_Data_cols <-
    c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
  Services_Data <- Services_Data[Services_Data_cols]
  Services_Data <-
    Services_Data[!(Services_Data$GeoAreaName %in% WB_drop),]
  Services_Data <-
    dcast(Services_Data,
          GeoAreaName + TimePeriod ~ SeriesCode,
          value.var = 'Value')
  # Reverse the orders for High is BAD
  for (i in Services_Indicators_reverse) {
    # The if removes the warning from the max function for when vectors are all NA
    if (!all(is.na(Services_Data[[i]]))) {
      Services_Data[i] <-
        0 - readr::parse_number(Services_Data[[i]]) + readr::parse_number(max(Services_Data[[i]], na.rm = TRUE))
    }
  }
  
  # Split into the 3 years
  for (i in years) {
    nam <- paste(deparse(substitute(Services_Data)), i, sep = "_")
    assign(nam, Services_Data[Services_Data$TimePeriod == i,])
  }
  
  Services_IndicatorsList <- 
    cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Services_Indicators]),
          unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Services_Indicators]),
          "Services")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Services_IndicatorsList)
  remove(Services_Indicators,
         Services_Indicators_reverse,
         Services_Data_cols)
  
  
  Goods_Indicators <- c(
    # Proportion of population using safely managed drinking water services, by urban/rural (%)
    "SH_H2O_SAFE",
    # Annual growth rate of real GDP per capita (%)
    "NY_GDP_PCAP",
    # Domestic material consumption per capita, by type of raw material (tonnes)
    "EN_MAT_DOMCMPC",
    # Internet users per 100 inhabitants
    "IT_USE_ii99")
  
  #High is BAD#
  Goods_Indicators_reverse <- c()
  Goods_Indicators <- c(Goods_Indicators, Goods_Indicators_reverse)
  
  Goods_Data <-
    subset(
      SDGIndicators,
      SDGIndicators$SeriesCode %in% Goods_Indicators &
        SDGIndicators$TimePeriod %in% years
    )
  Goods_Data_cols <-
    c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
  Goods_Data <- Goods_Data[Goods_Data_cols]
  Goods_Data <- Goods_Data[!(Goods_Data$GeoAreaName %in% WB_drop),]
  Goods_Data <-
    dcast(Goods_Data, GeoAreaName + TimePeriod ~ SeriesCode, value.var = 'Value')
  # Reverse the orders for High is BAD
  for (i in Goods_Indicators_reverse) {
    # The if removes the warning from the max function for when vectors are all NA
    if (!all(is.na(Goods_Data[[i]]))) {
      Goods_Data[i] <-
        0 - readr::parse_number(Goods_Data[[i]]) + readr::parse_number(max(Goods_Data[[i]], na.rm = TRUE))
    }
  }
  
  # Split into the 3 years
  for (i in years) {
    nam <- paste(deparse(substitute(Goods_Data)), i, sep = "_")
    assign(nam, Goods_Data[Goods_Data$TimePeriod == i,])
  }
  
  Goods_IndicatorsList <- 
    cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Goods_Indicators]),
    unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Goods_Indicators]),
    "Goods")
  SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Goods_IndicatorsList)
  remove(Goods_Indicators,
         Goods_Indicators_reverse,
         Goods_Data_cols)
  
  
  Housing_Indicators <- c()
  
  #High is BAD#
  Housing_Indicators_reverse <- c(
  # Proportion of urban population living in slums (%)
  "EN_LND_SLUM"
    )
  Housing_Indicators <-
    c(Housing_Indicators, Housing_Indicators_reverse)
  
  Housing_Data <-
    subset(SDGIndicators,
           SDGIndicators$SeriesCode %in% Housing_Indicators
           #For Housing, only 1 indicator, and only 2 years.
           #&
           #                         SDGIndicators$TimePeriod %in% years
           )
   Housing_Data_cols <-
     c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
   Housing_Data <- Housing_Data[Housing_Data_cols]
   Housing_Data <-
     Housing_Data[!(Housing_Data$GeoAreaName %in% WB_drop),]
   Housing_Data <-
     dcast(Housing_Data,
           GeoAreaName + TimePeriod ~ SeriesCode,
           value.var = 'Value')
   # Reverse the orders for High is BAD
   for (i in Housing_Indicators_reverse) {
     # The if removes the warning from the max function for when vectors are all NA
     if (!all(is.na(Housing_Data[[i]]))) {
       Housing_Data[i] <-
         0 - readr::parse_number(Housing_Data[[i]]) + readr::parse_number(max(Housing_Data[[i]], na.rm = TRUE))
     }
   }
   
   for (i in 1:nrow(Housing_Data)) {
     if (Housing_Data$TimePeriod[i] == 2005) {
       Housing_Data$TimePeriod[i] <- 2007
     }
     else if (Housing_Data$TimePeriod[i] == 2010) {
       Housing_Data$TimePeriod[i] <- 2011
     }
     else{
       stop(print("Housing has OTHER years than 2005 and 2010, fix it"))
     }
   }
   # Split into the 2 years
   for (i in years) {
     nam <- paste(deparse(substitute(Housing_Data)), i, sep = "_")
     assign(nam, Housing_Data[Housing_Data$TimePeriod == i,])
   }
   
   Housing_IndicatorsList <- 
     cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Housing_Indicators]),
           unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Housing_Indicators]),
           "Housing")
   SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Housing_IndicatorsList)
   remove(Housing_Indicators,
          Housing_Indicators_reverse,
          Housing_Data_cols)
   
   Transport_Indicators <- c()
   
   #High is BAD#
   Transport_Indicators_reverse <- c(
     # Death rate due to road traffic injuries (per 100,000 population)
     "SH_STA_TRAF")
   Transport_Indicators <-
     c(Transport_Indicators, Transport_Indicators_reverse)
   
   Transport_Data <-
     subset(
       SDGIndicators,
       SDGIndicators$SeriesCode %in% Transport_Indicators &
         SDGIndicators$TimePeriod %in% years
     )
   Transport_Data_cols <-
     c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
   Transport_Data <- Transport_Data[Transport_Data_cols]
   Transport_Data <-
     Transport_Data[!(Transport_Data$GeoAreaName %in% WB_drop),]
   Transport_Data <-
     dcast(Transport_Data,
           GeoAreaName + TimePeriod ~ SeriesCode,
           value.var = 'Value')
   # Reverse the orders for High is BAD
   for (i in Transport_Indicators_reverse) {
     # The if removes the warning from the max function for when vectors are all NA
     if (!all(is.na(Transport_Data[[i]]))) {
       Transport_Data[i] <-
         0 - readr::parse_number(Transport_Data[[i]]) + readr::parse_number(max(Transport_Data[[i]], na.rm = TRUE))
     }
   }
   
   # Split into the 3 years
   for (i in years) {
     nam <- paste(deparse(substitute(Transport_Data)), i, sep = "_")
     assign(nam, Transport_Data[Transport_Data$TimePeriod == i,])
   }
   
   Transport_IndicatorsList <- 
     cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%Transport_Indicators]),
           unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%Transport_Indicators]),
           "Personal Transportation")
   SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,Transport_IndicatorsList)
   remove(Transport_Indicators,
          Transport_Indicators_reverse,
          Transport_Data_cols)
   
   GFCF_Indicators <- c(
     # Number of automated teller machines (ATMs) per 100,000 adults
     "FB_ATM_TOTL",
     # Research and development expenditure as a proportion of GDP (%)
     "GB_XPD_RSDV"
     )
   
   #High is BAD#
   GFCF_Indicators_reverse <- c(
     # Direct agriculture loss attributed to disasters, by hazard type (millions of current United States dollars)
     "VC_DSR_AGLH")
   GFCF_Indicators <-
     c(GFCF_Indicators, GFCF_Indicators_reverse)
   
   GFCF_Data <-
     subset(
       SDGIndicators,
       SDGIndicators$SeriesCode %in% GFCF_Indicators &
         SDGIndicators$TimePeriod %in% years
     )
   GFCF_Data_cols <-
     c("SeriesCode", "GeoAreaName", "TimePeriod", "Value")
   GFCF_Data <- GFCF_Data[GFCF_Data_cols]
   GFCF_Data <-
     GFCF_Data[!(GFCF_Data$GeoAreaName %in% WB_drop),]
   GFCF_Data <-
     dcast(GFCF_Data, GeoAreaName + TimePeriod ~ SeriesCode, value.var = 'Value')
   # Reverse the orders for High is BAD
   for (i in GFCF_Indicators_reverse) {
     # The if removes the warning from the max function for when vectors are all NA
     if (!all(is.na(GFCF_Data[[i]]))) {
       GFCF_Data[i] <-
         0 - readr::parse_number(GFCF_Data[[i]]) + readr::parse_number(max(GFCF_Data[[i]], na.rm = TRUE))
     }
   }
   
   # Split into the 3 years
   for (i in years) {
     nam <- paste(deparse(substitute(GFCF_Data)), i, sep = "_")
     assign(nam, GFCF_Data[GFCF_Data$TimePeriod == i,])
   }
   
   GFCF_IndicatorsList <- 
     cbind(unique(SDGIndicators$SeriesCode[SDGIndicators$SeriesCode%in%GFCF_Indicators]),
           unique(SDGIndicators$SeriesDescription[SDGIndicators$SeriesCode%in%GFCF_Indicators]),
           "Gross Fixed Capital Formation")
   SDGIndicatorsDownloaded <- rbind (SDGIndicatorsDownloaded,GFCF_IndicatorsList)
   remove(GFCF_Indicators, GFCF_Indicators_reverse, GFCF_Data_cols)
   
   colnames(SDGIndicatorsDownloaded) <- c("indicator", "description", "CLUM")
           
}

## Get rid of indicators that have more NAs/NA_factor (eg. 1/2 if NA_factor is 2., .8 if NA_factor is 1.25 etc.
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


# Drop indicators that have more than 1/NA factor proportion of NAs
FoodData_NoNAs_2004 <- NARemove_Fun(Food_Data_2004, 2) ; remove(Food_Data_2004)
FoodData_NoNAs_2007 <- NARemove_Fun(Food_Data_2007, 2); remove(Food_Data_2007)
FoodData_NoNAs_2011 <- NARemove_Fun(Food_Data_2011, 2); remove(Food_Data_2011)
GovernmentData_NoNAs_2004 <- NARemove_Fun(Government_Data_2004, 2); remove(Government_Data_2004)
GovernmentData_NoNAs_2007 <- NARemove_Fun(Government_Data_2007, 2); remove(Government_Data_2007)
GovernmentData_NoNAs_2011 <- NARemove_Fun(Government_Data_2011, 2); remove(Government_Data_2011)
ServicesData_NoNAs_2004 <- NARemove_Fun(Services_Data_2004, 2); remove(Services_Data_2004)
ServicesData_NoNAs_2007 <- NARemove_Fun(Services_Data_2007, 2); remove(Services_Data_2007)
ServicesData_NoNAs_2011 <- NARemove_Fun(Services_Data_2011, 2); remove(Services_Data_2011)
TransportData_NoNAs_2004 <- NARemove_Fun(Transport_Data_2004, 1.25); remove(Transport_Data_2004)
TransportData_NoNAs_2007 <- NARemove_Fun(Transport_Data_2007, 1.25); remove(Transport_Data_2007)
TransportData_NoNAs_2011 <- NARemove_Fun(Transport_Data_2011, 1.25); remove(Transport_Data_2011)
HousingData_NoNAs_2004 <- NARemove_Fun(Housing_Data_2004, 1.25); remove(Housing_Data_2004)
HousingData_NoNAs_2007 <- NARemove_Fun(Housing_Data_2007, 1.25); remove(Housing_Data_2007)
HousingData_NoNAs_2011 <- NARemove_Fun(Housing_Data_2011, 1.25); remove(Housing_Data_2011)
Goods_Data_NoNAs_2004 <- NARemove_Fun(Goods_Data_2004, 2); remove(Goods_Data_2004)
Goods_Data_NoNAs_2007 <- NARemove_Fun(Goods_Data_2007, 2); remove(Goods_Data_2007)
Goods_Data_NoNAs_2011 <- NARemove_Fun(Goods_Data_2011, 2); remove(Goods_Data_2011)

if (WB_SDG == "SDG") {
  GFCF_Data_NoNAs_2004 <- NARemove_Fun(GFCF_Data_2004, 2); remove(GFCF_Data_2004)
  GFCF_Data_NoNAs_2007 <- NARemove_Fun(GFCF_Data_2007, 2); remove(GFCF_Data_2007)
  GFCF_Data_NoNAs_2011 <- NARemove_Fun(GFCF_Data_2011, 2); remove(GFCF_Data_2011)
}

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

FoodData_MaxMin_2004 <- MaxMin_Fun(FoodData_NoNAs_2004, "Food")
FoodData_MaxMin_2007 <- MaxMin_Fun(FoodData_NoNAs_2007, "Food")
FoodData_MaxMin_2011 <- MaxMin_Fun(FoodData_NoNAs_2011, "Food")
FoodData_MaxMin <- rbind(FoodData_MaxMin_2004,FoodData_MaxMin_2007,FoodData_MaxMin_2011)
remove(FoodData_MaxMin_2004,FoodData_MaxMin_2007,FoodData_MaxMin_2011)
GovernmentData_MaxMin_2004 <- MaxMin_Fun(GovernmentData_NoNAs_2004, "Government")
GovernmentData_MaxMin_2007 <- MaxMin_Fun(GovernmentData_NoNAs_2007, "Government")
GovernmentData_MaxMin_2011 <- MaxMin_Fun(GovernmentData_NoNAs_2011, "Government")
GovernmentData_MaxMin <- rbind(GovernmentData_MaxMin_2004,GovernmentData_MaxMin_2007,GovernmentData_MaxMin_2011)
remove(GovernmentData_MaxMin_2004,GovernmentData_MaxMin_2007,GovernmentData_MaxMin_2011)
ServicesData_MaxMin_2004 <- MaxMin_Fun(ServicesData_NoNAs_2004, "Services")
ServicesData_MaxMin_2007 <- MaxMin_Fun(ServicesData_NoNAs_2007, "Services")
ServicesData_MaxMin_2011 <- MaxMin_Fun(ServicesData_NoNAs_2011, "Services")
ServicesData_MaxMin <- rbind(ServicesData_MaxMin_2004,ServicesData_MaxMin_2007,ServicesData_MaxMin_2011)
remove(ServicesData_MaxMin_2004,ServicesData_MaxMin_2007,ServicesData_MaxMin_2011)
TransportData_MaxMin_2004 <- MaxMin_Fun(TransportData_NoNAs_2004, "Personal Transportation")
TransportData_MaxMin_2007 <- MaxMin_Fun(TransportData_NoNAs_2007, "Personal Transportation")
TransportData_MaxMin_2011 <- MaxMin_Fun(TransportData_NoNAs_2011, "Personal Transportation")
TransportData_MaxMin <- rbind(TransportData_MaxMin_2004,TransportData_MaxMin_2007, TransportData_MaxMin_2011)
remove(TransportData_MaxMin_2004,TransportData_MaxMin_2007, TransportData_MaxMin_2011)
if (WB_SDG == "SDG") {
#  HousingData_MaxMin_2004 <- MaxMin_Fun(HousingData_NoNAs_2004, "Housing")
  HousingData_MaxMin_2007 <- MaxMin_Fun(HousingData_NoNAs_2007, "Housing")
  HousingData_MaxMin_2011 <- MaxMin_Fun(HousingData_NoNAs_2011, "Housing")
  HousingData_MaxMin <- rbind(
    #HousingData_MaxMin_2004,
    HousingData_MaxMin_2007, HousingData_MaxMin_2011)
  remove(
#    HousingData_MaxMin_2004,
    HousingData_MaxMin_2007, HousingData_MaxMin_2011)
  Goods_Data_MaxMin_2004 <- MaxMin_Fun(Goods_Data_NoNAs_2004, "Goods")
  Goods_Data_MaxMin_2007 <- MaxMin_Fun(Goods_Data_NoNAs_2007, "Goods")
  Goods_Data_MaxMin_2011 <- MaxMin_Fun(Goods_Data_NoNAs_2011, "Goods")
  Goods_Data_MaxMin <- rbind(Goods_Data_MaxMin_2004,Goods_Data_MaxMin_2007, Goods_Data_MaxMin_2011)
  remove(Goods_Data_MaxMin_2004,Goods_Data_MaxMin_2007, Goods_Data_MaxMin_2011)
  GFCF_Data_MaxMin_2004 <- MaxMin_Fun(GFCF_Data_NoNAs_2004, "Gross Fixed Capital Formation")
  GFCF_Data_MaxMin_2007 <- MaxMin_Fun(GFCF_Data_NoNAs_2007, "Gross Fixed Capital Formation")
  GFCF_Data_MaxMin_2011 <- MaxMin_Fun(GFCF_Data_NoNAs_2011, "Gross Fixed Capital Formation")
  GFCF_Data_MaxMin <- rbind(GFCF_Data_MaxMin_2004,GFCF_Data_MaxMin_2007, GFCF_Data_MaxMin_2011)
  remove(GFCF_Data_MaxMin_2004,GFCF_Data_MaxMin_2007, GFCF_Data_MaxMin_2011)
}  
  
if (WB_SDG == "WB") {
##For Housing data, only one column and already 0 to 100

HousingData_MaxMin <- rbind(HousingData_NoNAs_2004,HousingData_NoNAs_2007,HousingData_NoNAs_2011)
colnames(HousingData_MaxMin) <- c("country", "year", "MaxMin_Index")
HousingData_MaxMin$MaxMin_Index <- HousingData_MaxMin$MaxMin_Index/100
HousingData_MaxMin$CLUM_category <- "Housing"
HousingData_MaxMin$NAPercent <- (rowSums(is.na(HousingData_MaxMin))/max(rowSums(is.na(HousingData_MaxMin))))*100

##For Goods Data, just rename column
Goods_Data_MaxMin <- Goods_Data
colnames(Goods_Data_MaxMin) <- c("country", "year", "MaxMin_Index")
Goods_Data_MaxMin$MaxMin_Index <- Goods_Data_MaxMin$MaxMin_Index/
  max(Goods_Data_MaxMin$MaxMin_Index, na.rm = TRUE)
Goods_Data_MaxMin$CLUM_category <- "Goods"
Goods_Data_MaxMin$NAPercent <- (rowSums(is.na(Goods_Data))/max(rowSums(is.na(Goods_Data))))*100

#Single column version
######  Transport if there is only 1 column (eg. if the NA_Factor for Transport is 2)
# colnames(TransportData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
# TransportData_NoNAs$MaxMin_Index <- TransportData_NoNAs$MaxMin_Index/max(TransportData_NoNAs$MaxMin_Index, na.rm=TRUE)
# TransportData_NoNAs$CLUM_category <- "Personal Transportation"
# TransportData_NoNAs$NAPercent <- (rowSums(is.na(TransportData_NoNAs))/max(rowSums(is.na(TransportData_NoNAs))))*100
# TransportData_MaxMin <- TransportData_NoNAs[,c(2:6)]
}

##Binding Data together for single spreadsheet
MaxMinData <- rbind(FoodData_MaxMin, GovernmentData_MaxMin, ServicesData_MaxMin, 
                    TransportData_MaxMin, HousingData_MaxMin, Goods_Data_MaxMin, 
                    if (WB_SDG=="SDG") GFCF_Data_MaxMin)
colnames(MaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NAPercent")

########Now for z-score stuff
####Max/Min function calculation####
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


FoodData_ZScore_2004 <- ZScore_Fun(FoodData_NoNAs_2004, "Food")
FoodData_ZScore_2007 <- ZScore_Fun(FoodData_NoNAs_2007, "Food")
FoodData_ZScore_2011 <- ZScore_Fun(FoodData_NoNAs_2011, "Food")
FoodData_ZScore <- rbind(FoodData_ZScore_2004, FoodData_ZScore_2007, FoodData_ZScore_2011)
remove(FoodData_ZScore_2004, FoodData_ZScore_2007, FoodData_ZScore_2011)
GovernmentData_ZScore_2004 <- ZScore_Fun(GovernmentData_NoNAs_2004, "Government")
GovernmentData_ZScore_2007 <- ZScore_Fun(GovernmentData_NoNAs_2007, "Government")
GovernmentData_ZScore_2011 <- ZScore_Fun(GovernmentData_NoNAs_2011, "Government")
GovernmentData_ZScore <- rbind(GovernmentData_ZScore_2004, GovernmentData_ZScore_2007, GovernmentData_ZScore_2011)
remove(GovernmentData_ZScore_2004, GovernmentData_ZScore_2007, GovernmentData_ZScore_2011)
TransportData_ZScore_2004 <- ZScore_Fun(TransportData_NoNAs_2004, "Personal Transportation")
TransportData_ZScore_2007 <- ZScore_Fun(TransportData_NoNAs_2007, "Personal Transportation")
TransportData_ZScore_2011 <- ZScore_Fun(TransportData_NoNAs_2011, "Personal Transportation")
TransportData_ZScore <- rbind(TransportData_ZScore_2004, TransportData_ZScore_2007, TransportData_ZScore_2011)
remove(TransportData_ZScore_2004, TransportData_ZScore_2007, TransportData_ZScore_2011)
ServicesData_ZScore_2004 <- ZScore_Fun(ServicesData_NoNAs_2004, "Services")
ServicesData_ZScore_2007 <- ZScore_Fun(ServicesData_NoNAs_2007, "Services")
ServicesData_ZScore_2011 <- ZScore_Fun(ServicesData_NoNAs_2011, "Services")
ServicesData_ZScore <- rbind(ServicesData_ZScore_2004, ServicesData_ZScore_2007, ServicesData_ZScore_2011)
remove(ServicesData_ZScore_2004, ServicesData_ZScore_2007, ServicesData_ZScore_2011)
if (WB_SDG=="SDG"){
  #HousingData_ZScore_2004 <- ZScore_Fun(HousingData_NoNAs_2004, "Housing"). # No data
  HousingData_ZScore_2007 <- ZScore_Fun(HousingData_NoNAs_2007, "Housing")
  HousingData_ZScore_2011 <- ZScore_Fun(HousingData_NoNAs_2011, "Housing")
  HousingData_ZScore <- rbind(
  # HousingData_ZScore_2004, # No data
     HousingData_ZScore_2007, HousingData_ZScore_2011)
  remove(
  # HousingData_ZScore_2004, # No data
    HousingData_ZScore_2007, HousingData_ZScore_2011)
  Goods_Data_ZScore_2004 <- ZScore_Fun(Goods_Data_NoNAs_2004, "Goods")
  Goods_Data_ZScore_2007 <- ZScore_Fun(Goods_Data_NoNAs_2007, "Goods")
  Goods_Data_ZScore_2011 <- ZScore_Fun(Goods_Data_NoNAs_2011, "Goods")
  Goods_Data_ZScore <- rbind(Goods_Data_ZScore_2004, Goods_Data_ZScore_2007, Goods_Data_ZScore_2011)
  remove(Goods_Data_ZScore_2004, Goods_Data_ZScore_2007, Goods_Data_ZScore_2011)
  GFCF_Data_ZScore_2004 <- ZScore_Fun(GFCF_Data_NoNAs_2004, "Gross Fixed Capital Formation")
  GFCF_Data_ZScore_2007 <- ZScore_Fun(GFCF_Data_NoNAs_2007, "Gross Fixed Capital Formation")
  GFCF_Data_ZScore_2011 <- ZScore_Fun(GFCF_Data_NoNAs_2011, "Gross Fixed Capital Formation")
  GFCF_Data_ZScore <- rbind(GFCF_Data_ZScore_2004, GFCF_Data_ZScore_2007, GFCF_Data_ZScore_2011)
  remove(GFCF_Data_ZScore_2004, GFCF_Data_ZScore_2007, GFCF_Data_ZScore_2011)
}

if (WB_SDG=="WB"){
  ##For Housing data, only one column and already 0 to 1
  ###HousingData_ZScore <- ZScore_Fun(HousingData_NoNAs, "Housing")
  ZScore_Index <- scale(HousingData_MaxMin$MaxMin_Index)
  HousingData_ZScore <- cbind(HousingData_MaxMin[,c(1,2)], ZScore_Index, HousingData_MaxMin[,4])
  colnames(HousingData_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")
  
  ##For Goods Data, just rename column
  ZScore_Index <- scale(Goods_Data_MaxMin$MaxMin_Index)
  Goods_Data_ZScore <- cbind(Goods_Data_MaxMin[,c(1:2)], ZScore_Index,Goods_Data_MaxMin[,4])
  colnames(Goods_Data_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")
}

##Binding Data together for single spreadsheet
ZScoreData <- rbind(FoodData_ZScore, GovernmentData_ZScore, ServicesData_ZScore, 
                    TransportData_ZScore, HousingData_ZScore, Goods_Data_ZScore,
                    if (WB_SDG=="SDG") GFCF_Data_ZScore)
colnames(ZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category")

##Combining MaxMin and Z-score datasets
IndicesData <- left_join(ZScoreData, MaxMinData, by = c("country", "year", "CLUM_category"))

#write.csv(IndicesData, "./World Bank Data/IndicesData.csv")
if(WB_SDG=="WB"){
write.csv(IndicesData, "./GFN_Data_Visualization/ScatterVisuals/IndicesDataWB.csv")
}
if(WB_SDG=="SDG"){
write.csv(IndicesData, "./GFN_Data_Visualization/ScatterVisuals/IndicesDataSDG.csv")
}

print ('Looks good, Run 2.Country Correspondence to make sure all countries and groupings were dealt with
       and do the GTAP weighted aggregation')

