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
             "Other non-specified areas",
             "Central America (excluding high income)", "South America (excluding high income)",
             # A bunch of Indonesian provinces I think
             "Aceh Barat Daya, Kab.", "Aceh Barat, Kab.", "Aceh Besar, Kab.", "Aceh Jaya, Kab.", "Aceh Selatan, Kab.",
             "Aceh Singkil, Kab.", "Aceh Tamiang, Kab.", "Aceh Tengah, Kab.", "Aceh Tenggara, Kab.", "Aceh Timur, Kab.",
             "Aceh Utara, Kab.", "Adm. Kepulauan Seribu, Kab.", "Agam, Kab.", "Alor, Kab.", "Ambon, Kota", "Asahan, Kab.",
             "Asmat, Kab.", "Badung, Kab.", "Balangan, Kab.", "Bali, Prop.", "Balikpapan, Kota", "Banda Aceh, Kota",
             "Bandar Lampung, Kota", "Bandung Barat, Kab.", "Bandung, Kab.", "Bandung, Kota", "Banggai Kepulauan, Kab.",
             "Banggai Laut, Kab.", "Banggai, Kab.", "Bangka Barat, Kab.", "Bangka Selatan, Kab.", "Bangka Tengah, Kab.",
             "Bangka, Kab.", "Bangkalan, Kab.", "Bangli, Kab.", "Banjar Baru, Kota", "Banjar, Kab.", "Banjar, Kota",
             "Banjarmasin, Kota", "Banjarnegara, Kab.", "Bantaeng, Kab.", "Banten, Prop.", "Bantul, Kab.", "Banyuasin, Kab.",
             "Banyumas, Kab.", "Banyuwangi, Kab.", "Barito Kuala, Kab.", "Barito Selatan, Kab.", "Barito Timur, Kab.",
             "Barito Utara, Kab.", "Barru, Kab.", "Batam, Kota", "Batang Hari, Kab.", "Batang, Kab.", "Batu Bara, Kab.",
             "Batu, Kota", "Bau-Bau, Kota", "Bekasi, Kab.", "Bekasi, Kota", "Belitung Timur, Kab.", "Belitung, Kab.",
             "Belu, Kab.", "Bener Meriah, Kab.", "Bengkalis, Kab.", "Bengkayang, Kab.", "Bengkulu Selatan, Kab.",
             "Bengkulu Tengah, Kab.", "Bengkulu Utara, Kab.", "Bengkulu, Kota", "Bengkulu, Prop.", "Berau, Kab.",
             "Biak Numfor, Kab.", "Bima, Kab.", "Bima, Kota", "Binjai, Kota", "Bintan, Kab.", "Bireuen, Kab.", "Bitung, Kota",
             "Blitar, Kab.", "Blitar, Kota", "Blora, Kab.", "Boalemo, Kab.", "Bogor, Kab.", "Bogor, Kota", "Bojonegoro, Kab.",
             "Bolaang Mongondow Selatan, Kab.", "Bolaang Mongondow Timur, Kab.", "Bolaang Mongondow Utara, Kab.", 
             "Bolaang Mongondow, Kab.", "Bombana, Kab.", "Bondowoso, Kab.", "Bone Bolango, Kab.", "Bone, Kab.", "Bontang, Kota",
             "Boven Digoel, Kab.", "Boyolali, Kab.", "Brebes, Kab.", "Bukittinggi, Kota", "Buleleng, Kab.", "Bulukumba, Kab.",
             "Bulungan, Kab.", "Bungo, Kab.", "Buol, Kab.", "Buru Selatan, Kab.", "Buru, Kab.", "Buton Selatan, Kab.",
             "Buton Tengah, Kab.", "Buton Utara, Kab.", "Buton, Kab.", "Ciamis, Kab.", "Cianjur, Kab.", "Cilacap, Kab.",
             "Cilegon, Kota", "Cimahi, Kota", "Cirebon, Kab.", "Cirebon, Kota", "Dairi, Kab.", "Deiyai, Kab.",
             "Deli Serdang, Kab.", "Demak, Kab.", "Denpasar, Kota", "Depok, Kota", "Dharmasraya, Kab.", "DI Yogyakarta,
             Prop.", "DKI Jakarta, Prop.", "Dogiyai, Kab.", "Dompu, Kab.", "Donggala, Kab.", "Dumai, Kota", "Empat Lawang,
             Kab.", "Ende, Kab.", "Enrekang, Kab.", "Fakfak, Kab.", "Flores Timur, Kab.", "Garut, Kab.", "Gayo Lues, Kab.",
             "Gianyar, Kab.", "Gorontalo Utara, Kab.", "Gorontalo, Kab.", "Gorontalo, Kota", "Gorontalo, Prop.", "Gowa, Kab.",
             "Gresik, Kab.", "Grobogan, Kab.", "Gunung Kidul, Kab.", "Gunung Mas, Kab.", "Gunung Sitoli, Kota",
             "Halmahera Barat, Kab.", "Halmahera Selatan, Kab.", "Halmahera Tengah, Kab.", "Halmahera Timur, Kab.",
             "Halmahera Utara, Kab.", "Hulu Sungai Selatan, Kab.", "Hulu Sungai Tengah, Kab.", "Hulu Sungai Utara, Kab.",
             "Humbang Hasundutan, Kab.", "Indragiri Hilir, Kab.", "Indragiri Hulu, Kab.", "Indramayu, Kab.",
             "Intan Jaya, Kab.", "Jakarta Barat, Kota", "Jakarta Pusat, Kota", "Jakarta Selatan, Kota", "Jakarta Timur,
             Kota", "Jakarta Utara, Kota", "Jambi, Kota", "Jambi, Prop.", "Jawa Barat, Prop.", "Jawa Tengah, Prop.",
             "Jawa Timur, Prop.", "Jayapura, Kab.", "Jayapura, Kota", "Jayawijaya, Kab.", "Jember, Kab.", "Jembrana, Kab.",
             "Jeneponto, Kab.", "Jepara, Kab.", "Jombang, Kab.", "Kaimana, Kab.", "Kalimantan Barat, Prop.",
             "Kalimantan Selatan, Prop.", "Kalimantan Tengah, Prop.", "Kalimantan Timur, Prop.", "Kalimantan Utara, Prop.",
             "Kampar, Kab.", "Kapuas Hulu, Kab.", "Kapuas, Kab.", "Karanganyar, Kab.", "Karangasem, Kab.", "Karawang, Kab.",
             "Karimun, Kab.", "Karo, Kab.", "Katingan, Kab.", "Kaur, Kab.", "Kayong Utara, Kab.", "Kebumen, Kab.",
             "Kediri, Kab.", "Kediri, Kota", "Keerom, Kab.", "Kendal, Kab.", "Kendari City",
             "Kep. Siau Tagulandang Biaro, Kab.", "Kepahiang, Kab.", "Kepulauan Anambas, Kab.", "Kepulauan Aru, Kab.",
             "Kepulauan Bangka-Belitung, Prop.", "Kepulauan Mentawai, Kab.", "Kepulauan Meranti, Kab.",
             "Kepulauan Riau, Prop.", "Kepulauan Sangihe, Kab.", "Kepulauan Selayar, Kab.", "Kepulauan Sula, Kab.",
             "Kepulauan Talaud, Kab.", "Kepulauan Tidore, Kota", "Kepulauan Yapen, Kab.", "Kerinci, Kab.", "Ketapang, Kab.",
             "Klaten, Kab.", "Klungkung, Kab.", "Kolaka Timur, Kab.", "Kolaka Utara, Kab.", "Kolaka, Kab.",
             "Konawe Kepulauan, Kab.", "Konawe Selatan, Kab.", "Konawe Utara, Kab.", "Konawe, Kab.", "Kota Baru, Kab.",
             "Kotamobagu, Kota", "Kotawaringin Barat, Kab.", "Kotawaringin Timur, Kab.", "Kuantan Singingi, Kab.",
             "Kubu Raya, Kab.", "Kudus, Kab.", "Kulon Progo, Kab.", "Kuningan, Kab.", "Kupang, Kab.", "Kupang, Kota",
             "Kutai Barat, Kab.", "Kutai Kartanegara, Kab.", "Kutai Timur, Kab.", "Labuhan Batu Selatan, Kab.",
             "Labuhan Batu Utara, Kab.", "Labuhan Batu, Kab.", "Lahat, Kab.", "Lamandau, Kab.", "Lamongan, Kab.",
             "Lampung Barat, Kab.", "Lampung Selatan, Kab.", "Lampung Tengah, Kab.", "Lampung Timur, Kab.",
             "Lampung Utara, Kab.", "Lampung, Prop.", "Landak, Kab.", "Langkat, Kab.", "Langsa, Kota", "Lanny Jaya, Kab.",
             "Lebak, Kab.", "Lebong, Kab.", "Lembata, Kab.", "Lhokseumawe, Kota", "Limapuluh Kota, Kab", "Lingga, Kab.",
             "Lombok Barat, Kab.", "Lombok Tengah, Kab.", "Lombok Timur, Kab.", "Lombok Utara, Kab.", "Lubuklinggau, Kota",
             "Lumajang, Kab.", "Luwu Timur, Kab.", "Luwu Utara, Kab.", "Luwu, Kab.", "Madiun, Kab.", "Madiun, Kota",
             "Magelang, Kab.", "Magelang, Kota", "Magetan, Kab.", "Mahakam Hulu, Kab.", "Majalengka, Kab.", "Majene, Kab.",
             "Makassar, Kota", "Malaka, Kab.", "Malang, Kab.", "Malang, Kota", "Malinau, Kab.", "Maluku Barat Daya, Kab.",
             "Maluku Tengah, Kab.", "Maluku Tenggara Barat, Kab.", "Maluku Tenggara, Kab.", "Maluku Utara, Prop.",
             "Maluku, Prop.", "Mamasa, Kab.", "Mamberamo Raya, Kab.", "Mamberamo Tengah, Kab.", "Mamuju Tengah, Kab.",
             "Mamuju Utara, Kab.", "Mamuju, Kab.", "Manado, Kota", "Mandailing Natal, Kab", "Manggarai Barat, Kab.",
             "Manggarai Timur, Kab.", "Manggarai, Kab.", "Manokwari Selatan, Kab.", "Manokwari, Kab.", "Mappi, Kab.",
             "Maros, Kab.", "Mataram, Kota", "Maybrat, Kab.", "Medan, Kota", "Melawi, Kab.", "Merangin, Kab.",
             "Merauke, Kab.", "Mesuji, Kab.", "Metro, Kota", "Mimika, Kab.", "Minahasa Selatan, Kab.",
             "Minahasa Tenggara, Kab.", "Minahasa Utara, Kab.", "Minahasa, Kab.", "Mojokerto, Kab.", "Mojokerto, Kota",
             "Morowali Utara, Kab.", "Morowali, Kab.", "Muara Enim, Kab.", "Muaro Jambi, Kab.", "Mukomuko, Kab.",
             "Muna Barat, Kab.", "Muna, Kab.", "Murung Raya, Kab.", "Musi Banyuasin, Kab.", "Musi Rawas Utara, Kab.",
             "Musi Rawas, Kab.", "Nabire, Kab.", "Nagan Raya, Kab.", "Nagekeo, Kab.", "Nanggroe Aceh Darussalam, Prop.",
             "Natuna, Kab.", "Nduga, Kab.", "Ngada, Kab.", "Nganjuk, Kab.", "Ngawi, Kab.", "Nias Barat, Kab.",
             "Nias Selatan, Kab.", "Nias Utara, Kab.", "Nias, Kab.", "Nunukan, Kab.", "Nusa Tenggara Barat, Prop.",
             "Nusa Tenggara Timur, Prop.", "Ogan Ilir, Kab.", "Ogan Komering Ilir, Kab.", "Ogan Komering Ulu Selatan, Kab.",
             "Ogan Komering Ulu Timur, Kab.", "Ogan Komering Ulu, Kab.", "Pacitan, Kab.", "Padang Lawas Utara, Kab.",
             "Padang Lawas, Kab.", "Padang Panjang, Kota", "Padang Pariaman, Kab.", "Padang Sidempuan, Kota", "Padang, Kota",
             "Pagar Alam, Kota", "Pakpak Bharat, Kab.", "Palangkaraya, Kota", "Palembang, Kota", "Palopo, Kota", "Palu, Kota",
             "Pamekasan, Kab.", "Pandeglang, Kab.", "Pangandaran, Kab.", "Pangkajene Kepulauan, Kab.", "Pangkal Pinang, Kota",
             "Paniai, Kab.", "Papua Barat, Prop.", "Papua, Prop.", "Parepare, Kota", "Pariaman, Kota", "Parigi Moutong, Kab.",
             "Pasaman Barat, Kab.", "Pasaman, Kab", "Pasir, Kab.", "Pasuruan, Kab.", "Pasuruan, Kota", "Pati, Kab.", "Payakumbuh, Kota",
             "Pegunungan Arfak, Kab.", "Pegunungan Bintang, Kab.", "Pekalongan, Kab.", "Pekalongan, Kota", "Pekanbaru, Kota",
             "Pelalawan, Kab.", "Pemalang, Kab.", "Pematang Siantar, Kota", "Penajam Paser Utara, Kab.",
             "Penukal Abab Lematang Ilir, Kab.", "Pesawaran, Kab.", "Pesisir Barat, Kab.", "Pesisir Selatan, Kab.", "Pidie Jaya, Kab.",
             "Pidie, Kab.", "Pinrang, Kab.", "Pohuwato, Kab.", "Polewali Mandar, Kab.", "Ponorogo, Kab.", "Pontianak, Kab.",
             "Pontianak, Kota", "Poso, Kab.", "Prabumulih, Kota", "Pringsewu, Kab.", "Probolinggo, Kab.", "Probolinggo, Kota",
             "Pulang Pisau, Kab.", "Pulau Morotai, Kab.", "Pulau Taliabu, Kab.", "Puncak Jaya, Kab.", "Puncak, Kab.", "Purbalingga, Kab.",
             "Purwakarta, Kab.", "Purworejo, Kab.", "Raja Ampat, Kab.", "Rejang Lebong, Kab.", "Rembang, Kab.", "Riau, Prop.",
             "Rokan Hilir, Kab.", "Rokan Hulu, Kab.", "Rote Ndao, Kab.", "Sabang, Kota", "Sabu Raijua, Kab.", "Salatiga, Kota",
             "Samarinda, Kota", "Sambas, Kab.", "Samosir, Kab.", "Sampang, Kab.", "Sanggau, Kab.", "Sarmi, Kab.", "Sarolangun, Kab.",
             "Sawahlunto, Kota", "Sekadau, Kab.", "Seluma, Kab.", "Semarang, Kab.", "Semarang, Kota", "Seram Bagian Barat, Kab.",
             "Seram Bagian Timur, Kab.", "Serang, Kab.", "Serang, Kota", "Serdang Bedagai, Kab.", "Seruyan, Kab.", "Siak, Kab.",
             "Sibolga, Kota", "Sidenreng Rappang, Kab.", "Sidoarjo, Kab.", "Sigi, Kab.", "Sijunjung, Kab.", "Sikka, Kab.",
             "Simalungun, Kab.", "Simeulue, Kab.", "Singkawang, Kota", "Sinjai, Kab.", "Sintang, Kab.", "Situbondo, Kab.", "Sleman, Kab.",
             "Solok Selatan, Kab.", "Solok, Kab.", "Solok, Kota", "Soppeng, Kab.", "Sorong Selatan, Kab.", "Sorong, Kab.", "Sorong, Kota",
             "Sragen, Kab.", "Subang, Kab.", "Subulussalam, Kota", "Sukabumi, Kab.", "Sukabumi, Kota", "Sukamara, Kab.",
             "Sukoharjo, Kab.", "Sulawesi Barat, Prop.", "Sulawesi Selatan, Prop.", "Sulawesi Tengah, Prop.",
             "Sulawesi Tenggara, Prop.", "Sulawesi Utara, Prop.", "Sumatera Barat, Prop.", "Sumatera Selatan, Prop.",
             "Sumatera Utara, Prop.", "Sumba Barat Daya, Kab.", "Sumba Barat, Kab.", "Sumba Tengah, Kab.", "Sumba Timur, Kab.",
             "Sumbawa Barat, Kab.", "Sumbawa, Kab.", "Sumedang, Kab.", "Sumenep, Kab.", "Sungai Penuh, Kota", "Supiori, Kab.",
             "Surabaya, Kota", "Surakarta, Kota", "Tabalong, Kab.", "Tabanan, Kab.", "Takalar, Kab.", "Tambrauw, Kab",
             "Tana Tidung, Kab.", "Tana Toraja, Kab.", "Tanah Bumbu, Kab.", "Tanah Datar, Kab.", "Tanah Laut, Kab.",
             "Tangerang Selatan, Kota", "Tangerang, Kab.", "Tangerang, Kota", "Tanggamus, Kab.", "Tanjung Balai, Kota",
             "Tanjung Jabung Barat, Kab.", "Tanjung Jabung Timur, Kab.", "Tanjung Pinang, Kota", "Tapanuli Selatan, Kab.",
             "Tapanuli Tengah, Kab.", "Tapanuli Utara, Kab.", "Tapin, Kab.", "Tarakan, Kota", "Tasikmalaya, Kab.",
             "Tasikmalaya, Kota", "Tebing Tinggi, Kota", "Tebo, Kab.", "Tegal, Kab.", "Tegal, Kota", "Teluk Bintuni, Kab.",
             "Teluk Wondama, Kab.", "Temanggung, Kab.", "Ternate, Kota", "Timor Tengah Selatan, Kab.", "Timor Tengah Utara, Kab.",
             "Toba Samosir, Kab.", "Tojo Una-Una, Kab.", "Toli-Toli, Kab.", "Tolikara, Kab.", "Tomohon, Kota",
             "Toraja Utara, Kab.", "Trenggalek, Kab.", "Tual, Kota", "Tuban, Kab.", "Tulang Bawang Barat, Kab.",
             "Tulang Bawang, Kab.", "Tulungagung, Kab.", "Wajo, Kab.", "Wakatobi, Kab.", "Waropen, Kab.", "Way Kanan, Kab.",
             "Wonogiri, Kab.", "Wonosobo, Kab.", "Yahukimo, Kab.", "Yalimo, Kab.", "Yogyakarta, Kota", "DI Yogyakarta, Prop.",
             "Empat Lawang, Kab.", "Jakarta Timur, Kota",
             #Some cities and Regions
             "Chittagong", "Dhaka", "BES Islands", "Rio de Janeiro", "Sao Paulo", "Caucasian and Central Asia", "Beijing",
             "Shanghai", "Eastern Asia (not including Japan)", "Eastern Asia (including Japan)",
             "Latin America and Caribbean", "Oceania (not including Australia and New Zealand)", "South Eastern Asia",
             "Jakarta", "Surabaya", "Delhi", "Mumbai", "Osaka", "Tokyo", "Mexico City", "Monterrey",
             "Nothern America", "Kano", "Lagos", "Karachi", "Lahore", "Moscow", "St. Petersburg", "Los Angeles", "New York")
# Write to file for reading in country correspondence script
write.csv(Region_drop, file = "./DropTheseCountries.csv", row.names = F)

## WB pull and split section
#{
WB_DataPull_Function <- function(indicator_list, CLUM_startyear, CLUM_middleyear, CLUM_endyear){
  DataFrame <- WDI(country = "all", indicator = indicator_list, start = CLUM_startyear, 
                   end = CLUM_endyear, extra = FALSE, cache = NULL)
  DataFrame <- subset(DataFrame, year == CLUM_startyear | year == CLUM_middleyear | year == CLUM_endyear)
  #Get rid of rows with all NAs: seems related to multiple iso2c values for individual country/years
  DataFrame <- as.data.frame(DataFrame[rowSums(is.na(DataFrame[,4:ncol(DataFrame)])) != ncol(DataFrame[,4:ncol(DataFrame)]),])
  DataFrame$iso2c <- NULL
  # Average data when mulitple rows for a country/year
# Nope
     # aggregate(DataFrame[,4:ncol(DataFrame)] ~ country + year, mean, data = DataFrame)    
   # keys <- c("DataFrame$country", "DataFrame$year")
   # DataFrame[,lapply(.SD,mean),keys]
  #Seems good 
  DataFrame <- DataFrame %>% group_by(country, year) %>% summarise_all(funs(mean(., na.rm = TRUE)))
   return(DataFrame)
}
#Single year pull function - Actually, better to just group in the minmax zscore
# WB_DataPull_Function <- function(indicator_list, Datayear){
#   DataFrame <- WDI(country = "all",
#                    indicator = indicator_list,
#                    start = Datayear, end = Datayear, extra = FALSE, cache = NULL) 
#   return(DataFrame)
# }
### Testing
# DataFrame <- WDI(country = "all", indicator = WBFood_Indicators, start = 2004, 
#                  end = 2011, extra = FALSE, cache = NULL)
# DataFrame <- subset(DataFrame, year == 2004 | year == 2007 | year == 2011)
# DataFrame <- DataFrame[rowSums(is.na(DataFrame[,4:ncol(DataFrame)])) != ncol(DataFrame[,4:ncol(DataFrame)]),]

#WBFood_Data_2004[rowSums(is.na(WBFood_Data_2004[,3:17])) != ncol(WBFood_Data_2004[,3:17]),]

# Select Indicators and organising for World Bank data
  WBIndicatorsDownloaded <- data.frame()
  #  Date=as.Date(character()),
  #                                     File=character(), 
  #                                     User=character())
#  colnames(WBIndicatorsDownloaded) <- c()
  #if(WB_SDG=="WB"){
  ######Food Section
  #Cereal Yield (Kg Per Hectare)
  WBFood_Indicators <- c(
    "AG.YLD.CREL.KG",
    # Agriculture Value Added Per Worker (Constant 2010 US$)
    "EA.PRD.AGRI.KD", #No download/data available 6/10/18
    # Agriculture Value Added Per Hectare Of Agricultural Land (Constant 1995 US$)
    "EA.PRD.LAND.KD", #No download/data available 6/10/18
    #Fertilizer Consumption (Kilograms Per Hectare Of Arable Land)
    "AG.CON.FERT.ZS",
    #Food Production Index (2004-2006 = 100) Food production index covers food crops that are considered edible and that contain nutrients.
    "AG.PRD.FOOD.XD",
    #Agricultural Machinery, Tractors Per 100 Sq. Km Of Arable Land
    "AG.LND.TRAC.ZS",  #High is good, low is bad#
    #Agricultural Irrigated Land (% Of Total Agricultural Land)
    "AG.LND.IRIG.AG.ZS" #High is good, low is bad#
  )
  
  #High is BAD#
  #how many calories would be needed to lift the undernourished from their status, everything else being constant.
  WBFood_Indicators_reverse <- c(
    # Depth of the food deficit (kilocalories per person per day)
    "SN.ITK.DFCT", #High is BAD
    # Depth of hunger (kilocalories per person per day)	
    "SN.ITK.DPTH",
    # Prevalence of moderate or severe food insecurity in the population (%)
    "SN.ITK.MSFI.ZS",
    # Prevalence of severe food insecurity in the population (%)
    "SN.ITK.SVFI.ZS",
    #Malnourished Children (Underweight, -2SD) (% Of Children Under 5): Q1 (Lowest)
    "SH.STA.MALN.ZS", #High is BAD
    # Prevalence of undernourishment (% of population)	
    "SN.ITK.DEFC.ZS",
    # Malnourished women (BMI is less than 18.5) (% of women): Q1 (lowest)
    "SH.STA.LBMI.Q1.ZS",
    # Malnourished children (underweight, -2SD) (% of children under 5): Q1 (lowest)
    "SH.STA.MALN.Q1.ZS",
    # Malnourished children (stunting, -2SD) (% of children under 5): Q1 (lowest)
    "SH.STA.STNT.Q1.ZS",
    # Malnourished children (wasting, -2SD) (% of children under 5): Q1 (lowest)
    "SH.STA.WAST.Q1.ZS",
    # Fish Species, Threatened. number of species classified by the IUCN as endangered, vulnerable, rare, indeterminate, out of danger, or insufficiently known
    "EN.FSH.THRD.NO" #High is BAD  #Only 2017 data 6/10/18
  )

  # WBIndicatorsDownloadedF <- 
  #   subset(WBIndicatorList,WBIndicatorList$indicator %in% WBFood_Indicators)
  # WBIndicatorsDownloaded$CLUM <- "Food"
  # 
 
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBFood_Indicators]),
                           unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBFood_Indicators]),
                           "Food", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBFood_Indicators_reverse]),
                           unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBFood_Indicators_reverse]),
                           "Food", "Reversed")
  WBIndicatorsDownloaded <- rbind(WBIndicatorsDownloaded,
                                    if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                    if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBFood_Indicators <- c(WBFood_Indicators,WBFood_Indicators_reverse)
  
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
  # Now in the function #WBFood_Data$iso2c <- NULL
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
    "SE.ENR.PRSC.FM.ZS",
    # Gross Savings (% Of GDP). Gross savings are calculated as gross national income less total consumption, plus net transfers.
    "NY.GNS.ICTR.ZS",
    #Current Account Balance (% Of GDP). Current account balance is the sum of net exports of goods and services, net primary income, and net secondary income.
    "BN.CAB.XOKA.GD.ZS", #High is good (I reckon)
    #Expenditure On Education As % Of Total Government Expenditure (%)
    "SE.XPD.TOTL.GB.ZS",
    #Government Expenditure Per Primary Student (Constant PPP$)
    "UIS.XUNIT.PPPCONST.1.FSGOV",
    #Percentage Of Teachers In Primary Education Who Are Trained, Both Sexes (%)
    "SE.PRM.TCAQ.ZS",
    #Domestic Credit To Private Sector (% Of GDP)
    "FS.AST.PRVT.GD.ZS",
    #Health Expenditure, Public (% Of GDP)
    "SH.XPD.PUBL.ZS",  #No download/data availability 6/10/18
    #CPIA Economic Management Cluster Average (1=Low To 6=High). The economic management cluster includes macroeconomic management, fiscal policy, and debt policy.
    "IQ.CPA.ECON.XQ",
    #CPIA Public Sector Management And Institutions Cluster Average (1=Low To 6=High)
    "IQ.CPA.PUBS.XQ",
    #IDA Resource Allocation Index (1=Low To 6=High). IDA Resource Allocation Index is obtained by calculating the average score for each cluster and then by averaging those scores. For each of 16 criteria countries are rated on a scale of 1 (low) to 6 (high)
    "IQ.CPA.IRAI.XQ",
    #Proportion Of Seats Held By Women In National Parliaments (%)
    "SG.GEN.PARL.ZS",
    #CPIA Policies For Social Inclusion/Equity Cluster Average (1=Low To 6=High). The policies for social inclusion and equity cluster includes gender equality, equity of public resource use, building human resources, social protection and labor, and policies and institutions for environmental sustainability.
    "IQ.CPA.SOCI.XQ",
    # CPIA quality of public administration rating (1=low to 6=high)	
    "IQ.CPA.PADM.XQ",	
    # CPIA equity of public resource use rating (1=low to 6=high)	
    "IQ.CPA.PRES.XQ",
    # CPIA transparency, accountability, and corruption in the public sector rating (1=low to 6=high)	
    "IQ.CPA.TRAN.XQ",
    # Gini inequality index reduction (%) - All Social Protection and Labor	
    "per_allsp_gini_tot",
    #CPIA Structural Policies Cluster Average (1=Low To 6=High). The structural policies cluster includes trade, financial sector, and business regulatory environment.
    "IQ.CPA.STRC.XQ" 
    #Central Government Debt, Total (% Of GDP). Debt is the entire stock of direct government fixed-term contractual obligations to others outstanding on a particular date. It includes domestic and foreign liabilities such as currency and money deposits, securities other...
  )
  WBGovernment_Indicators_reverse <- c(
    #High is BAD
    "GC.DOD.TOTL.GD.ZS", #High is BAD
    #Unemployment, Total (% Of Total Labor Force) (National Estimate)
    "SL.UEM.TOTL.NE.ZS", #High is BAD
    #Net Official Development Assistance Received (Constant 2014 US$)
    "DT.ODA.ODAT.KD", #High is BAD
    # Bribery index (% of gift or informal payment requests during public transactions)	
    "IC.FRM.CORR.GRAFT2",
    #	Informal payments to public officials (% of firms)
    "IC.FRM.CORR.ZS",
    #Inflation, Consumer Prices (Annual %)
    "FP.CPI.TOTL.ZG" #High is BAD
  )  
  
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBGovernment_Indicators),
  #                                     CLUM="Government"),WBIndicatorsDownloaded)
  
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBGovernment_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBGovernment_Indicators]),
                             "Government", "Fwd")
  
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBGovernment_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBGovernment_Indicators_reverse]),
                             "Government", "Reversed")

  WBIndicatorsDownloaded <- rbind(WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBGovernment_Indicators <- c(WBGovernment_Indicators, WBGovernment_Indicators_reverse)
  WBGovernment_Data <- WB_DataPull_Function(WBGovernment_Indicators, 2004, 2007, 2011)
  WBGovernment_Data <- WBGovernment_Data[!(WBGovernment_Data$country %in% Region_drop),]
  #Now in the pull funtcion WBGovernment_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBGovernment_Indicators_reverse){WBGovernment_Data[i] <- 0 - WBGovernment_Data[i] +
    max(WBGovernment_Data[i], na.rm = TRUE)
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBGovernment_Data)), i, sep = "_") 
    assign(nam, WBGovernment_Data[WBGovernment_Data$year==i,])
  }
  
  remove(WBGovernment_Indicators, WBGovernment_Indicators_reverse, WBGovernment_Data)

  ##Services Metrics
## Search terms included 'educ', 'sport', 'theat', 'financ', 'liter', 'serv', 'recrea', 'leis', 'cult'
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
    #	DHS: Net intake rate for the first grade of primary education	
    "HH.DHS.NIR.1",
    #	PASEC: Mean performance on the mathematics scale for 6th grade students who attended pre-primary education	
    "LO.PASEC.MAT.6.PP",
    #	PASEC: Mean performance on the reading scale for 6th grade students who attended pre-primary education	
    "LO.PASEC.REA.6.PP",
    # Gross graduation ratio from primary education, both sexes (%)	
    "SE.PRM.CMPL.ZS",
    # Percentage of students in tertiary education who are female (%)	
    "SE.TER.ENRL.FE.ZS",
    #	Government expenditure on education, total (% of governmen
    "SE.XPD.TOTL.GB.ZS",
    # PIAAC: Adults by literacy proficiency level (%). Level 5	
    "LO.PIAAC.LIT.5",
    # Illiteracy rate, adult total (% of people ages 15 and above)	
    "SE.ADT.ILIT.ZS",
    #	No account because financial services are too expensive (% a
    "fin11b.a",
    # People Using Basic Sanitation Services (% Of Population)
    "SH.STA.BASS.ZS",
    #	People using safely managed drinking water services (% of population)	
    "SH.H2O.SMDW.ZS",
    #	Population covered by mobile cellular network (%)
    "IT.CEL.COVR.ZS",
    #	Fixed line and mobile cellular subscriptions (per 100 people)
    "IT.TEL.TOTL.P2",
    #	Community health workers (per 1,000 people)
    "SH.MED.CMHW.P3",
    #	Specialist surgical workforce (per 100,000 population)
    "SH.MED.SAOP.P5",
    # CPIA financial sector rating (1=low to 6=high) .  Financial sector assesses the structure of the financial sector and the policies and regulations that affect it. 
    "IQ.CPA.FINS.XQ"
  )
  # IC.ELC.RSTT.XD.08.DB1619	Getting electricity: Reliability of supply and transparency of tariff index (0-8) (DB16-20 methodology)
  # IC.ELC.RSTT.XD.08.DFRN.DB1619	Getting electricity: Reliability of supply and transparency of tariff index (0-8) (DB16-20 methodology) - Score
  WBServices_Indicators_reverse <- c(
    #High is BAD
    # Pupil-teacher ratio, preprimary .  Preprimary school pupil-teacher ratio is the average number of pupils per teacher in preprimary school. 
    "SE.PRE.ENRL.TC.ZS", #High is BAD
    #	Barro-Lee: Percentage of population age 15+ with no education
    "BAR.NOED.15UP.ZS",
    #	Cumulative drop-out rate to the last grade of lower secondary general education, both sexes (%)	
    "UIS.DR.2.GPV.T",
    #	Pupil/qualified teacher ratio in pre-primary education (headcount basis)	
    "UIS.PTRHC.02.QUALIFIED",
    #	Pupil/qualified teacher ratio in primary education (headcount basis)	
    "UIS.PTRHC.1.QUALIFIED",
    #	Pupil/trained teacher ratio in secondary education (headcount basis)	
    "UIS.PTRHC.2T3.TRAINED",
    # Pupil-teacher ratio, primary .  Primary school pupil-teacher ratio is the average number of pupils per teacher in primary school. 
    "SE.PRM.ENRL.TC.ZS" #High is BAD
  )
  
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBServices_Indicators),
  #                                     CLUM="Services"),WBIndicatorsDownloaded)
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBServices_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBServices_Indicators]),
                             "Services", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBServices_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBServices_Indicators_reverse]),
                             "Services", "Reversed")
  WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBServices_Indicators <- c(WBServices_Indicators, WBServices_Indicators_reverse)
  WBServices_Data <- WB_DataPull_Function(WBServices_Indicators, 2004, 2007, 2011)
  WBServices_Data <- WBServices_Data[!(WBServices_Data$country %in% Region_drop),]
  # Now in the pull function WBServices_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  ## Added catches for non-downloaeded series AND all nas
  for (i in WBServices_Indicators_reverse[WBServices_Indicators_reverse %in% colnames(WBServices_Data)]){
    if(!all(is.na(WBServices_Data[i]))){
      WBServices_Data[i] <- 0 - WBServices_Data[i] + max(WBServices_Data[i], na.rm = TRUE)
   }
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBServices_Data)), i, sep = "_") 
    assign(nam, WBServices_Data[WBServices_Data$year==i,])
  }
  remove(WBServices_Indicators, WBServices_Indicators_reverse, WBServices_Data)
  
  ######Personal Transportation Section
  ## Search terms included 'transp', 'trans', auto', 'veh', 'train', 'loco', 'work', 'comm', 'mob', 'truck', 'car', 'carr'
  WBTransport_Indicators <- c(
    # Urban Road Density (KMs Per 1000 Population) .  Urban roads density is measured in KMs of Urban roads in the area (State, District) divided by population in thousands in that area (State, District). Urban roads are roads within a limits of a Municipality, Military Cantonment, Port o a Railway Authority. 
    "IN.TRANSPORT.URBNRD.DENSIT",
    # Rural Road Density (KMs/1000 Population) .  Rural roads density is measured in KMs of rural roads in the area (State, District) divided by population in thousands in that area (State, District). Rural roads are roads within a district for which the specifications are lower than for district roads.  
    "IN.TRANSPORT.RURLRD.DENSIT",
    # Access to an all-season road (% of rural population) .  Access to an all-season road is measured as the proportion of rural people who live within 2 kilometers (typically equivalent to a 20-minute walk) of an all-season road. An all-season road is a road that is motorable all year by the prevailing means of rural transport (often a pick-up or a truck which does not have four-wheel-drive). Predictable interruptions of short duration during inclement weather (e.g. heavy rainfall) are acceptable, particularly on low volume roads. The preferred approach to measuring this indicator is by analysis of household surveys that include appropriate questions about access to transport. 
    "IS.ROD.ALLS.ZS",
    ## Removed because it's not weighted by population so big populations (China, India) big outliers
    # # Railways, passengers carried (million passenger-km) .  Passengers carried by railway are the number of passengers transported by rail times kilometers traveled. 
    # "IS.RRS.PASG.KM",
    # Railways, passenger-km (per PPP $ million of GDP) .   
    "IS.RRS.PASG.K2.PP.ZS",
    # Roads, passengers carried (million passenger-km) .  Passengers carried by road are the number of passengers transported by road times kilometers traveled. 
    "IS.ROD.PSGR.K6",
    # Roads, paved (% of total roads) .  Paved roads are those surfaced with crushed stone (macadam) and hydrocarbon binder or bituminized agents, with concrete, or with cobblestones, as a percentage of all the country's roads, measured in length. 
    "IS.ROD.PAVE.ZS",
    #	Motor vehicles (per 1,000 people)
    "IS.VEH.NVEH.P3",
    #No per cap so outlier for high population countries    # Air transport, passengers carried .  Air passengers carried include both domestic and international aircraft passengers of air carriers registered in the country. 
                # "IS.AIR.PSGR",
    #	Logistics performance index: Quality of trade and transport-related infrastructure 
    "LP.LPI.INFR.XQ",
    # Railways, passenger-km (per PPP $ million of GDP)	
    "IS.RRS.PASG.K2.PP.ZS",
    # Mortality caused by road traffic injury (per 100,000 people) .  Mortality caused by road traffic injury is estimated road traffic fatal injury deaths per 100,000 population. 
    "SH.STA.TRAF.P5"
  )
  WBTransport_Indicators_reverse <- c(
    # Traffic accidents (injured or killed per 1,000 vehicles)	
    "EN.TRN.ACCT.VEH.ZS",
    #	Road traffic (vehicles per km)
    "EN.ROD.TRAF",
    #	Transportation (% of firms identifying this as a major constraint)	
    "IC.FRM.TRSP.ZS",
    #	Problems in accessing health care (having to take transport) (% of women): Q1 (lowest)	Problems in accessing health care: Percentage of women who report they have big problems in accessing health care for themselves when they are sick, by type of problem. The types of problem specified are; knowing where to go for treatment, getting permission to go for treatment, getting money for treatment, distance to health facility, having to take transport, not wanting to go alone, and concern there may not be a female provider.	Health Nutrition and Population Statistics by Wealth Quintile	Household Surveys (DHS, MICS)
    "SH.ACS.TRAN.Q1.ZS",
    #	Problems in accessing health care (having to take transport) (% of women): Q2	Problems in accessing health care: Percentage of women who report they have big problems in accessing health care for themselves when they are sick, by type of problem. The types of problem specified are; knowing where to go for treatment, getting permission to go for treatment, getting money for treatment, distance to health facility, having to take transport, not wanting to go alone, and concern there may not be a female provider.	Health Nutrition and Population Statistics by Wealth Quintile	Household Surveys (DHS, MICS)
    "SH.ACS.TRAN.Q2.ZS",
    #	Problems in accessing health care (having to take transport) (% of women): Q3	Problems in accessing health care: Percentage of women who report they have big problems in accessing health care for themselves when they are sick, by type of problem. The types of problem specified are; knowing where to go for treatment, getting permission to go for treatment, getting money for treatment, distance to health facility, having to take transport, not wanting to go alone, and concern there may not be a female provider.	Health Nutrition and Population Statistics by Wealth Quintile	Household Surveys (DHS, MICS)
    "SH.ACS.TRAN.Q3.ZS",
  	#	Problems in accessing health care (having to take transport) (% of women): Q4	Problems in accessing health care: Percentage of women who report they have big problems in accessing health care for themselves when they are sick, by type of problem. The types of problem specified are; knowing where to go for treatment, getting permission to go for treatment, getting money for treatment, distance to health facility, having to take transport, not wanting to go alone, and concern there may not be a female provider.	Health Nutrition and Population Statistics by Wealth Quintile	Household Surveys (DHS, MICS)
    "SH.ACS.TRAN.Q4.ZS",
    #	Road sector gasoline fuel consumption per capita (kg of oil equivalent)	
    "IS.ROD.SGAS.PC",
    #	Problems in accessing health care (having to take transport) (% of women): Q5 (highest)	
    "SH.ACS.TRAN.Q5.ZS"
      )
  
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBTransport_Indicators),
  #                                     CLUM="Personal Transportation"),WBIndicatorsDownloaded)
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBTransport_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBTransport_Indicators]),
                             "Personal Transportation", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBTransport_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBTransport_Indicators_reverse]),
                             "Personal Transportation", "Reversed")
  WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  WBTransport_Indicators <- c(WBTransport_Indicators,WBTransport_Indicators_reverse)
  WBTransport_Data <- WB_DataPull_Function(WBTransport_Indicators, 2004, 2007, 2011)
  WBTransport_Data <- WBTransport_Data[!(WBTransport_Data$country %in% Region_drop),]
  # Now is pull function WBTransport_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBTransport_Indicators_reverse[WBTransport_Indicators_reverse %in% colnames(WBTransport_Data)]){
    if(!all(is.na(WBTransport_Data[i]))){
      WBTransport_Data[i] <- 0 - WBTransport_Data[i] + max(WBTransport_Data[i], na.rm = TRUE)
    }
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBTransport_Data)), i, sep = "_") 
    assign(nam, WBTransport_Data[WBTransport_Data$year==i,])
  }
  remove(WBTransport_Indicators, WBTransport_Data, WBTransport_Indicators_reverse)
  
  ####Housing Section
  # Search terms included 'shel', 'buil', 'constr', 'qual', 'bath', 'cook', 'healt', 'hous', 'kitch', 'safe', 'roof', 'disas', 'home', 'displ'
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
    #	Access to Clean Fuels and Technologies for cooking (% of total population)	
    "2.1_ACCESS.CFT.TOT",
    #	Access to clean fuels and technologies for cooking (% of population)	
    "EG.CFT.ACCS.ZS",
    #	Dealing with construction permits: Quality of building regulations index (0-2) (DB16-20 methodology)
    "IC.CNST.PRMT.QBR.XD.02.DB1619",
    #	Dealing with construction permits: Building quality control index (0-15) (DB16-20 methodology) - Score	
    "IC.CNST.PRMT.BQCI.015.DB1619.DFRN",
    #	Household Access to Electricity: Total (in % of total household)	
    "HOU.ELC.ACSN.ZS",
    #	Household Access to Safe Water (in % of total household)
    "HOU.H2O.ACSN.ZS",
    #	Total households with drinking water facility
    "IN.POV.HH.DRKNGWATER",
    ##Main Cooking Fuel: LPG/Natural Gas/Biogas (% Of Households)
    "SG.COK.LPGN.ZS" #High is good, low is bad#
  )
  WBHousing_Indicators_reverse <- c(
    #	Location of cooking: outdoors (% of households)
    "SG.COK.OUTD.ZS",
    #	Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)	
    "SH.STA.WASH.P5",
    #	Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)	    
    "SH.STA.AIRP.P5"
    )
  
  
  # 
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBHousing_Indicators),
  #                                     CLUM="Housing"),WBIndicatorsDownloaded)

  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBHousing_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBHousing_Indicators]),
                             "Housing", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBHousing_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBHousing_Indicators_reverse]),
                             "Housing", "Reversed")
  WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBHousing_Indicators <- c(WBHousing_Indicators,WBHousing_Indicators_reverse)
  WBHousing_Data <- WB_DataPull_Function(WBHousing_Indicators, 2004, 2007, 2011)
  #Housing_Data$country <- trimws(Housing_Data$country, which = c("both", "left", "right"))
  #Filter out regions on the non-NFA country/region list
  WBHousing_Data <- WBHousing_Data[!(WBHousing_Data$country %in% as.character(Region_drop)),]
  #Drop iso column
  # Delete: Now in pull function WBHousing_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBHousing_Indicators_reverse[WBHousing_Indicators_reverse %in% colnames(WBHousing_Data)]){
    if(!all(is.na(WBHousing_Data[i]))){
      WBHousing_Data[i] <- 0 - WBHousing_Data[i] + max(WBHousing_Data[i], na.rm = TRUE)
    }
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBHousing_Data)), i, sep = "_") 
    assign(nam, WBHousing_Data[WBHousing_Data$year==i,])
  }
  
  remove(WBHousing_Indicators, WBHousing_Indicators_reverse, WBHousing_Data)
  
  ## Gross Fixed Capital Formation
  ## Search terms included 'infra', 'mach', 'buil', 'equip', 'asse', 'struc', 'dwell'
  WBGFCF_Indicators <- c(
    #  Quality- of trade and transport-related infrastructure, rank (1=highest performer)  
    "LP.LPI.INFR.RK",
    #	Machinery and transport equipment (% of value added in mfg
    "NV.MNF.MTRN.UN.ZS",
    #	Machinery and transport equipment (% of value added in manufacturing)
    "NV.MNF.MTRN.ZS.UN",
    #	Quality of port infrastructure, WEF (1=extremely underdeveloped to 7=well developed and efficient by international standards)	
    "IQ.WEF.PORT.XQ",
    #	Agricultural machinery, tractors per 100 sq. km of arable land
    "AG.LND.TRAC.ZS",
    #	Net investment in nonfinancial assets (% of GDP)
    "GC.NFN.TOTL.GD.ZS",
    #	2nd pillar: Infrastructure
    "GCI.2NDPILLAR.XQ",
    #	Dealing with construction permits (rank)
    "IC.FRM.XQ",
    #	Capital health expenditure (% of GDP)
    "SH.XPD.KHEX.GD.ZS",
    # Gross public investment (% of GDP)	
    "NE.GDI.FPUB.ZS"
  )
  WBGFCF_Indicators_reverse <- c(
  )
  
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBGFCF_Indicators),
  #                                       CLUM="GFCF"),WBIndicatorsDownloaded)
  
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBGFCF_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBGFCF_Indicators]),
                             "GFCF", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBGFCF_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBGFCF_Indicators_reverse]),
                             "GFCF", "Reversed")
  WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBGFCF_Indicators <- c(WBGFCF_Indicators,WBGFCF_Indicators_reverse)
  WBGFCF_Data <- WB_DataPull_Function(WBGFCF_Indicators, 2004, 2007, 2011)
  WBGFCF_Data <- WBGFCF_Data[!(WBGFCF_Data$country %in% Region_drop),]
  # Delete: Now in pul function WBGFCF_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBGFCF_Indicators_reverse[WBGFCF_Indicators_reverse %in% colnames(WBGFCF_Data)]){
    if(!all(is.na(WBGFCF_Data[i]))){
      WBGFCF_Data[i] <- 0 - WBGFCF_Data[i] + max(WBGFCF_Data[i], na.rm = TRUE)
    }
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBGFCF_Data)), i, sep = "_") 
    assign(nam, WBGFCF_Data[WBGFCF_Data$year==i,])
  }
  remove(WBGFCF_Indicators, WBGFCF_Data, WBGFCF_Indicators_reverse)
  
  "WB Goods data sources, for integration instead of the Oxford"
  "fwd"
  ## Goods
  ## Search terms included 
  WBGoods_Indicators <- c(
    #	Ratio of textbooks per pupil, primary education, language
    "3.14_PRI.LANGU.BOOK.PER.PUPIL",
    #	Annualized average growth rate in per capita real survey m
    "SI.SPR.PC40.ZG",
    #	Households with a radio (%)
    "IT.RAD.HOUS.ZS",
    #	Households with telephones (%)
    "IN.POV.HH.ASSETS.PHONE.PCT",
    #	Households with television (%)
    "IT.TVS.HOUS.ZS",
    #	Ratio of textbooks per pupil, primary education, mathematics
    "3.13_PRI.MATH.BOOK.PER.PUPIL"
  )
  WBGoods_Indicators_reverse <- c(
    
  )
  # WBIndicatorsDownloaded <- rbind(cbind(subset(WBIndicatorList,WBIndicatorList$indicator %in% WBGoods_Indicators),
  #                                       CLUM="Goods"),WBIndicatorsDownloaded)
  
  WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBGoods_Indicators]),
                             unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBGoods_Indicators]),
                             "Goods", "Fwd")
  WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBGoods_Indicators_reverse]),
                             unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBGoods_Indicators_reverse]),
                             "Goods", "Reversed")
  WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
                                   if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
                                   if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  WBGoods_Indicators <- c(WBGoods_Indicators,WBGoods_Indicators_reverse)
  WBGoods_Data <- WB_DataPull_Function(WBGoods_Indicators, 2004, 2007, 2011)
  WBGoods_Data <- WBGoods_Data[!(WBGoods_Data$country %in% Region_drop),]
  # Delete: now in pull function WBGoods_Data$iso2c <- NULL
  # Reverse the orders for High is BAD
  for (i in WBGoods_Indicators_reverse[WBGoods_Indicators_reverse %in% colnames(WBGoods_Data)]){
    if(!all(is.na(WBGoods_Data[i]))){
      WBGoods_Data[i] <- 0 - WBGoods_Data[i] + max(WBGoods_Data[i], na.rm = TRUE)
    }
  }
  for (i in years){
    nam <- paste(deparse(substitute(WBGoods_Data)), i, sep = "_") 
    assign(nam, WBGoods_Data[WBGoods_Data$year==i,])
  }
  remove(WBGoods_Indicators, WBGoods_Data, WBGoods_Indicators_reverse)
  
  ## Goods is now from different data source - in ass_pov_final.csv
  ##Goods Metrics (Prelminary from Scott) 
  # I'm tempted to use just 1 or two measures of material satisfaction and well-being
  # One of the World economists (http://blogs.worldbank.org/impactevaluations/what-is-the-good-life-can-we-measure-it) 
  # linked to http://ophi.org.uk/multidimensional-poverty-index/global-mpi-2017/mpi-data/ has asset poverty measures for many countires.
  # Food_Beverages_Tobacco <- "NV.MNF.FBTO.ZS.UN"
  # Textiles_Clothing <- "NV.MNF.TXTL.ZS.UN"
  # WBGoods_Data <- read.csv("./ass_pov_final.csv")
  # # Reverse the orders for High is BAD
  # WBGoods_Data$ass_pov_extr <- 0-WBGoods_Data$ass_pov_extr+max(WBGoods_Data$ass_pov_extr, na.rm = TRUE)
  # for (i in years){
  #   nam <- paste(deparse(substitute(WBGoods_Data)), i, sep = "_")
  #   assign(nam, WBGoods_Data[WBGoods_Data$year ==i,])
  # }
  # 
  # #Add a row for Goods Data, including that it's needed for the plot function
  # WBGoodsDL <- cbind.data.frame(
  #   "ass_pov_extr",
  #   "Percentage of population experiencing 'asset poverty'",
  #   "People not owning a radio, TV, telephone, bicycle, motorbike, or
  #   refrigerator, and does not own a car or truck.",
  #   "Oxford Poverty & Human Development Initiative",
  #   "Oxford University",
  #   "Goods"
  # )
  # WBGoodsDownloaded <- cbind("ass_pov_extr", "Percentage of population experiencing 'asset poverty'", "Goods", "Reversed")
  # colnames(WBGoodsDownloaded) <- colnames(WBIndicatorsDownloaded)
### Not needed/useful unless I move to WB database instead of the Oxford data
  # WBIndicatorsListF <- cbind(unique(WBIndicators$series[WBIndicators$series%in%WBGoods_Indicators]),
  #                            unique(WBIndicators$series[,"name"][WBIndicators$series%in%WBGoods_Indicators]),
  #                            "Goods", "Fwd")
  # WBIndicatorsListR <- cbind(unique(SDGIndicators$series[SDGIndicators$series%in%WBGoods_Indicators_reverse]),
  #                            unique(SDGIndicators$series["name"][SDGIndicators$series%in%WBGoods_Indicators_reverse]),
  #                            "Goods", "Reversed")
  # WBIndicatorsDownloaded <- rbind (WBIndicatorsDownloaded,
  #                                  if(ncol(WBIndicatorsListF)==4){WBIndicatorsListF}, 
  #                                  if(ncol(WBIndicatorsListR)==4){WBIndicatorsListR})
  
  colnames(WBIndicatorsDownloaded) <- c("indicator", "description", "CLUM", "Forw_Revd")
  
  # Collect Warnings (listing series not DLoaded) to include in file
  WBIndicators_Nodownloads <- as.data.frame(names(warnings()))
  
  #Output WB Indicators info, and list of what was not downloaded at all at the end
  write.csv(WBIndicatorsDownloaded,"./WBIndicatorsDLed.csv")
  cat("\n Not Downloaded (and other warnings) \n", file = "./WBIndicatorsDLed.csv", append = TRUE) 
  write.table(WBIndicators_Nodownloads,"./WBIndicatorsDLed.csv",
              append = TRUE, col.names=FALSE)
#From if WB  

#}
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
    # A try to give a message if none of the data series exist
    try(reshape2::dcast(Data, geoAreaName + timePeriodStart ~ series, value.var = 'value', fun.aggregate=mean))
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
  SDGIndicators_Nodownloads <- as.data.frame(names(warnings()))
  cat("\n Not Downloaded (and other warnings) \n", file = "./SDGIndicatorsDLed.csv", append = TRUE) 
  write.table(SDGIndicators_Nodownloads,"./SDGIndicatorsDLed.csv",
              append = TRUE, col.names=FALSE)
#From if SDG
#  }
}

WBSDGFood_Data_2004 <- merge(WBFood_Data_2004, SDG_Food_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGFood_Data_2007 <- merge(WBFood_Data_2007, SDG_Food_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGFood_Data_2011 <- merge(WBFood_Data_2011, SDG_Food_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGovernment_Data_2004 <- merge(WBGovernment_Data_2004, SDG_Government_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGovernment_Data_2007 <- merge(WBGovernment_Data_2007, SDG_Government_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGovernment_Data_2011 <- merge(WBGovernment_Data_2011, SDG_Government_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGServices_Data_2004 <- merge(WBServices_Data_2004, SDG_Services_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGServices_Data_2007 <- merge(WBServices_Data_2007, SDG_Services_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGServices_Data_2011 <- merge(WBServices_Data_2011, SDG_Services_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGTransport_Data_2004 <- left_join(WBTransport_Data_2004, SDG_Transport_Data_2004[,-2], by = c("country" = "geoAreaName"))
WBSDGTransport_Data_2007 <- left_join(WBTransport_Data_2007, SDG_Transport_Data_2007[,-2], by = c("country" = "geoAreaName"))
WBSDGTransport_Data_2011 <- left_join(WBTransport_Data_2011, SDG_Transport_Data_2011[,-2], by = c("country" = "geoAreaName"))
WBSDGHousing_Data_2004 <- merge(WBHousing_Data_2004, SDG_Housing_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGHousing_Data_2007 <- merge(WBHousing_Data_2007, SDG_Housing_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGHousing_Data_2011 <- merge(WBHousing_Data_2011, SDG_Housing_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGFCF_Data_2004 <- merge(WBGFCF_Data_2004, SDG_GFCF_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGFCF_Data_2007 <- merge(WBGFCF_Data_2007, SDG_GFCF_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGFCF_Data_2011 <- merge(WBGFCF_Data_2011, SDG_GFCF_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGoods_Data_2004 <- merge(WBGoods_Data_2004, SDG_Goods_Data_2004[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGoods_Data_2007 <- merge(WBGoods_Data_2007, SDG_Goods_Data_2007[,-2], by.x = "country", by.y = "geoAreaName")
WBSDGGoods_Data_2011 <- merge(WBGoods_Data_2011, SDG_Goods_Data_2011[,-2], by.x = "country", by.y = "geoAreaName")

  
  
  
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
k <- 1
WBFoodData_NoNAs_2004 <- NARemove_Fun(WBFood_Data_2004, 1.2);  #remove(WBFood_Data_2004) #1.2
WBFoodData_NoNAs_2007 <- NARemove_Fun(WBFood_Data_2007, 1.2);  #remove(WBFood_Data_2007) #1.2
WBFoodData_NoNAs_2011 <- NARemove_Fun(WBFood_Data_2011, 1.2);  #remove(WBFood_Data_2011) #1.2
WBGovernmentData_NoNAs_2004 <- NARemove_Fun(WBGovernment_Data_2004, 2);  #remove(WBGovernment_Data_2004) #2
WBGovernmentData_NoNAs_2007 <- NARemove_Fun(WBGovernment_Data_2007, 2);  #remove(WBGovernment_Data_2007) #2
WBGovernmentData_NoNAs_2011 <- NARemove_Fun(WBGovernment_Data_2011, 2);  #remove(WBGovernment_Data_2011) #2
WBServicesData_NoNAs_2004 <- NARemove_Fun(WBServices_Data_2004, 1.5);  #remove(WBServices_Data_2004) #1.5
WBServicesData_NoNAs_2007 <- NARemove_Fun(WBServices_Data_2007, 1.5);  #remove(WBServices_Data_2007) #1.5
WBServicesData_NoNAs_2011 <- NARemove_Fun(WBServices_Data_2011, 1.5);  #remove(WBServices_Data_2011) #1.5
WBTransportData_NoNAs_2004 <- NARemove_Fun(WBTransport_Data_2004, 1.02);  #remove(WBTransport_Data_2004) #1.02
WBTransportData_NoNAs_2007 <- NARemove_Fun(WBTransport_Data_2007, 1.02);  #remove(WBTransport_Data_2007) #1.02
WBTransportData_NoNAs_2011 <- NARemove_Fun(WBTransport_Data_2011, 1.02);  #remove(WBTransport_Data_2011) #1.02
WBHousingData_NoNAs_2004 <- NARemove_Fun(WBHousing_Data_2004, 1.25);  #remove(WBHousing_Data_2004) #1.25
WBHousingData_NoNAs_2007 <- NARemove_Fun(WBHousing_Data_2007, 1.25);  #remove(WBHousing_Data_2007) #1.25
WBHousingData_NoNAs_2011 <- NARemove_Fun(WBHousing_Data_2011, 1.25);  #remove(WBHousing_Data_2011) #1.25
WBGFCFData_NoNAs_2004 <- NARemove_Fun(WBGFCF_Data_2004, 2);  #remove(WBGFCF_Data_2004) #2
WBGFCFData_NoNAs_2007 <- NARemove_Fun(WBGFCF_Data_2007, 2);  #remove(WBGFCF_Data_2007) #2
WBGFCFData_NoNAs_2011 <- NARemove_Fun(WBGFCF_Data_2011, 2);  #remove(WBGFCF_Data_2011) #2
WBGoodsData_NoNAs_2004 <- NARemove_Fun(WBGoods_Data_2004, 1.02); #remove(WBGoods_Data_2004) #1.02
WBGoodsData_NoNAs_2007 <- NARemove_Fun(WBGoods_Data_2007, 1.02); #remove(WBGoods_Data_2007) #1.02
WBGoodsData_NoNAs_2011 <- NARemove_Fun(WBGoods_Data_2011, 1.02); #remove(WBGoods_Data_2011) #1.02

WBIndicatorsDownloadedNoNAs <- WBIndicatorsDownloaded[WBIndicatorsDownloaded$indicator %in%
        unique(c(colnames(WBFoodData_NoNAs_2004[-(1:2)]),
                 colnames(WBFoodData_NoNAs_2007[-(1:2)]), colnames(WBFoodData_NoNAs_2011[-(1:2)]),
                 colnames(WBGovernmentData_NoNAs_2004[-(1:2)]),      colnames(WBGovernmentData_NoNAs_2007[-(1:2)]),
                 colnames(WBGovernmentData_NoNAs_2011[-(1:2)]),      colnames(WBServicesData_NoNAs_2004[-(1:2)]),
                 colnames(WBServicesData_NoNAs_2007[-(1:2)]),      colnames(WBServicesData_NoNAs_2011[-(1:2)]),
                 colnames(WBTransportData_NoNAs_2004[-(1:2)]),      colnames(WBTransportData_NoNAs_2007[-(1:2)]),
                 colnames(WBTransportData_NoNAs_2011[-(1:2)]),      colnames(WBHousingData_NoNAs_2004[-(1:2)]),
                 colnames(WBHousingData_NoNAs_2007[-(1:2)]),      colnames(WBHousingData_NoNAs_2011[-(1:2)]),
                 colnames(WBGFCFData_NoNAs_2004[-(1:2)]),      colnames(WBGFCFData_NoNAs_2007[-(1:2)]),
                 colnames(WBGFCFData_NoNAs_2011[-(1:2)]), colnames(WBGoodsData_NoNAs_2004[-(1:2)]),
                 colnames(WBGoodsData_NoNAs_2007[-(1:2)]), colnames(WBGoodsData_NoNAs_2011[-(1:2)]))),]

write.csv(WBIndicatorsDownloadedNoNAs, "./WBIndicatorsDownloaded-Included.csv")

# Drop indicators that have more than 1/NA factor proportion of NAs for SDG data
SDGFoodData_NoNAs_2004 <- NARemove_Fun(SDG_Food_Data_2004, 1.05) #; remove(SDG_Food_Data_2004)
SDGFoodData_NoNAs_2007 <- NARemove_Fun(SDG_Food_Data_2007, 1.05)#; remove(SDG_Food_Data_2007)
SDGFoodData_NoNAs_2011 <- NARemove_Fun(SDG_Food_Data_2011, 1.05)#; remove(SDG_Food_Data_2011)
SDGGovernmentData_NoNAs_2004 <- NARemove_Fun(SDG_Government_Data_2004, 1.5)#; remove(SDG_Government_Data_2004)
SDGGovernmentData_NoNAs_2007 <- NARemove_Fun(SDG_Government_Data_2007, 1.5)#; remove(SDG_Government_Data_2007)
SDGGovernmentData_NoNAs_2011 <- NARemove_Fun(SDG_Government_Data_2011, 1.5)#; remove(SDG_Government_Data_2011)
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

write.csv(SDGIndicatorsDownloadedNoNAs, "./SDGIndicatorsDownloaded-Included.csv")
#Create a min-max range version of all remaining data to normalize btw 0 and 1, then aggregate with Averaging
####Max/Min function calculation####

WBSDGFoodData_NoNAs_2004 <- NARemove_Fun(WBSDGFood_Data_2004, 1.2);  remove(WBSDGFood_Data_2004) #1.2
WBSDGFoodData_NoNAs_2007 <- NARemove_Fun(WBSDGFood_Data_2007, 1.2);  remove(WBSDGFood_Data_2007) #1.2
WBSDGFoodData_NoNAs_2011 <- NARemove_Fun(WBSDGFood_Data_2011, 1.2);  remove(WBSDGFood_Data_2011) #1.2
WBSDGGovernmentData_NoNAs_2004 <- NARemove_Fun(WBSDGGovernment_Data_2004, 2);  remove(WBSDGGovernment_Data_2004) #2
WBSDGGovernmentData_NoNAs_2007 <- NARemove_Fun(WBSDGGovernment_Data_2007, 2);  remove(WBSDGGovernment_Data_2007) #2
WBSDGGovernmentData_NoNAs_2011 <- NARemove_Fun(WBSDGGovernment_Data_2011, 2);  remove(WBSDGGovernment_Data_2011) #2
WBSDGServicesData_NoNAs_2004 <- NARemove_Fun(WBSDGServices_Data_2004, 1.5);  remove(WBSDGServices_Data_2004) #1.5
WBSDGServicesData_NoNAs_2007 <- NARemove_Fun(WBSDGServices_Data_2007, 1.5);  remove(WBSDGServices_Data_2007) #1.5
WBSDGServicesData_NoNAs_2011 <- NARemove_Fun(WBSDGServices_Data_2011, 1.5);  remove(WBSDGServices_Data_2011) #1.5
WBSDGTransportData_NoNAs_2004 <- NARemove_Fun(WBSDGTransport_Data_2004, 1.02);  remove(WBSDGTransport_Data_2004) #1.02
WBSDGTransportData_NoNAs_2007 <- NARemove_Fun(WBSDGTransport_Data_2007, 1.02);  remove(WBSDGTransport_Data_2007) #1.02
WBSDGTransportData_NoNAs_2011 <- NARemove_Fun(WBSDGTransport_Data_2011, 1.02);  remove(WBSDGTransport_Data_2011) #1.02
WBSDGHousingData_NoNAs_2004 <- NARemove_Fun(WBSDGHousing_Data_2004, 1.25);  remove(WBSDGHousing_Data_2004) #1.25
WBSDGHousingData_NoNAs_2007 <- NARemove_Fun(WBSDGHousing_Data_2007, 1.25);  remove(WBSDGHousing_Data_2007) #1.25
WBSDGHousingData_NoNAs_2011 <- NARemove_Fun(WBSDGHousing_Data_2011, 1.25);  remove(WBSDGHousing_Data_2011) #1.25
WBSDGGFCFData_NoNAs_2004 <- NARemove_Fun(WBSDGGFCF_Data_2004, 2);  remove(WBSDGGFCF_Data_2004) #2
WBSDGGFCFData_NoNAs_2007 <- NARemove_Fun(WBSDGGFCF_Data_2007, 2);  remove(WBSDGGFCF_Data_2007) #2
WBSDGGFCFData_NoNAs_2011 <- NARemove_Fun(WBSDGGFCF_Data_2011, 2);  remove(WBSDGGFCF_Data_2011) #2
WBSDGGoodsData_NoNAs_2004 <- NARemove_Fun(WBSDGGoods_Data_2004, 1.02); remove(WBSDGGoods_Data_2004) #1.02
WBSDGGoodsData_NoNAs_2007 <- NARemove_Fun(WBSDGGoods_Data_2007, 1.02); remove(WBSDGGoods_Data_2007) #1.02
WBSDGGoodsData_NoNAs_2011 <- NARemove_Fun(WBSDGGoods_Data_2011, 1.02); remove(WBSDGGoods_Data_2011) #1.02

WBSDGIndicatorsDownloaded <- rbind(WBIndicatorsDownloaded, SDGIndicatorsDownloaded)
WBSDGIndicatorsDownloadedNoNAs <- WBSDGIndicatorsDownloaded[WBSDGIndicatorsDownloaded$indicator %in%
      unique(c(colnames(WBSDGFoodData_NoNAs_2004[-(1:2)]),
               colnames(WBSDGFoodData_NoNAs_2007[-(1:2)]), colnames(WBSDGFoodData_NoNAs_2011[-(1:2)]),
               colnames(WBSDGGovernmentData_NoNAs_2004[-(1:2)]),      colnames(WBSDGGovernmentData_NoNAs_2007[-(1:2)]),
               colnames(WBSDGGovernmentData_NoNAs_2011[-(1:2)]),      colnames(WBSDGServicesData_NoNAs_2004[-(1:2)]),
               colnames(WBSDGServicesData_NoNAs_2007[-(1:2)]),      colnames(WBSDGServicesData_NoNAs_2011[-(1:2)]),
               colnames(WBSDGTransportData_NoNAs_2004[-(1:2)]),      colnames(WBSDGTransportData_NoNAs_2007[-(1:2)]),
               colnames(WBSDGTransportData_NoNAs_2011[-(1:2)]),      colnames(WBSDGHousingData_NoNAs_2004[-(1:2)]),
               colnames(WBSDGHousingData_NoNAs_2007[-(1:2)]),      colnames(WBSDGHousingData_NoNAs_2011[-(1:2)]),
               colnames(WBSDGGFCFData_NoNAs_2004[-(1:2)]),      colnames(WBSDGGFCFData_NoNAs_2007[-(1:2)]),
               colnames(WBSDGGFCFData_NoNAs_2011[-(1:2)]), colnames(WBSDGGoodsData_NoNAs_2004[-(1:2)]),
               colnames(WBSDGGoodsData_NoNAs_2007[-(1:2)]), colnames(WBSDGGoodsData_NoNAs_2011[-(1:2)]))),]

write.csv(WBSDGIndicatorsDownloadedNoNAs, "./WBSDGIndicatorsDownloaded-Included.csv")


MaxMin_Fun <- function(data, category){
  colnames_important <- as.data.frame(data[,-c(1:2)])
  datamatrix <- as.data.frame(matrix(ncol = ncol(colnames_important), nrow = nrow(data)))
  for(i in 1:ncol(colnames_important)){
    datamatrix[,i] <- data[,i+2]/max(data[,i+2], na.rm = TRUE)
  }
  colnames(datamatrix) <- colnames(colnames_important)
  datamatrix$MaxMin_Index <- rowMeans(datamatrix, na.rm = TRUE)
  #colnames(datamatrix)[ncol(datamatrix)] <- "MaxMin_Index"
  datamatrix$CLUM_category <- category
  datamatrix$NApercent <- (rowSums(is.na(datamatrix))/ncol(colnames_important))*100
  datamatrix <- cbind(as.data.frame(data[,c(1:2)]), datamatrix[,(ncol(datamatrix)-2):ncol(datamatrix)])
  # Redundant datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-2):ncol(datamatrix))]
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
WBTransportData_MaxMin <- rbind(WBTransportData_MaxMin_2004,WBTransportData_MaxMin_2007,
                                if(length(WBTransportData_NoNAs_2011)>2){WBTransportData_MaxMin_2011})
remove(WBTransportData_MaxMin_2004,WBTransportData_MaxMin_2007, WBTransportData_MaxMin_2011)
try(WBGFCFData_MaxMin_2004 <- MaxMin_Fun(WBGFCFData_NoNAs_2004, "Gross Fixed Capital Formation"))
WBGFCFData_MaxMin_2007 <- MaxMin_Fun(WBGFCFData_NoNAs_2007, "Gross Fixed Capital Formation")
WBGFCFData_MaxMin_2011 <- MaxMin_Fun(WBGFCFData_NoNAs_2011, "Gross Fixed Capital Formation")
WBGFCFData_MaxMin <- rbind(if(length(WBGFCFData_NoNAs_2004)>2){WBGFCFData_MaxMin_2004},
                           WBGFCFData_MaxMin_2007,WBGFCFData_MaxMin_2011)
WBHousingData_MaxMin_2004 <- MaxMin_Fun(WBHousingData_NoNAs_2004, "Housing")
WBHousingData_MaxMin_2007 <- MaxMin_Fun(WBHousingData_NoNAs_2007, "Housing")
WBHousingData_MaxMin_2011 <- MaxMin_Fun(WBHousingData_NoNAs_2011, "Housing")
WBHousingData_MaxMin <- rbind(WBHousingData_MaxMin_2004,WBHousingData_MaxMin_2007,WBHousingData_MaxMin_2011)
remove(WBHousingData_MaxMin_2004,WBHousingData_MaxMin_2007,WBHousingData_MaxMin_2011)
WBGoodsData_MaxMin_2004 <- MaxMin_Fun(WBGoodsData_NoNAs_2004, "Goods")
WBGoodsData_MaxMin_2007 <- MaxMin_Fun(WBGoodsData_NoNAs_2007, "Goods")
WBGoodsData_MaxMin_2011 <- MaxMin_Fun(WBGoodsData_NoNAs_2011, "Goods")
WBGoodsData_MaxMin <- rbind(WBGoodsData_MaxMin_2004,WBGoodsData_MaxMin_2007,WBGoodsData_MaxMin_2011)
remove(WBGoodsData_MaxMin_2004,WBGoodsData_MaxMin_2007,WBGoodsData_MaxMin_2011)

#Single column version
######  Transport if there is only 1 column (eg. if the NA_Factor for Transport is 2)
# colnames(TransportData_NoNAs) <- c("iso2c", "country", "MaxMin_Index", "year")
# TransportData_NoNAs$MaxMin_Index <- TransportData_NoNAs$MaxMin_Index/max(TransportData_NoNAs$MaxMin_Index, na.rm=TRUE)
# TransportData_NoNAs$CLUM_category <- "Personal Transportation"
# TransportData_NoNAs$NAPercent <- (rowSums(is.na(TransportData_NoNAs))/max(rowSums(is.na(TransportData_NoNAs))))*100
# TransportData_MaxMin <- TransportData_NoNAs[,c(2:6)]

##For WB Housing data, only one column and already 0 to 100
# WBHousingData_MaxMin <- rbind(WBHousingData_NoNAs_2004,WBHousingData_NoNAs_2007,WBHousingData_NoNAs_2011)
# colnames(WBHousingData_MaxMin) <- c("country", "year", "MaxMin_Index")
# WBHousingData_MaxMin$MaxMin_Index <- WBHousingData_MaxMin$MaxMin_Index/100
# WBHousingData_MaxMin$CLUM_category <- "Housing"
# WBHousingData_MaxMin$NAPercent <- (rowSums(is.na(WBHousingData_MaxMin))/max(rowSums(is.na(WBHousingData_MaxMin))))*100
##For Goods Data, just rename column
# WBGoodsData_MaxMin <- WBGoods_Data
# colnames(WBGoodsData_MaxMin) <- c("country", "year", "MaxMin_Index")
# WBGoodsData_MaxMin$MaxMin_Index <- WBGoodsData_MaxMin$MaxMin_Index/
#   max(WBGoodsData_MaxMin$MaxMin_Index, na.rm = TRUE)
# WBGoodsData_MaxMin$CLUM_category <- "Goods"
# WBGoodsData_MaxMin$NAPercent <- (rowSums(is.na(WBGoods_Data))/max(rowSums(is.na(WBGoods_Data))))*100

##Binding Data together for single spreadsheet
WBMaxMinData <- rbind(WBFoodData_MaxMin, WBGovernmentData_MaxMin, WBServicesData_MaxMin, 
                      WBTransportData_MaxMin, WBGFCFData_MaxMin, WBHousingData_MaxMin, WBGoodsData_MaxMin)
colnames(WBMaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NApercent")

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

SDGMaxMinData <- rbind(SDGFoodData_MaxMin, SDGGovernmentData_MaxMin, SDGServicesData_MaxMin, 
                       SDGTransportData_MaxMin, SDGHousingData_MaxMin, SDGGoods_Data_MaxMin, 
                       SDGGFCF_Data_MaxMin)
colnames(SDGMaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NApercent")


WBSDGFoodData_MaxMin_2004 <- MaxMin_Fun(WBSDGFoodData_NoNAs_2004, "Food")
WBSDGFoodData_MaxMin_2007 <- MaxMin_Fun(WBSDGFoodData_NoNAs_2007, "Food")
WBSDGFoodData_MaxMin_2011 <- MaxMin_Fun(WBSDGFoodData_NoNAs_2011, "Food")
WBSDGFoodData_MaxMin <- rbind(WBSDGFoodData_MaxMin_2004,WBSDGFoodData_MaxMin_2007, WBSDGFoodData_MaxMin_2011)
remove(WBSDGFoodData_MaxMin_2004,WBSDGFoodData_MaxMin_2007,WBSDGFoodData_MaxMin_2011)
WBSDGGovernmentData_MaxMin_2004 <- MaxMin_Fun(WBSDGGovernmentData_NoNAs_2004, "Government")
WBSDGGovernmentData_MaxMin_2007 <- MaxMin_Fun(WBSDGGovernmentData_NoNAs_2007, "Government")
WBSDGGovernmentData_MaxMin_2011 <- MaxMin_Fun(WBSDGGovernmentData_NoNAs_2011, "Government")
WBSDGGovernmentData_MaxMin <- rbind(WBSDGGovernmentData_MaxMin_2004,WBSDGGovernmentData_MaxMin_2007, WBSDGGovernmentData_MaxMin_2011)
remove(WBSDGGovernmentData_MaxMin_2004,WBSDGGovernmentData_MaxMin_2007,WBSDGGovernmentData_MaxMin_2011)
WBSDGServicesData_MaxMin_2004 <- MaxMin_Fun(WBSDGServicesData_NoNAs_2004, "Services")
WBSDGServicesData_MaxMin_2007 <- MaxMin_Fun(WBSDGServicesData_NoNAs_2007, "Services")
WBSDGServicesData_MaxMin_2011 <- MaxMin_Fun(WBSDGServicesData_NoNAs_2011, "Services")
WBSDGServicesData_MaxMin <- rbind(WBSDGServicesData_MaxMin_2004,WBSDGServicesData_MaxMin_2007,WBSDGServicesData_MaxMin_2011)
remove(WBSDGServicesData_MaxMin_2004,WBSDGServicesData_MaxMin_2007,WBSDGServicesData_MaxMin_2011)
WBSDGTransportData_MaxMin_2004 <- MaxMin_Fun(WBSDGTransportData_NoNAs_2004, "Personal Transportation")
WBSDGTransportData_MaxMin_2007 <- MaxMin_Fun(WBSDGTransportData_NoNAs_2007, "Personal Transportation")
WBSDGTransportData_MaxMin_2011 <- MaxMin_Fun(WBSDGTransportData_NoNAs_2011, "Personal Transportation")
WBSDGTransportData_MaxMin <- rbind(WBSDGTransportData_MaxMin_2004, WBSDGTransportData_MaxMin_2007, WBSDGTransportData_MaxMin_2011)
remove(WBSDGTransportData_MaxMin_2004,WBSDGTransportData_MaxMin_2007, WBSDGTransportData_MaxMin_2011)
WBSDGHousingData_MaxMin_2004 <- MaxMin_Fun(WBSDGHousingData_NoNAs_2004, "Housing")
WBSDGHousingData_MaxMin_2007 <- MaxMin_Fun(WBSDGHousingData_NoNAs_2007, "Housing")
WBSDGHousingData_MaxMin_2011 <- MaxMin_Fun(WBSDGHousingData_NoNAs_2011, "Housing")
WBSDGHousingData_MaxMin <- rbind(WBSDGHousingData_MaxMin_2004, WBSDGHousingData_MaxMin_2007, WBSDGHousingData_MaxMin_2011)
remove(WBSDGHousingData_MaxMin_2004,WBSDGHousingData_MaxMin_2007, WBSDGHousingData_MaxMin_2011)
WBSDGGoodsData_MaxMin_2004 <- MaxMin_Fun(WBSDGGoodsData_NoNAs_2004, "Goods")
WBSDGGoodsData_MaxMin_2007 <- MaxMin_Fun(WBSDGGoodsData_NoNAs_2007, "Goods")
WBSDGGoodsData_MaxMin_2011 <- MaxMin_Fun(WBSDGGoodsData_NoNAs_2011, "Goods")
WBSDGGoodsData_MaxMin <- rbind(WBSDGGoodsData_MaxMin_2004,WBSDGGoodsData_MaxMin_2007, WBSDGGoodsData_MaxMin_2011)
remove(WBSDGGoodsData_MaxMin_2004,WBSDGGoodsData_MaxMin_2007, WBSDGGoodsData_MaxMin_2011)
WBSDGGFCFData_MaxMin_2004 <- MaxMin_Fun(WBSDGGFCFData_NoNAs_2004, "Gross Fixed Capital Formation")
WBSDGGFCFData_MaxMin_2007 <- MaxMin_Fun(WBSDGGFCFData_NoNAs_2007, "Gross Fixed Capital Formation")
WBSDGGFCFData_MaxMin_2011 <- MaxMin_Fun(WBSDGGFCFData_NoNAs_2011, "Gross Fixed Capital Formation")
WBSDGGFCFData_MaxMin <- rbind(WBSDGGFCFData_MaxMin_2004,WBSDGGFCFData_MaxMin_2007, WBSDGGFCFData_MaxMin_2011)
remove(WBSDGGFCFData_MaxMin_2004,WBSDGGFCFData_MaxMin_2007, WBSDGGFCFData_MaxMin_2011)

WBSDGMaxMinData <- rbind(WBSDGFoodData_MaxMin, WBSDGGovernmentData_MaxMin, WBSDGServicesData_MaxMin, 
                       try(WBSDGTransportData_MaxMin), WBSDGHousingData_MaxMin, WBSDGGoodsData_MaxMin, 
                       WBSDGGFCFData_MaxMin)
colnames(WBSDGMaxMinData) <- c("country", "year", "MaxMin_Index", "CLUM_category", "NApercent")

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
  datamatrix$ZScore_Index <- rowMeans(datamatrix, na.rm = TRUE)
  #Redundant: colnames(datamatrix)[ncol(datamatrix)] <- "ZScore_Index"
  datamatrix$CLUM_category <- category
  datamatrix$NApercent <- (rowSums(is.na(datamatrix))/ncol(colnames_important))*100
  datamatrix <- cbind(as.data.frame(data[,c(1:2)]), datamatrix[,(ncol(datamatrix)-2):ncol(datamatrix)])
  # Redundant datamatrix <- datamatrix[,c(1:2,(ncol(datamatrix)-1):ncol(datamatrix))]
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
try(WBTransportData_ZScore_2011 <- ZScore_Fun(WBTransportData_NoNAs_2011, "Personal Transportation"))
WBTransportData_ZScore <- rbind(WBTransportData_ZScore_2004, WBTransportData_ZScore_2007, 
                                if(length(WBTransportData_NoNAs_2011)>2){WBTransportData_ZScore_2011})
remove(WBTransportData_ZScore_2004, WBTransportData_ZScore_2007, WBTransportData_ZScore_2011)
WBServicesData_ZScore_2004 <- ZScore_Fun(WBServicesData_NoNAs_2004, "Services")
WBServicesData_ZScore_2007 <- ZScore_Fun(WBServicesData_NoNAs_2007, "Services")
WBServicesData_ZScore_2011 <- ZScore_Fun(WBServicesData_NoNAs_2011, "Services")
WBServicesData_ZScore <- rbind(WBServicesData_ZScore_2004, WBServicesData_ZScore_2007, WBServicesData_ZScore_2011)
remove(WBServicesData_ZScore_2004, WBServicesData_ZScore_2007, WBServicesData_ZScore_2011)
try(WBGFCFData_ZScore_2004 <- ZScore_Fun(WBGFCFData_NoNAs_2004, "Gross Fixed Capital Formation"))
WBGFCFData_ZScore_2007 <- ZScore_Fun(WBGFCFData_NoNAs_2007, "Gross Fixed Capital Formation")
WBGFCFData_ZScore_2011 <- ZScore_Fun(WBGFCFData_NoNAs_2011, "Gross Fixed Capital Formation")
WBGFCFData_ZScore <- rbind(if(length(WBGFCFData_NoNAs_2004)>2){WBGFCFData_ZScore_2004},
                           WBGFCFData_ZScore_2007, WBGFCFData_ZScore_2011)
WBHousingData_ZScore_2004 <- ZScore_Fun(WBHousingData_NoNAs_2004, "Housing")
WBHousingData_ZScore_2007 <- ZScore_Fun(WBHousingData_NoNAs_2007, "Housing")
WBHousingData_ZScore_2011 <- ZScore_Fun(WBHousingData_NoNAs_2011, "Housing")
WBHousingData_ZScore <- rbind(WBHousingData_ZScore_2004, WBHousingData_ZScore_2007, WBHousingData_ZScore_2011)
remove(WBHousingData_ZScore_2004, WBHousingData_ZScore_2007, WBHousingData_ZScore_2011)
WBGoodsData_ZScore_2004 <- ZScore_Fun(WBGoodsData_NoNAs_2004, "Goods")
WBGoodsData_ZScore_2007 <- ZScore_Fun(WBGoodsData_NoNAs_2007, "Goods")
WBGoodsData_ZScore_2011 <- ZScore_Fun(WBGoodsData_NoNAs_2011, "Goods")
WBGoodsData_ZScore <- rbind(WBGoodsData_ZScore_2004, WBGoodsData_ZScore_2007, WBGoodsData_ZScore_2011)
remove(WBGoodsData_ZScore_2004, WBGoodsData_ZScore_2007, WBGoodsData_ZScore_2011)



##For Housing data, only one column and already 0 to 1
###HousingData_ZScore <- ZScore_Fun(HousingData_NoNAs, "Housing")
# WBZScore_Index <- scale(WBHousingData_MaxMin$MaxMin_Index)
# WBHousingData_ZScore <- cbind(WBHousingData_MaxMin[,c(1,2)], WBZScore_Index, WBHousingData_MaxMin[,4])
# colnames(WBHousingData_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")
# ##For Goods Data, just rename column
# WBZScore_Index <- scale(WBGoods_Data_MaxMin$MaxMin_Index)
# WBGoods_Data_ZScore <- cbind(WBGoods_Data_MaxMin[,c(1:2)], WBZScore_Index,WBGoods_Data_MaxMin[,4])
# colnames(WBGoods_Data_ZScore) <- c("country", "year", "ZScore_Index", "CLUM_category")


##Binding Data together for single spreadsheet
WBZScoreData <- rbind(WBFoodData_ZScore, WBGovernmentData_ZScore, WBServicesData_ZScore, 
                      WBTransportData_ZScore, WBHousingData_ZScore, WBGFCFData_ZScore, WBGoodsData_ZScore)
#colnames(WBZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category")

##Combining MaxMin and Z-score datasets
WBIndicesData <- merge(WBZScoreData, WBMaxMinData, by.x = c("country", "year", "CLUM_category", "NApercent"),
                       by.y = c("country", "year", "CLUM_category", "NApercent"),
                       all=TRUE)

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
colnames(SDGZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category", "NApercent")

##Combining MaxMin and Z-score datasets
SDGIndicesData <- merge(SDGZScoreData, SDGMaxMinData, by.x = c("country", "year", "CLUM_category", "NApercent"),
                        by.y = c("country", "year", "CLUM_category", "NApercent"),
                        all=TRUE) 

write.csv(SDGIndicesData, "./IndicesDataSDG.csv")

WBSDGFoodData_ZScore_2004 <- ZScore_Fun(WBSDGFoodData_NoNAs_2004, "Food")
WBSDGFoodData_ZScore_2007 <- ZScore_Fun(WBSDGFoodData_NoNAs_2007, "Food")
WBSDGFoodData_ZScore_2011 <- ZScore_Fun(WBSDGFoodData_NoNAs_2011, "Food")
WBSDGFoodData_ZScore <- rbind(WBSDGFoodData_ZScore_2004, WBSDGFoodData_ZScore_2007, WBSDGFoodData_ZScore_2011)
remove(WBSDGFoodData_ZScore_2004, WBSDGFoodData_ZScore_2007, WBSDGFoodData_ZScore_2011)
WBSDGGovernmentData_ZScore_2004 <- ZScore_Fun(WBSDGGovernmentData_NoNAs_2004, "Government")
WBSDGGovernmentData_ZScore_2007 <- ZScore_Fun(WBSDGGovernmentData_NoNAs_2007, "Government")
WBSDGGovernmentData_ZScore_2011 <- ZScore_Fun(WBSDGGovernmentData_NoNAs_2011, "Government")
WBSDGGovernmentData_ZScore <- rbind(WBSDGGovernmentData_ZScore_2004, WBSDGGovernmentData_ZScore_2007, WBSDGGovernmentData_ZScore_2011)
remove(WBSDGGovernmentData_ZScore_2004, WBSDGGovernmentData_ZScore_2007, WBSDGGovernmentData_ZScore_2011)
# Temp fix for no data in 2004 or 2007 as of 12/22/2020
WBSDGTransportData_ZScore_2004 <- ZScore_Fun(WBSDGTransportData_NoNAs_2004, "Personal Transportation")
WBSDGTransportData_ZScore_2007 <- ZScore_Fun(WBSDGTransportData_NoNAs_2007, "Personal Transportation")
WBSDGTransportData_ZScore_2011 <- ZScore_Fun(WBSDGTransportData_NoNAs_2011, "Personal Transportation")
WBSDGTransportData_ZScore <- rbind(WBSDGTransportData_ZScore_2004,WBSDGTransportData_ZScore_2007,WBSDGTransportData_ZScore_2011)
remove(WBSDGTransportData_ZScore_2004, WBSDGTransportData_ZScore_2007, WBSDGTransportData_ZScore_2011)
WBSDGServicesData_ZScore_2004 <- ZScore_Fun(WBSDGServicesData_NoNAs_2004, "Services")
WBSDGServicesData_ZScore_2007 <- ZScore_Fun(WBSDGServicesData_NoNAs_2007, "Services")
WBSDGServicesData_ZScore_2011 <- ZScore_Fun(WBSDGServicesData_NoNAs_2011, "Services")
WBSDGServicesData_ZScore <- rbind(WBSDGServicesData_ZScore_2004, WBSDGServicesData_ZScore_2007, WBSDGServicesData_ZScore_2011)
remove(WBSDGServicesData_ZScore_2004, WBSDGServicesData_ZScore_2007, WBSDGServicesData_ZScore_2011)
WBSDGHousingData_ZScore_2004 <- ZScore_Fun(WBSDGHousingData_NoNAs_2004, "Housing") # No data
WBSDGHousingData_ZScore_2007 <- ZScore_Fun(WBSDGHousingData_NoNAs_2007, "Housing")
WBSDGHousingData_ZScore_2011 <- ZScore_Fun(WBSDGHousingData_NoNAs_2011, "Housing")
WBSDGHousingData_ZScore <- rbind(WBSDGHousingData_ZScore_2004, WBSDGHousingData_ZScore_2007, WBSDGHousingData_ZScore_2011)
remove(WBSDGHousingData_ZScore_2004, WBSDGHousingData_ZScore_2007, WBSDGHousingData_ZScore_2011)
WBSDGGoodsData_ZScore_2004 <- ZScore_Fun(WBSDGGoodsData_NoNAs_2004, "Goods")
WBSDGGoodsData_ZScore_2007 <- ZScore_Fun(WBSDGGoodsData_NoNAs_2007, "Goods")
WBSDGGoodsData_ZScore_2011 <- ZScore_Fun(WBSDGGoodsData_NoNAs_2011, "Goods")
WBSDGGoodsData_ZScore <- rbind(WBSDGGoodsData_ZScore_2004, WBSDGGoodsData_ZScore_2007, WBSDGGoodsData_ZScore_2011)
remove(WBSDGGoodsData_ZScore_2004, WBSDGGoodsData_ZScore_2007, WBSDGGoodsData_ZScore_2011)
WBSDGGFCFData_ZScore_2004 <- ZScore_Fun(WBSDGGFCFData_NoNAs_2004, "Gross Fixed Capital Formation")
WBSDGGFCFData_ZScore_2007 <- ZScore_Fun(WBSDGGFCFData_NoNAs_2007, "Gross Fixed Capital Formation")
WBSDGGFCFData_ZScore_2011 <- ZScore_Fun(WBSDGGFCFData_NoNAs_2011, "Gross Fixed Capital Formation")
WBSDGGFCFData_ZScore <- rbind(WBSDGGFCFData_ZScore_2004, WBSDGGFCFData_ZScore_2007, WBSDGGFCFData_ZScore_2011)
remove(WBSDGGFCFData_ZScore_2004, WBSDGGFCFData_ZScore_2007, WBSDGGFCFData_ZScore_2011)

##Binding Data together for single spreadsheet
WBSDGZScoreData <- rbind(WBSDGFoodData_ZScore, WBSDGGovernmentData_ZScore, WBSDGServicesData_ZScore, 
                       try(WBSDGTransportData_ZScore), WBSDGHousingData_ZScore, WBSDGGFCFData_ZScore, WBSDGGoodsData_ZScore)
colnames(WBSDGZScoreData) <- c("country", "year", "ZScore_Index", "CLUM_category", "NApercent")

##Combining MaxMin and Z-score datasets
WBSDGIndicesData <- merge(WBSDGZScoreData, WBSDGMaxMinData, by.x = c("country", "year", "CLUM_category", "NApercent"),
                          by.y = c("country", "year", "CLUM_category", "NApercent"),
                          all=TRUE)

write.csv(WBSDGIndicesData, "./IndicesDataWBSDG.csv")


cat('Looks good, Run 2.Country Correspondence to make sure all countries and groupings were dealt with\n 
    and do the GTAP weighted aggregation')

proc.time() - ptm
