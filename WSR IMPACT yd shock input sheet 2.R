#===========================================================================
# Define country groupings for ease of input
eAfr_vec <- c("Ethiopia", "Kenya", "Uganda", "Tanzania")
sAfr_vec <- c("South Africa", "Zambia", "Zimbabwe")
medBasin_vec <- c("Other Balkans", "Croatia", "Egypt",
                  "Greece", "Israel", "Lebanon", "Libya",
                  "Morocco", "Slovenia", "Syria", "Tunisia", "Algeria")
#(medBasin_vec excludes W. Europe and Turkey)
wEurope_vec <- c("France", "Spain", "Italy", "Germany", "Belgium-Luxembourg",
                 "UK", "Ireland", "Netherlands", "Austria",
                 "Denmark", "Switzerland", "Poland")
sAsia_vec <- c("India", "Pakistan", "Nepal", "Bangladesh")
latAm_vec <- c("Argentina", "Brazil", "Uruguay", "Paraguay",
               "Chile", "Peru", "Colombia", "Ecuador", "Venezuela",
               "Bolivia", "Mexico", "Honduras", "El Salvador",
               "Nicaragua", "Panama", "Costa Rica")
# Define shock groups
mBasCwaLat_vec <- c(medBasin_vec, latAm_vec, "Iraq", "Uzbekistan", "Tajikistan", 
                    "Russia")
cWaEtc_vec <- c(sAfr_vec, "Afghanistan", "Turkmenistan", "Kyrgyzstan",
                "Iran", "USA", "Ukraine")
group4_vec <-c("Turkey", "Kazakhstan", "Canada", "Australia")
wEurChin_vec <-c(wEurope_vec, "China")
cWaEtcWeurChSas_vec <- c(cWaEtc_vec, wEurChin_vec, sAsia_vec, group4_vec)
all_countries <- c(eAfr_vec, mBasCwaLat_vec,
                   cWaEtcWeurChSas_vec)
# Direct yield loss (as percentage) due to WSR outbreaks
# Best case
pctYdShkBest_eAfr <- 0.10
pctYdShkBest_mBasCwaLat <- 0.05
pctYdShkBest_cWaEtcWeurChSas <- 0
# Worst case
pctYdShkWrst_eAfr <- 2 * pctYdShkBest_eAfr
pctYdShkWrst_mBasCwaLat <- 2 * pctYdShkBest_mBasCwaLat
pctYdShkWrst_cWaEtcWeurChSas <- 0.05
# Outbreak interval (yrs)
# interval_eAfr <- 5
# interval_mBasCwaLat <- 5
# interval_cWaEtcWeurChSas <- 10
# interval_group4 <- 10
interval_SSA <- 1
interval_WANA <- 3
interval_LAC <- 5 # Uruguay3-5, CWANA3
interval_EUR <- 10
interval_RUSCA <- 5
interval_NAM <- 10
interval_EA <- 10
interval_AUS <- 10
intervalVec <- c(interval_SSA, interval_WANA, interval_LAC, interval_EUR, interval_RUSCA, interval_NAM, interval_EA, interval_AUS)
intervalRegVec <- c("East & Southern Africa",
                    "West Asia & North Africa",
                    "South & Central America",
                    "Europe",
                    "Russia & Central Asia",
                    "North America",
                    "China",
                    "Australia")
#=======================================================================
# Tables summarizing expert consultation for paper
# WSR Yield shock
groupVec <- c()
groupVec <- c("East Africa",
              "Southern & Eastern Mediterranean basin (excl. Turkey), South & Central America (inc. Mexico), Iraq, Russia, Uzbekistan, Tajikistan",
              "Southern Africa, South Asia, Central Asia (excl. Uzbekistan and Tajikistan), Western & Northern Europe, Turkey, Ukraine, China, Australia, USA, Canada")
SEmedBasinVec <- c("Morroco", "Tunisia", "Algeria", "Libya", "Egypt",
                   "Israel", "Syria", "Lebanon", "Turkey", "the Balkans")
cAsiaVec <- c("Afghanistan", "Kyrgyzstan", "Kazakhstan", "Turkmenistan", "Iran", "Uzbekistan", "Tajikistan")

dfTab <- data.frame(c("Region", groupVec),
                    c("Best case",
                      pctYdShkBest_eAfr,
                      pctYdShkBest_mBasCwaLat,
                      pctYdShkBest_cWaEtcWeurChSas
                    ),
                    c("Worst case",
                      pctYdShkWrst_eAfr,
                      pctYdShkWrst_mBasCwaLat,
                      pctYdShkWrst_cWaEtcWeurChSas
                    )
                    )
colnames(dfTab) <- dfTab[1, ]
dfTab <- dfTab[-1, ]
library(flextable)
footText1 <- paste(c("Southern & Eastern Mediterranean basin", paste(SEmedBasinVec, collapse = ", ")), collapse = " = ")
footText2 <- paste(c("Southern Africa", paste(sAfr_vec, collapse = ", ")), collapse = " = ")
footText3 <- paste(c("Central Asia", paste(cAsiaVec, collapse = ", ")), collapse = " = ")
footText4 <- paste(c("East Africa", paste(eAfr_vec, collapse = ", ")), collapse = " = ")
headerText <- "Table 1: Summary of expert assessment of WSR yield shock (%)"
dfTab %>% regulartable() %>%
  add_header_row(values = headerText, colwidths = c(3)) %>%
  add_footer_row(values = footText1, colwidths = c(3)) %>%
  add_footer_row(values = footText2, colwidths = c(3)) %>%
  add_footer_row(values = footText3, colwidths = c(3)) %>%
  add_footer_row(values = footText4, colwidths = c(3)) %>%
  fontsize(size = 8, part = "footer") %>%
  line_spacing(space = 0.15, part = "footer") %>%
  autofit()
#this_folder <- "E:/BSCHIEK/2Blades WSR IMPACT/New run/"
this_folder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/WSR 2Blades/"
this_file <- "WSR impact expert summary table_ydShock.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(dfTab, this_filepath)
#------------------------------------------------------------------------
# groupVec <- c()
# groupVec[1] <- paste(eAfr_vec, collapse = ", ")
# groupVec[2] <- paste(mBasCwaLat_vec, collapse = ", ")
# groupVec[3] <- paste(cWaEtcWeurChSas_vec, collapse = ", ")
dfYdShk <- data.frame(Country = all_countries, ydShockBest = 0, ydShockWrst = 0)
u <- dfYdShk$Country
dfYdShk$ydShockBest[which(u %in% eAfr_vec)] <- 1 - pctYdShkBest_eAfr
dfYdShk$ydShockBest[which(u %in% mBasCwaLat_vec)] <- 1 - pctYdShkBest_mBasCwaLat
dfYdShk$ydShockBest[which(u %in% cWaEtcWeurChSas_vec)] <- 1 - pctYdShkBest_cWaEtcWeurChSas
dfYdShk$ydShockWrst[which(u %in% eAfr_vec)] <- 1 - pctYdShkWrst_eAfr
dfYdShk$ydShockWrst[which(u %in% mBasCwaLat_vec)] <- 1 - pctYdShkWrst_mBasCwaLat
dfYdShk$ydShockWrst[which(u %in% cWaEtcWeurChSas_vec)] <- 1 - pctYdShkWrst_cWaEtcWeurChSas
# this_file <- "WSR yd shock expert assessment.csv"
# this_filepath <- paste0(this_folder, this_file)
# write.csv(dfYdShk, this_filepath)
#------------------------------------------------------------------------
# WSR outbreak frequency
dfTab <- data.frame(c("Region", intervalRegVec),
                    c("Outbreak frequency", intervalVec))
#---
this_folder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/WSR 2Blades/"
this_file <- "WSR impact expert summary table_outbreakInterval.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(dfTab, this_filepath)
#---
colnames(dfTab) <- dfTab[1, ]
dfTab <- dfTab[-1, ]
WANA_vec <- setdiff(c(SEmedBasinVec, "Iraq"), "the Balkans")
NAM_vec <- c("Canada", "USA")
footText1 <- paste(c("West Asia & North Africa", paste(WANA_vec, collapse = ", ")), collapse = " = ")
footText2 <- paste(c("North America", paste(NAM_vec, collapse = ", ")), collapse = " = ")
headerText <- "Table 2: Summary of expert assessment of WSR outbreak frequency (every x years)"
dfTab %>% regulartable() %>%
  add_header_row(values = headerText, colwidths = c(2)) %>%
  align(j = 2, align = "center", part = "body") %>%
  add_footer_row(values = footText1, colwidths = c(2)) %>%
  add_footer_row(values = footText2, colwidths = c(2)) %>%
  fontsize(size = 8, part = "footer") %>%
  line_spacing(space = 0.15, part = "footer") %>%
  autofit()
#===========================================================================
# Get country-region key sorted out and merge with ydShk df
this_file <- "FAO-IMPACT country-region key.csv"
this_filepath <- paste0(this_folder, this_file)
dfCtyRegKey <- read.csv(this_filepath, stringsAsFactors = F)
#setdiff(all_countries, dfCtyRegKey$Country)
#sum(all_countries %in% dfCtyRegKey$Country)
# Put Ukraine in Europe
dfCtyRegKey$IMPACT[which(dfCtyRegKey$Country == "Ukraine")] <- "EUR"
# Put Mexico in North America
dfCtyRegKey$IMPACT[which(dfCtyRegKey$Country == "Mexico")] <- "NAM"
dfCtyRegKey$Region <- NA
u <- dfCtyRegKey$IMPACT
dfCtyRegKey$Region[which(u == "FSU")] <- "Russia & Central Asia"
dfCtyRegKey$Region[which(u == "EUR")] <- "Europe"
dfCtyRegKey$Region[which(u == "NAM")] <- "North America"
dfCtyRegKey$Region[which(u == "SAS")] <- "South Asia"
dfCtyRegKey$Region[which(u == "SSA")] <- "Africa South of the Sahara"
dfCtyRegKey$Region[which(u == "LAC")] <- "South America"
dfCtyRegKey$Region[which(u == "EAP")] <- "East Asia"
dfCtyRegKey$Region[which(u == "MEN")] <- "West Asia & North Africa"
dfCtyRegKey$Region[which(dfCtyRegKey$Country == "Australia")] <- "Australia"
dfYdShk <- merge(dfYdShk, dfCtyRegKey)
#===========================================================================
# Initiate yield shock data frame for IMPACT input
this_file <- "outbreakSpatDist.csv"
this_filepath <- paste0(this_folder, this_file)
dfOutbreak <- read.csv(this_filepath, stringsAsFactors = F)
dfOutbreak$X <- NULL
setdiff(dfOutbreak$Region, dfCtyRegKey$Region)
colnames(dfOutbreak) <- gsub("X", "", colnames(dfOutbreak))
dfOutbreak <- merge(dfOutbreak, dfCtyRegKey[, c("Country", "Region")])
dfOutbreak <- subset(dfOutbreak, Country %in% all_countries)
df <- merge(dfYdShk, dfOutbreak)
# setdiff(all_countries, df$Country) # 61 countries not 66 b/c South Asia left out--experts say no outbreaks there
#===========================================================================
# Get FPU
# Merge with ISO3-Country key and then merge with PF elast file to match ISO3 to FPU
#this_folder <- "C:/Users/bensc/OneDrive/Documents/2 Blades/"
this_file <- "ISO3 Country key.txt"
this_filepath <- paste0(this_folder, this_file)
df_iso <- read.table(this_filepath, sep = ",", stringsAsFactors = F)
colnames(df_iso) <- df_iso[1, ]
df_iso <- df_iso[-1, ]
colnames(df_iso)[c(1, 3)] <- c("ISO3", "CountryName")
df_iso$CountryName[which(df_iso$CountryName == "Timor LEste")] <- "Timor L'Este"
df_iso <- df_iso[-nrow(df_iso), ]
colnames(df_iso)[3] <- "Country"
setdiff(df$Country, df_iso$Country) #!!!!
df <- merge(df, df_iso, by = "Country")#, all.y = T)
#df_iso$CountryName[grep("Belgium", df_iso$CountryName)]
df <- df[, c("WB", "ISO3", colnames(df))]
#which(duplicated(df$Country))
# Merge with chemical price elasticity of yield file to get FPU
# # This also gives you the FPU
this_file <- "PF elast IMPACT.txt"
this_filepath <- paste0(this_folder, this_file)
df_pf <- read.table(this_filepath, sep = ",", stringsAsFactors = F)
colnames(df_pf) <- c("crop", "FPU", "sys", "type", "PF")
df_pf <- df_pf[-which(duplicated(df_pf$FPU)), ]
df_pf$ISO3 <- gsub(".*_", "", df_pf$FPU)
df_pf <- df_pf[, c("FPU", "ISO3")]
df <- merge(df, df_pf, by = "ISO3", all.x = T)
# #setdiff(all_countries, unique(df$CountryName))
df$ISO3 <- NULL
df$WB <- NULL
df <- df[, c("FPU", colnames(df)[-ncol(df)])]
#=================================================================
yrCols <- as.character(c(2025:2049))
keepCols <- c("FPU", "Country", "ydShockBest", yrCols)
dfBest <- df[, keepCols]
dfBest[, yrCols] <- dfBest[, yrCols] * dfBest$ydShockBest
dfBest$ydShockBest <- NULL
keepCols <- c("FPU", "Country", "ydShockWrst", yrCols)
dfWrst <- df[, keepCols]
dfWrst[, yrCols] <- dfWrst[, yrCols] * dfWrst$ydShockWrst
dfWrst$ydShockWrst <- NULL
#=================================================================
#this_folder <- "E:/BSCHIEK/IMPACT3-Model-ver3.3/InputFiles/Ben/WSR/"
this_file <- "WSR_best2.csv"
this_filepath <- paste0(this_folder, this_file)
dfBest$Country <- NULL
write.csv(dfBest, this_filepath, row.names = F)
this_file <- "WSR_worst2.csv"
this_filepath <- paste0(this_folder, this_file)
dfWrst$Country <- NULL
write.csv(dfWrst, this_filepath, row.names = F)
