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
all_countries <- c(eAfr_vec, mBasCwaLat_vec, cWaEtc_vec,
                   group4_vec, wEurChin_vec, sAsia_vec)

# Define direct and indirect yield shocks for best and worst case WSR scenarios
# Best case
# Indirect, i.e. fungicide price increase
pctCostIncBest_eAfr <- 5
pctCostIncBest_mBasCwaLat <- 0
pctCostIncBest_cWaEtc <- 15
pctCostIncBest_group4 <- 10
pctCostIncBest_wEurCh <- 15
pctCostIncBest_sAsia <- 10
#---
# pctCostIncBest_eAfr <- 1
# pctCostIncBest_mBasCwaLat <- 0
# pctCostIncBest_cWaEtc <- 3
# pctCostIncBest_group4 <- 2
# pctCostIncBest_wEurCh <- 3
# pctCostIncBest_sAsia <- 2
#---
# pctCostIncBest_eAfr <- 1
# pctCostIncBest_mBasCwaLat <- 0
# pctCostIncBest_cWaEtc <- 2
# pctCostIncBest_group4 <- 2
# pctCostIncBest_wEurCh <- 2
# pctCostIncBest_sAsia <- 2
# Direct yield loss (as percentage) due to WSR outbreaks
pctYdShkBest_eAfr <- -10
pctYdShkBest_mBasCwaLat <- -5
pctYdShkBest_cWaEtc <- 0
pctYdShkBest_group4 <- -5
pctYdShkBest_wEurCh <- 0
pctYdShkBest_sAsia <- 0
# Annualize direct yd loss
# pctYdShkBest_eAfr <- pctYdShkBest_eAfr / 3
# pctYdShkBest_mBasCwaLat <- pctYdShkBest_mBasCwaLat / 3
# pctYdShkBest_cWaEtc <- pctYdShkBest_cWaEtc / 3
# pctYdShkBest_group4 <- pctYdShkBest_group4 / 3
# pctYdShkBest_wEurCh <- pctYdShkBest_wEurCh / 3
# pctYdShkBest_sAsia <- pctYdShkBest_sAsia / 3
# Worst case
# Indirect
pctCostIncWrst_eAfr <- 2 * pctCostIncBest_eAfr
pctCostIncWrst_mBasCwaLat <- 15
pctCostIncWrst_cWaEtc <- 2 * pctCostIncBest_cWaEtc
pctCostIncWrst_group4 <- 2 * pctCostIncBest_group4
pctCostIncWrst_wEurCh <- 2 * pctCostIncBest_wEurCh
pctCostIncWrst_sAsia <- 2 * pctCostIncBest_sAsia
# Direct
pctYdShkWrst_eAfr <- 2 * pctYdShkBest_eAfr
pctYdShkWrst_mBasCwaLat <- 2 * pctYdShkBest_mBasCwaLat
pctYdShkWrst_cWaEtc <- -5
pctYdShkWrst_group4 <- 2 * pctYdShkBest_group4
pctYdShkWrst_wEurCh <- -5
pctYdShkWrst_sAsia <- -10
#===========================================================================
# Initiate yield shock data frame for IMPACT input
df_inBest <- data.frame(CountryName = all_countries,
                        Crop = "whea", CropName = "Wheat",
                        Land = "All",
                        y1 = NA, y2 = NA, y3 = NA, y4 = NA,
                        y5 = NA, y6 = NA, y7 = NA, y8 = NA, y9 = NA)
yrColNames <- c("2005-2009", "2010-2014", "2015-2019",
              "2020-2024", "2025-2029", "2030-2034", 
              "2035-2039", "2040-2044", "2045-2049")
colnames(df_inBest)[5:ncol(df_inBest)] <- yrColNames
#===========================================================================
# Merge with ISO3 3 letter country codes
this_folder <- "C:/Users/bensc/OneDrive/Documents/2 Blades/"
this_file <- "ISO3 Country key.csv"
this_filepath <- paste0(this_folder, this_file)
df_iso <- read.csv(this_filepath, stringsAsFactors = F)
colnames(df_iso)[c(1, 3)] <- c("ISO3", "CountryName")
# this_file <- "WSR IMPACT yd shock draft.csv"
# this_filepath <- paste0(this_folder, this_file)
# df_wsr <- read.csv(this_filepath, stringsAsFactors = F)
#colnames(df_wsr)
# row_rm <- which(df_wsr$CountryName == "")
# df_wsr <- df_wsr[-row_rm, ]
df <- merge(df_iso, df_inBest, by = "CountryName")#, all.y = T)
#df_iso$CountryName[grep("Belgium", df_iso$CountryName)]
df <- df[, c("WB", "ISO3", colnames(df_inBest))]
#which(duplicated(df$CountryName))
# Merge with chemical price elasticity of yield file
# This also gives you the FPU
this_file <- "PF elast IMPACT.txt"
this_filepath <- paste0(this_folder, this_file)
df_pf <- read.table(this_filepath, sep = ",", stringsAsFactors = F)
colnames(df_pf) <- c("crop", "FPU", "sys", "type", "PF")
df_pf$ISO3 <- gsub(".*_", "", df_pf$FPU)
df_pf <- subset(df_pf, crop == "jwhea" &
                  ISO3 %in% df$ISO3 &
                  type == "FERT" &
                  sys == "arf")
df_pf$sys <- NULL
df_pf$type <- NULL
df_pf$crop <- NULL
df <- merge(df, df_pf, by = "ISO3", all.x = T)
#setdiff(all_countries, unique(df$CountryName))
df$ISO3 <- NULL
df$WB <- NULL
df <- df[, c("FPU", colnames(df)[-(ncol(df) - 1)])]

ind_mBasCwaLat <- which(df$CountryName %in% mBasCwaLat_vec)
ind_eAfr <- which(df$CountryName %in% eAfr_vec)
ind_cWaEtc <- which(df$CountryName %in% cWaEtc_vec)
ind_group4 <- which(df$CountryName %in% group4_vec)
ind_wEurChin <- which(df$CountryName %in% wEurChin_vec)
ind_sAsia <- which(df$CountryName %in% sAsia_vec)

df$pctCostIncBest <- NA
df$pctCostIncBest[ind_mBasCwaLat] <- pctCostIncBest_mBasCwaLat
df$pctCostIncBest[ind_eAfr] <- pctCostIncBest_eAfr
df$pctCostIncBest[ind_cWaEtc] <- pctCostIncBest_cWaEtc
df$pctCostIncBest[ind_group4] <- pctCostIncBest_group4
df$pctCostIncBest[ind_wEurChin] <- pctCostIncBest_wEurCh
df$pctCostIncBest[ind_sAsia] <- pctCostIncBest_sAsia

df$pctCostIncWrst <- NA
df$pctCostIncWrst[ind_mBasCwaLat] <- pctCostIncWrst_mBasCwaLat
df$pctCostIncWrst[ind_eAfr] <- pctCostIncWrst_eAfr
df$pctCostIncWrst[ind_cWaEtc] <- pctCostIncWrst_cWaEtc
df$pctCostIncWrst[ind_group4] <- pctCostIncWrst_group4
df$pctCostIncWrst[ind_wEurChin] <- pctCostIncWrst_wEurCh
df$pctCostIncWrst[ind_sAsia] <- pctCostIncWrst_sAsia

df$pctYdShkBest <- NA
df$pctYdShkBest[ind_mBasCwaLat] <- pctYdShkBest_mBasCwaLat
df$pctYdShkBest[ind_eAfr] <- pctYdShkBest_eAfr
df$pctYdShkBest[ind_cWaEtc] <- pctYdShkBest_cWaEtc
df$pctYdShkBest[ind_group4] <- pctYdShkBest_group4
df$pctYdShkBest[ind_wEurChin] <- pctYdShkBest_wEurCh
df$pctYdShkBest[ind_sAsia] <- pctYdShkBest_sAsia

df$pctYdShkWrst <- NA
df$pctYdShkWrst[ind_mBasCwaLat] <- pctYdShkWrst_mBasCwaLat
df$pctYdShkWrst[ind_eAfr] <- pctYdShkWrst_eAfr
df$pctYdShkWrst[ind_cWaEtc] <- pctYdShkWrst_cWaEtc
df$pctYdShkWrst[ind_group4] <- pctYdShkWrst_group4
df$pctYdShkWrst[ind_wEurChin] <- pctYdShkWrst_wEurCh
df$pctYdShkWrst[ind_sAsia] <- pctYdShkWrst_sAsia

ydShkDir_cols <- c("2025-2029", "2035-2039", "2045-2049")
ydShkInd_cols <- c("2020-2024", "2025-2029", "2030-2034",
                   "2035-2039", "2040-2044", "2045-2049")
ydShkIndExclDir_cols <- setdiff(ydShkInd_cols, ydShkDir_cols)

# df$pctYdShkIndBest <- 100 * df$pctCostIncBest * df$PF
# df$pctYdShkIndWrst <- 100 * df$pctCostIncWrst * df$PF

ind_ydShkInd <- which(df$pctCostIncBest != 0)
ind_ydShkDir <- which(df$pctYdShkBest != 0)
ind_ydShkDirInd <- intersect(ind_ydShkDir, ind_ydShkInd)
ind_ydShkIndOnly <- setdiff(ind_ydShkInd, ind_ydShkDir)
ind_ydShkDirOnly <- setdiff(ind_ydShkDir, ind_ydShkInd)

df_best <- df
df_best[ind_ydShkInd, ydShkInd_cols] <- df_best$PF[ind_ydShkInd] * df_best$pctCostIncBest[ind_ydShkInd]
df_best[ind_ydShkDirInd, ydShkDir_cols] <- -100 * (1 - (1 + df_best[ind_ydShkDirInd, ydShkDir_cols] / 100) * (1 + df_best$pctYdShkBest[ind_ydShkDirInd] / 100))
df_best[ind_ydShkDirOnly, ydShkDir_cols] <- df_best$pctYdShkBest[ind_ydShkDirOnly]

df_wrst <- df
df_wrst[ind_ydShkInd, ydShkInd_cols] <- df_wrst$PF[ind_ydShkInd] * df_wrst$pctCostIncWrst[ind_ydShkInd]
df_wrst[ind_ydShkDirInd, ydShkDir_cols] <- -100 * (1 - (1 + df_wrst[ind_ydShkDirInd, ydShkDir_cols] / 100) * (1 + df_wrst$pctYdShkWrst[ind_ydShkDirInd] / 100))
df_wrst[ind_ydShkDirOnly, ydShkDir_cols] <- df_wrst$pctYdShkWrst[ind_ydShkDirOnly]

df_best[, ydShkInd_cols] <- (df_best[, ydShkInd_cols] + 100) / 100
df_wrst[, ydShkInd_cols] <- (df_wrst[, ydShkInd_cols] + 100) / 100

allYrInd_cols <- c("2005-2009", "2010-2014", "2015-2019", ydShkInd_cols)
n_cols <- length(allYrInd_cols)
for(i in 1:n_cols){
this_col <- df_best[, allYrInd_cols[i]]
ind_row <- which(is.na(this_col))
df_best[ind_row, allYrInd_cols[i]] <- 1

this_col <- df_wrst[, allYrInd_cols[i]]
ind_row <- which(is.na(this_col))
df_wrst[ind_row, allYrInd_cols[i]] <- 1

}

this_file <- "WSR IMPACT yd shock best.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_best, this_filepath, row.names = F)

this_file <- "WSR IMPACT yd shock worst.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_wrst, this_filepath, row.names = F)



#============================================================================
# nAfr_vec <- c("Libya", "Egypt",
#               "Morocco", "Tunisia", "Algeria")
# eAfr_vec <- eAfr_vec
# sAfr_vec <- sAfr_vec
# nAm_vec <- c("USA", "Canada")
# sAm_vec <- c("Argentina", "Brazil", "Uruguay", "Paraguay",
#              "Chile", "Peru", "Colombia", "Ecuador", "Venezuela",
#              "Bolivia")
# cAm_vec <- c("Mexico", "Honduras", "El Salvador", "Nicaragua", 
#              "Panama", "Costa Rica")
# sAsia_vec <- sAsia_vec
# #eAsia_vec <- "China"
# wAsia_vec <- c("Israel", "Lebanon", "Syria", "Iraq", "Iran", "Turkey")
# cAsia_vec <- c("Uzbekistan", "Tajikistan", "Afghanistan",
#                "Turkmenistan", "Kyrgyzstan", "Kazakhstan")
# wEur_vec <- c("France", "Germany", "Belgium-Luxembourg", "Netherlands",
#               "Austria", "Switzerland")
# nEur_vec <- c("UK", "Ireland", "Denmark")
# eEur_vec <- c("Russia", "Croatia", "Poland", "Slovenia", "Ukraine")
# sEur_vec <- c("Spain", "Italy", "Other Balkans", "Greece")
# #ausNz_vec <- c("Australia")
# 
# df_best$CountryName[which(df_best$CountryName %in% nAfr_vec)] <- "Northern Africa"
# df_best$CountryName[which(df_best$CountryName %in% sAfr_vec)] <- "Southern Africa"
# df_best$CountryName[which(df_best$CountryName %in% eAfr_vec)] <- "Eastern Africa"
# df_best$CountryName[which(df_best$CountryName %in% nAm_vec)] <- "Northern America"
# df_best$CountryName[which(df_best$CountryName %in% sAm_vec)] <- "South America"  
# df_best$CountryName[which(df_best$CountryName %in% cAm_vec)] <- "Central America"
# df_best$CountryName[which(df_best$CountryName %in% sAsia_vec)] <- "Southern Asia"
# #df_best$CountryName[which(df_best$CountryName %in% eAsia_vec)] <- "Eastern Asia"
# df_best$CountryName[which(df_best$CountryName %in% wAsia_vec)] <- "Western Asia"
# df_best$CountryName[which(df_best$CountryName %in% cAsia_vec)] <- "Central Asia"
# df_best$CountryName[which(df_best$CountryName %in% wEur_vec)] <- "Western Europe"
# df_best$CountryName[which(df_best$CountryName %in% eEur_vec)] <- "Eastern Europe"
# df_best$CountryName[which(df_best$CountryName %in% nEur_vec)] <- "Northern Europe"
# df_best$CountryName[which(df_best$CountryName %in% sEur_vec)] <- "Southern Europe"  
# 
# keep_cols <- c("CountryName",
#                "2005-2009", "2010-2014", "2015-2019", "2020-2024",
#                "2025-2029", "2030-2034", "2035-2039", "2040-2044",
#                "2045-2049")
# 
# library(tidyverse)
# df_bestOut <- df_best[, keep_cols]
# df_bestOut <- df_bestOut %>% gather(Year, Val, `2005-2009`:`2045-2049`) %>%
#   group_by(CountryName, Year) %>% summarise(Val = mean(Val, na.rm = T)) %>%
#   spread(Year, Val) %>% as.data.frame()
# 
# df_wrstOut <- df_wrst[, keep_cols]
# df_wrstOut <- df_wrstOut %>% gather(Year, Val, `2005-2009`:`2045-2049`) %>%
#   group_by(CountryName, Year) %>% summarise(Val = mean(Val, na.rm = T)) %>%
#   spread(Year, Val) %>% as.data.frame()
# 
# 
# this_file <- "WSR IMPACT yd shock best.csv"
# this_filepath <- paste0(this_folder, this_file)
# write.csv(df_bestOut, this_filepath, row.names = F)
# 
# this_file <- "WSR IMPACT yd shock worst.csv"
# this_filepath <- paste0(this_folder, this_file)
# write.csv(df_wrstOut, this_filepath, row.names = F)

#============================================================================








# ind_costInc <- which(!is.na(df$`FcidePriceInc (%)`))
# ind_ydInc <- which(!is.na(df$`2045-2049`))
# ind_noYdInc <- which(is.na(df$`2045-2049`))
# ind_costAndYdInc <- intersect(ind_ydInc, ind_costInc)
# ind_costIncOnly <- intersect(ind_noYdInc, ind_costInc)
# #intersect(ind_costAndYdInc, ind_costIncOnly)
# 
# priceInc <- df$`FcidePriceInc (%)`
# PFelastPct <- df$PF
# indirectYdShk_pct <- 100 * (priceInc * PFelastPct)
# df[ind_costIncOnly, these_cols] <- indirectYdShk_pct[ind_costIncOnly]
# df[ind_costAndYdInc, these_cols] <- -100 * indirectYdShk_pct[ind_costAndYdInc] / 100 * df[ind_costAndYdInc, these_cols] / 100
# 
# df$crop <- NULL
# df$type <- NULL
# df$PF <- NULL
# df$ISO3 <- NULL
# df$WB <- NULL
# df$`FcidePriceInc (%)` <- NULL
# 
# df <- df[, c("FPU", colnames(df)[-ncol(df)])]
# this_file <- "WSR IMPACT yd shock best.csv"
# this_filepath <- paste0(this_folder, this_file)
# write.csv(df, this_filepath, row.names = F)

# hist(df$PF[which(df$CountryName %in% latAm_vec)])
# wEurope_vec
# medBasin_vec
# latAm_vec