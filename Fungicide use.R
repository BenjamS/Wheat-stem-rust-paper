library(tidyverse)
library(patchwork)
this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/FAO Data/"
this_file <- "Inputs_Pesticides_Use_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_fung <- read.csv(this_filepath, stringsAsFactors = F)
df_fung$Area.Code <- NULL
df_fung$Item.Code <- NULL
df_fung$Element.Code <- NULL
colnames(df_fung)[5:ncol(df_fung)] <- gsub("Y", "", colnames(df_fung)[5:ncol(df_fung)])
rm_cols <- grep("F", colnames(df_fung))
df_fung <- df_fung[, -rm_cols]
df_fung <- subset(df_fung, Item == "Fungicides and Bactericides")
df_fung <- df_fung %>% gather(Year, `Fung & bact cide`, `1990`:`2019`)
#df_fung <- df_fung[, c("Area", "Element", "Item", "Year", "Unit", "Value")]
#colnames(df_fung)[ncol(df_fung)] <- "Fung & bact cide"



this_file <- "FAO WSR crop area.csv"
this_filepath <- paste0(this_folder, this_file)
df_area <- read.csv(this_filepath, stringsAsFactors = F)
df_area <- df_area[, c("Area", "Element", "Item", "Year", "Unit", "Value")]

df_wheatArea <- subset(df_area, Item == "Wheat")
df_wheatArea <- df_wheatArea[, c("Area", "Year", "Value")]
colnames(df_wheatArea)[ncol(df_wheatArea)] <- "Wheat area"
df_totArea <- df_area %>% group_by(Area, Year) %>%
  summarise(`Tot crop area` = sum(Value, na.rm = T)) %>% as.data.frame()

df_area <- merge(df_wheatArea, df_totArea, by = c("Area", "Year"))
df <- merge(df_fung, df_area, by = c("Area", "Year"))
keep_cols <- c("Area", "Year", "Item", "Fung & bact cide",
               "Wheat area", "Tot crop area")
df <- df[, keep_cols] 
df <- df %>% gather(Element, Value, `Fung & bact cide`:`Tot crop area`)
#-----------------------------------------------------------------------------
# Consolidate regions
SAfrica_vec <- c("South Africa", "Zimbabwe", "Zambia")
df$Area[which(df$Area %in% SAfrica_vec)] <- "Southern Africa"
SAsia_vec <- c("India", "Pakistan", "Nepal", "Bangladesh")
df$Area[which(df$Area %in% SAsia_vec)] <- "South Asia"
# WEur_vec <- c("Austria", "France", "Spain", "Netherlands", "Italy",
#               "Germany", "Belgium", "United Kingdom of Great Britain and Northern Ireland")
WEur_vec <- c("Western Europe", "Italy", "Poland", "Denmark",
              "United Kingdom of Great Britain and Northern Ireland")
df$Area[which(df$Area %in% WEur_vec)] <- "Western Europe"
EAfrica_vec <- c("Kenya", "Ethiopia", "Uganda", "United Republic of Tanzania")
df$Area[which(df$Area %in% EAfrica_vec)] <- "Eastern Africa"
# KazakKyr_vec <- c("Kazakhstan", "Kyrgyzstan")
# df$Area[which(df$Area %in% KazakKyr_vec)] <- "Kazakh. & Kyrgyzstan"
df$Area[grep("Iran", df$Area)] <- "Iran"
df$Area[grep("United States of America", df$Area)] <- "USA"
keep_these <- c("Australia", "Canada", "China, mainland", "Eastern Africa",
                "Iran", "Iraq", "Kazakhstan", "Kyrgyzstan",
                "Russian Federation", "USA", "South Asia",
                "Southern Africa", "Turkey", "Turkmenistan",
                "Ukraine", "Western Europe", "South America")
df <- subset(df, Area %in% keep_these)
df <- df %>% group_by(Area, Year, Element, Item) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
#-----------------------------------------------------------------------------
df <- df %>% spread(Element, Value) %>% as.data.frame()
df$`Pct. wheat` <- 100 * df$`Wheat area` / df$`Tot crop area`
df$`Fung & bact cide / crop area` <- df$`Fung & bact cide` / df$`Tot crop area`
df$`Fung & bact cide / wheat area` <-  df$`Fung & bact cide` / df$`Wheat area`
df$Year <- as.integer(df$Year)
df <- subset(df, Year >= 2000)
df <- df %>% group_by(Area) %>%
  mutate(`Index (100 = 2000)` = 100 * `Fung & bact cide / crop area` / `Fung & bact cide / crop area`[1]) %>%
  as.data.frame()


#df_plot <- subset(df, Element == "Index (100 = 2000)")
gg <- ggplot(df, aes(x = Year, y = `Index (100 = 2000)`))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~Area, scales = "free_y")
gg <- gg + labs(subtitle = "Fungicide and bactericide use, all crops (metric tons / hectare)")
gg <- gg + theme(axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 60, hjust = 1),
                 strip.text = element_text(size = 8))
gg_fungArea <- gg

#df_plot <- subset(df, Element == "Pct. wheat")
gg <- ggplot(df, aes(x = Year, y = `Pct. wheat`))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~Area, scales = "free_y")
gg <- gg + labs(subtitle = "Wheat percentage of total crop area",
                caption = "Source: FAO")
gg <- gg + theme(axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 60, hjust = 1),
                 strip.text = element_text(size = 8))
gg_wheatPct <- gg

gg_fungArea / gg_wheatPct
this_folder <- "C:/Users/bensc/OneDrive/Documents/2 Blades/"
this_fileName <- "fungicide use graphic.png"
this_filepath <- paste0(this_folder, this_fileName)
ggsave(this_filepath, height = 7, width = 7)
