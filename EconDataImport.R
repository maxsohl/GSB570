
library(readxl)
library(lubridate)
library(dplyr)
library(zoo)



#Main Data
Data <- read_excel("Data.xlsx")

#Econ DF
econ_df<- read.csv("EconData.csv")

econ_df$DATE <- as.character(econ_df$DATE)
econ_df$DATE <- gsub("^01-", "", econ_df$DATE)
econ_df <- econ_df %>%
  mutate(DATE = as.yearmon(DATE, format = "%m-%Y"))


Data$`Month of Period End` <- as.yearmon(Data$`Month of Period End`, format = "%B %Y")



#Merge
matched_data <- merge(econ_df, Data, by.x = "DATE", by.y = "Month of Period End")

#YEar column 
matched_data <- matched_data %>%
  mutate(YEAR = as.yearmon(DATE, format = "%b %Y") %>% format("%Y"))

#Population by city
pop_20_22_df<- read_excel("E-4_2023_InternetVersion.xlsx", sheet = "Table 1 County State")
pop_20_22_df <- pop_20_22_df[, -which(names(pop_20_22_df) == "4/1/2020")]

pop_10_20_df<- read_excel("E-4_2010-2020-Internet-Version.xlsx", sheet = "Table 1 County State")
pop_10_20_df <- pop_10_20_df[, -which(names(pop_10_20_df) == "4/1/2010")]

pop_merged_df <- merge(pop_10_20_df, pop_20_22_df, by = "COUNTY")
pop_merged_df <- subset(pop_merged_df, COUNTY %in% c("San Francisco", "Yuba"))

colnames(pop_merged_df) <- sub("^1/1/", "", colnames(pop_merged_df))

# Transpose the data frame
transposed_data <- pop_merged_df %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()

# Update column names
colnames(transposed_data) <- c("San Francisco County, CA", "Yuba County, CA")

transposed_data <- transposed_data[-1,]

pop_merged_df <- transposed_data


library(tidyverse)
pop_merged_df$YEAR <- rownames(pop_merged_df)

# Convert row names to integers (if necessary)
pop_merged_df$YEAR<- as.integer(pop_merged_df$YEAR)

# Reset row names to default row numbering
rownames(pop_merged_df) <- NULL


pop_long_df <- pop_merged_df %>%
  pivot_longer(cols = -YEAR, names_to = "COUNTY", values_to = "population") %>%
  mutate(COUNTY = sub("_", " ", COUNTY))


#Merge
merged_data <- merge(matched_data,pop_long_df, by.x = c("YEAR", "Region"), by.y = c("YEAR", "COUNTY"))


################################################## HPI ############################################################


HPI_US_df<- read.csv("USSTHPI.csv")


