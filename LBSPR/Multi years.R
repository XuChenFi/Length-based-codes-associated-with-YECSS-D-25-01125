# Load data
df <- read.csv("LIME/Year.csv")

# Create empty list to store data for each year
year_list <- list()

# Get all years
years <- sort(unique(df$Year))

# Extract data by year, keeping original Length values
for (yr in years) {
  df_year <- df[df$Year == yr, c("Length", "Frequency")]
  names(df_year)[2] <- as.character(yr)  # Rename column to year
  year_list[[as.character(yr)]] <- df_year
}

# Merge data for all years using full outer join with Length as key
library(dplyr)

result <- year_list[[1]]
for (i in 2:length(year_list)) {
  result <- full_join(result, year_list[[i]], by = "Length")
}

# Replace NA with 0
result[is.na(result)] <- 0

# Sort by Length
result <- result[order(result$Length), ]

# Rename first column to "Length_Group_Median"
colnames(result)[1] <- "Length_Group_Median"
