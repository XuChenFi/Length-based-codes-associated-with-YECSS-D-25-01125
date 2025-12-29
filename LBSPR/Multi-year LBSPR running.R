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
write.table(result, "C:/data.csv", 
            col.names = FALSE, 
            sep = ",")
# Rename first column to "Length_Group_Median"
colnames(result)[1] <- "Length_Group_Median"
#LBSPR running
library(LBSPR)
LB_pars <- new("LB_pars")
LB_pars@MK <- 0.57/0.272324733
LB_pars@Linf <- max(result[,1])
LB_pars@L50 <- 11
LB_pars@L95 <- 12
LB_pars@CVLinf <- 0.15
LB_pars@FecB <- 3
LB_pars@Mpow <- 0
LB_pars@Walpha <- 0.231
LB_pars@Wbeta <- 2.36
LB_pars@BinWidth <- 1
Len1 <- new("LB_lengths", LB_pars=LB_pars, file=paste0("C:/data.csv"),
dataType="freq")
Len2 <- new("LB_lengths", LB_pars=LB_pars, file=paste0("C:/data.csv"),
dataType="raw")
lbspr_res <- LBSPRfit(LB_pars=LB_pars, LB_lengths=Len2, Control=list(modtype=c("GTG")))
spr <- lbspr_res@SPR
