# author: Christian Alarcio, Ellen Chan, Anais Sidhu, Ruomeng (Michelle) Yang
# title: Cleaning and Preprocessing Data

# Set working directory
setwd("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/")

# Extract raw data
beta <- read.csv("raw_data/total_beta.csv", header = TRUE,  stringsAsFactors = FALSE)
pe <- read.csv("raw_data/pe_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Combine data from the two files
raw_data <- merge(beta, pe, by = intersect(names(beta), names(pe)))

# Inspect merged data
head(raw_data)
summary(raw_data)
names(raw_data)
str(raw_data)

# Inspect individual elements of the merged data
print("Summary of elements in raw_data")
for (i in 1:length(raw_data)) {
    print(paste0("Summary of ", names(raw_data)[i], ":"))
    print(summary(raw_data[, i]))
}

# Remove blank columns
clean_data$X <- NULL

# Rename column titled Aggregate.Mkt.Cap..Net.Income
clean_data$Aggregate.Mkt.Cap.Net.Income <- clean_data$Aggregate.Mkt.Cap..Net.Income
clean_data$Aggregate.Mkt.Cap..Net.Income <- NULL

# Rename column titled clean_data$Aggregate.Mkt.Cap..Trailing.Net.Income
clean_data$Aggregate.Mkt.Cap.Trailing.Net.Income <- clean_data$Aggregate.Mkt.Cap..Trailing.Net.Income
clean_data$Aggregate.Mkt.Cap..Trailing.Net.Income <- NULL

# Rename column titled Number.of.firms
clean_data$Number.of.Firms <- clean_data$Number.of.firms
clean_data$Number.of.firms <- NULL

# Rename column titled Average.correlation and turn it into a numeric column vector
clean_data$Average.Correlation <- as.numeric(gsub("%", "", clean_data$Average.correlation))
clean_data$Average.correlation <- NULL
head(clean_data$Average.Correlation)

# Rename column titled Expected.growth...next.5.years and turn it into a numeric column vector
clean_data$Expected.Growth.Next.5.Years <- as.numeric(gsub("%", "", clean_data$Expected.growth...next.5.years))
clean_data$Expected.growth...next.5.years <- NULL
head(clean_data$Expected.Growth.Next.5.Years)

# Make copy of clean_data without the row "Unclassified"
clean_data <- clean_data[-which(clean_data$Industry == "Unclassified"),]

# Make copy of clean_data without the row "Unclassified" or "Total Market"
industries_only <- clean_data[-which(clean_data$Industry == "Unclassified" | clean_data$Industry == "Total Market"),]

# Inspect elements of the final clean_data and industries_only
print("Summary of elements in clean_data")
for (i in 1:length(clean_data)) {
    print(paste0("Summary of ", names(clean_data)[i], ":"))
    print(summary(clean_data[, i]))
}

print("Summary of elements in industries_only")
for (i in 1:length(industries_only)) {
    print(paste0("Summary of ", names(industries_only)[i], ":"))
    print(summary(industries_only[, i]))
}

# Basic plots of variables
plot(clean_data$Average.Unlevered.Beta)
plot(clean_data$Average.Levered.Beta)
plot(clean_data$Current.PE)
plot(clean_data$Forward.PE)
plot(clean_data$PEG.Ratio)
plot(clean_data$Average.Correlation)
plot(clean_data$Expected.Growth.Next.5.Years)

# Exploration of industries that have certain maximum and minimum values
clean_data[which.max(clean_data$Average.Unlevered.Beta), "Industry"]
clean_data[which.min(clean_data$Average.Unlevered.Beta), "Industry"]
clean_data[which.max(clean_data$Average.Levered.Beta), "Industry"]
clean_data[which.min(clean_data$Average.Levered.Beta), "Industry"]
clean_data[which.max(clean_data$Current.PE), "Industry"]
clean_data[which.min(clean_data$Current.PE), "Industry"]
clean_data[which.max(clean_data$PEG.Ratio), "Industry"]
clean_data[which.min(clean_data$PEG.Ratio), "Industry"]
clean_data[which.max(clean_data$Expected.Growth.Next.5.Years), "Industry"]
clean_data[which.min(clean_data$Expected.Growth.Next.5.Years), "Industry"]

# Histograms of select variables
hist(clean_data$Average.Unlevered.Beta,
     main = "Histogram of Average Unlevered Beta",
     xlab = "Average Unlevered Beta")
hist(clean_data$Average.Levered.Beta,
     main = "Histogram of Average Levered Beta",
     xlab = "Average Levered Beta")
hist(clean_data$PEG.Ratio,
     main = "Histogram of PEG Ratio",
     xlab = "PEG Ratio")
hist(clean_data$Expected.Growth.Next.5.Years,
     main = "Histogram of Expected Growth in the Next 5 Years",
     xlab = "Expected Growth in the Next 5 Years")

# Create CSV files for clean data
file.create("clean_data/clean_data.csv")
write.csv(clean_data, file = "clean_data/clean_data.csv")
file.create("clean_data/industries_only.csv")
write.csv(industries_only, file = "clean_data/industries_only.csv")
