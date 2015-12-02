# Set up ggplot2
library(ggplot2)
dev.off()

# Extract raw data
beta <- read.csv("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/total_beta.csv", header = TRUE)
pe <- read.csv("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/PE_data.csv", header = TRUE)

# Combine data from the two files
raw_data <- merge(beta, pe, by = intersect(names(beta), names(pe)))
str(raw_data)

# Rename columns
raw_data$Expected.growth.in.next.5.years <- raw_data$Expected.growth...next.5.years
raw_data$Expected.growth...next.5.years <- NULL

# Turn Expected Growth in the Next 5 Years to a numeric column vector
raw_data$Expected.growth...next.5.years <- as.vector(raw_data$Expected.growth...next.5.years)
for (i in 1:length(raw_data$Expected.growth...next.5.years)) {
    raw_data$Expected.growth...next.5.years[i] <- gsub("%", "", raw_data$Expected.growth...next.5.years[i])
}
raw_data$Expected.growth...next.5.years <- as.numeric(raw_data$Expected.growth...next.5.years)

# Make copy of raw_data without the row "Unclassified"
no_unclassified <- raw_data[-which(raw_data$Industry == "Unclassified"),]

# Make copy of raw_data without the row "Unclassified" or "Total Market"
industries_only <- raw_data[-which(raw_data$Industry == "Unclassified" | raw_data$Industry == "Total Market"),]

# Scatter plot of Current PE to Expected Growth in the Next 5 Years
plot(raw_data$Current.PE, raw_data$Expected.growth...next.5.years)

# Make bubble plots of Current PE and Expected Growth in the Next 5 Years for different industries
symbols(industries_only$Current.PE, industries_only$Expected.growth.in.next.5.years,
        circles = industries_only$Number.of.firms, inches=0.35, fg="white",
        bg="pink", xlab="Current PE", ylab="Expected Growth in 5 Years")
text(industries_only$Current.PE, industries_only$Expected.growth.in.next.5.years,
     cex = 0.2)

# Density curve of Current PE and of Expected Growth in the Next 5 Years
plot(density(no_unclassified$Current.PE))
plot(density(no_unclassified$Expected.growth...next.5.years))

# Graph Current PE to Expected Growth in the Next 5 Years
ggplot(data = raw_data) +
    geom_line(aes(x = Current.PE, y = Expected.growth.in.next.5.years))
