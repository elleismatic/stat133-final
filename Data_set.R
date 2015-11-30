beta <- read.csv("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/total_beta.csv", header = TRUE)
pe <- read.csv("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/PE_data.csv", header = TRUE)

raw_data <- merge(beta, pe, by = intersect(names(beta), names(pe)))
str(raw_data)

raw_data$Expected.growth...next.5.years <- as.vector(raw_data$Expected.growth...next.5.years)
for (i in 1:length(raw_data$Expected.growth...next.5.years)) {
    raw_data$Expected.growth...next.5.years[i] <- as.numeric(gsub("%", "", raw_data$Expected.growth...next.5.years[i]))
}
plot(raw_data$Current.PE, raw_data$Expected.growth...next.5.years)

#start with beta and pe Current.PE
#2nd: beta and expected gworth

# library(ggplot2)
# dev.off()

# ggplot(data = raw_data) +
#    geom_line(aes(x = PEG.Ratio, y = Expected.growth...next.5.years))
