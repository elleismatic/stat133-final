# author: Christian Alarcio, Ellen Chan, Anais Sidhu, Ruomeng (Michelle) Yang
# title: Methods & Analysis of Data

# Set working directory
setwd("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/")

# Set up ggplot2
library(ggplot2)
dev.off()

# Set up readr
library(readr)

# Set up scatterplot3d
library(scatterplot3d)

# Set up stringr
library(stringr)

# Read data files
clean_data <- read.csv("clean_data/clean_data.csv", header = TRUE, stringsAsFactors = FALSE)
industries_only <- read.csv("clean_data/industries_only.csv", header = TRUE, stringsAsFactors = FALSE)

#### ELLEN ####
# Scatter plot of Current PE to Forward PE
plot(clean_data$Current.PE, clean_data$Forward.PE)

# Bar plot of Current PE, Forward PE, Total Levered Beta, and Expected Growth
barplot(clean_data$Current.PE, xlab = "Current PE", ylab = "Frequency")
barplot(clean_data$Forward.PE, xlab = "Foward PE", ylab = "Frequency")
barplot(clean_data$Total.Levered.Beta, xlab = "Beta", ylab = "Frequency")
barplot(clean_data$Expected.Growth.Next.5.Years, xlab = "Expected growth rate", ylab = "Frequency")

# Analyze which industries are in the highest and lowest beta interval
beta_intervals <- seq(from = 0, to = 1.5, by = 0.3)
interval_fun <- function(beta) {
    for (i in 1:5) {
        if (beta_intervals[i] < beta & beta < beta_intervals[i + 1]) {
            return (i)
        }
    }
}
beta_interval <- unlist(lapply(clean_data$Average.Unlevered.Beta, FUN = interval_fun))
as.vector(clean_data$Industry[unlist(beta_interval) == 5])
as.vector(clean_data$Industry[unlist(beta_interval) == 1])

# Make a histogram of the beta intervals created above
dfbeta_interval <- data.frame(table(unlist(beta_interval)))
names(dfbeta_interval) <- c("Interval", "Frequency")
hist(beta_interval, main = "Histogram of beta intevals",
     xlab = "beta intervals") 

# Plot of Beta versus PE Ratio by given beta intervals
dat1 <- data.frame(PE = industries_only$Current.PE, beta = industries_only$Average.Unlevered.Beta)
ggplot(dat1, aes(x = beta, y = PE)) +
    geom_line(stat = "identity", color = beta_interval) + 
    ggtitle("Beta vs PE Ratio") + theme_classic() + 
    scale_x_continuous(breaks = beta_intervals) + 
    scale_y_continuous(breaks = seq(from = 0, to = 1500, by = 100)) 
summary(industries_only$Current.PE)

# Bar plot of Beta versus Expected Growth Rate in the Next 5 Years
dat2 <- data.frame(beta = clean_data$Average.Unlevered.Beta,
                   growth = clean_data$Expected.Growth.Next.5.Years)
ggplot(dat2, aes(x = beta, y = growth)) +
    geom_bar(position = "identity", stat = "identity") + 
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

#### MICHELLE ####
# Density curve of Current PE and of Expected Growth in the Next 5 Years
plot(density(clean_data$Current.PE))
plot(density(clean_data$Expected.Growth.Next.5.Years))

# Scatter plot of Current PE to Expected Growth in the Next 5 Years
plot(clean_data$Current.PE, clean_data$Expected.Growth.Next.5.Years)

# Make a linear regression of Current PE to Expected Growth in the Next 5 Years
fit1 <- lm(Expected.Growth.Next.5.Years ~ Current.PE,
          data = clean_data)
abline(fit1)
summary(fit1)

# Remove outliers and find the linear regression of Current PE
# to Expected Growth in the Next 5 Years
sorted_pe <- unlist(sort(clean_data$Current.PE,
                         decreasing = TRUE))
indices <- unlist(sort(clean_data$Current.PE,
                       decreasing = TRUE,
                       index.return = TRUE)[[2]])
sorted_growth <- clean_data$Expected.Growth.Next.5.Years[indices]
sorted_pe_growth <- data.frame(pe = sorted_pe, growth = sorted_growth)
dat3 <- subset(sorted_pe_growth, sorted_pe < 300)
plot(dat3$pe, dat3$growth)
fit2 <- lm(growth ~ pe, data = dat3)
abline(fit2)
summary(fit2)

# Account for beta when sorting PE ratios and growth by taking
# the PE ratios and growths of industries with lower beta
sorted_beta <- unlist(sort(clean_data$Average.Unlevered.Beta,
                          index.return = TRUE)[[2]])
beta_pe <- clean_data$Current.PE[sorted_beta]
beta_growth <- clean_data$Expected.Growth.Next.5.Years[sorted_beta]
end <- length(beta_pe)
cutoff <- round(end/2)
beta1 <- data.frame(pe = beta_pe[1:cutoff],
                    growth = beta_growth[1:cutoff])
plot(beta1$pe, beta1$growth)
fit3 <- lm(growth ~ pe, data = beta1)
abline(fit3)
summary(fit3)

# Industries with lower beta
print(clean_data$Industry[sorted_beta][1:cutoff])

# Account for beta when sorting PE ratios and growth by taking
# the PE ratios and growths of industries with higher beta
beta2 <- data.frame(pe = beta_pe[(cutoff+1):end],
                    growth = beta_growth[(cutoff+1):end])
plot(beta2$pe, beta2$growth)
fit4 <- lm(growth ~ pe, data = beta2)
abline(fit4)
summary(fit4)

# Industries with higher beta
print(clean_data$Industry[sorted_beta][(cutoff+1):end])

# Graph Current PE to Expected Growth in the Next 5 Years
ggplot(data = clean_data, aes(x = Current.PE,
                              y = Expected.Growth.Next.5.Years)) +
    geom_line(color = "#548edf") + xlab("Current PE") +
    ylab("Expected Growth in the Next 5 Years") +
    ggtitle("Plot of Current PE to Expected Growth") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries
# with growth, g, fulfilling g < 300
ggplot(dat3, aes(x = pe, y = growth)) +
    geom_line(color = "#ebcefb") +
    geom_bar(position = "identity",
             stat = "identity",
             color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries
# with growth, g, fulfilling 100 <= g < 200
dat4 <- subset(sorted_pe_growth, sorted_pe >= 100 &
                   sorted_pe < 200)
ggplot(dat4, aes(x = pe, y = growth)) +
    geom_line(color = "#ebcefb") +
    geom_bar(position = "identity",
             stat = "identity",
             color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries
# with growth, g, fulfilling g < 100
dat5 <- subset(sorted_pe_growth, sorted_pe < 100)
ggplot(dat5, aes(x = pe, y = growth)) +
    geom_line(color = "#ebcefb") +
    geom_bar(position = "identity",
             stat = "identity",
             color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

# Make bubble plots of Current PE and Expected Growth in the
# Next 5 Years for different industries
symbols(industries_only$Current.PE,
        industries_only$Expected.Growth.Next.5.Years,
        circles = industries_only$Number.of.Firms,
        inches=0.35, fg="white", bg="#facccc8d",
        xlab="Current PE", ylab="Expected Growth in 5 Years",
        main = paste("Current PE vs Expected Growth",
                     "in the Next 5 Years"))
text(industries_only$Current.PE,
     industries_only$Expected.Growth.Next.5.Years, 
     labels = industries_only$Number.of.Firms,
     cex = 0.2)

# Make bubble plots of Current PE and Expected Growth in the
# Next 5 Years for different industries accounting for outliers
industry_pe <- unlist(sort(industries_only$Current.PE,
                         decreasing = TRUE))
industry_indices <- unlist(sort(industries_only$Current.PE,
                       decreasing = TRUE,
                       index.return = TRUE)[[2]])
industry_growth <- industries_only$
    Expected.Growth.Next.5.Years[industry_indices]
industry_num_firms <- industries_only$Number.of.Firms[industry_indices]
industry_pe_growth <- data.frame(pe = industry_pe, growth = industry_growth,
                                 num_firms = industry_num_firms)
dat6 <- subset(industry_pe_growth, industry_pe < 150)
symbols(dat6$pe, dat6$growth, circles = dat6$num_firms,
        inches=0.35, fg="white", bg="#facccc8d",
        xlab="Current PE", ylab="Expected Growth in 5 Years",
        main = paste("Current PE Less Than 150 vs",
                     "Expected Growth in the Next 5 Years"))
text(dat6$pe, dat6$growth, labels = dat6$num_firms, cex = 0.3)

#### ANAIS ####
# Make a regression of Expected Growth in the Next 5 Years
# and Average Unlevered Beta
beta_reg <- lm(Expected.Growth.Next.5.Years ~
                   Average.Unlevered.Beta ,
               data = clean_data)
summary(beta_reg)

# Plot of Average Unlevered Beta to Expected Growth in the
# Next 5 Years
ggplot(clean_data, aes(x = Average.Unlevered.Beta,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + ylab("Expected Growth (%)") + xlab("Unlevered Beta") +
    ggtitle("Unlevered Beta's Impact on Expected Growth") +
    theme_classic()

reg_2 <- lm(Expected.Growth.Next.5.Years ~
                Average.Unlevered.Beta + Current.PE,
            data = clean_data)
summary(reg_2)

ggplot(clean_data, aes(x = Current.PE,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("P/E Ratio") +
    ylab("Expected Growth (%)") +
    ggtitle("P/E Ratio Impact on Expected Growth")

# Graph the scatterplot of PEG Ratio's impact on Expected Growth
# when accounting for the multivariate regression of Expected Growth
# in the Next 5 Years to Average Unlevered Beta, Current PE,
# and PEG Ratio
reg3 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE + PEG.Ratio, data = clean_data)
summary(reg3)
ggplot(clean_data, aes(x = PEG.Ratio,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("PEG Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg3)["PEG.Ratio"]),
                    intercept = unname(coef(reg3)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("PEG Ratio's Impact on Expected Growth") +
    theme_classic()

# Graph the scatterplot of PEG Ratio's impact on Expected Growth
# when accounting for the multivariate regression of Expected Growth
# in the Next 5 Years to Average Unlevered Beta, Current PE,
# and PEG Ratio
reg4 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE + PEG.Ratio, data = clean_data)
summary(reg4)
ggplot(clean_data, aes(x = Current.PE,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Current PE") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg4)["PEG.Ratio"]),
                    intercept = unname(coef(reg4)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Current PE's Impact on Expected Growth") +
    theme_classic()

# Graph the scatterplot of PEG Ratio's impact on Expected Growth
reg5 <- lm(Expected.Growth.Next.5.Years ~ PEG.Ratio,
          data = clean_data)
summary(reg5)
ggplot(clean_data, aes(x = PEG.Ratio,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("PEG Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg5)["PEG.Ratio"]),
                    intercept = unname(coef(reg5)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("PEG Ratio's Impact on Expected Growth") +
    theme_classic()

#### CHRISTIAN ####
plot(clean_data$Trailing.PE, clean_data$Forward.PE,
     xlab = "Trailing PE", ylab = "Forward PE",
     main = "Correlation Between Trailing PE and Forward PE",
     col = 'blue')

plot(clean_data$Forward.PE, clean_data$Trailing.PE, xlab = "Forward PE",
     ylab = "Trailing PE", main = "Correlation Between Trailing PE and Forward PE", col = 'dark green')

plot(clean_data$Average.Unlevered.Beta, clean_data$PEG.Ratio, xlab = "Average Unlevered Beta",
     ylab = "PE Ratio", main = "Correlation Between Beta and PE Ratio", col = 'red')

plot(clean_data$PEG.Ratio, clean_data$Average.Unlevered.Beta, xlab = "PE Ratio",
     ylab = "Average Unlevered Beta", main = "Correlation Between PE Ratio and Beta", col = 'dark red')

plot(clean_data$Trailing.PE, clean_data$Expected.Growth.Next.5.Years, xlab = "Trailing PE",
     ylab = "Expected Growth in the Next 5 Years", main = "Correlation Between Traling PE and Expected Growth in the Next 5 Years", col = 'purple')

plot(clean_data$Expected.Growth.Next.5.Years, clean_data$Trailing.PE, xlab = "Expected Growth in the Next 5 Years",
     ylab = "Trailing PE", main = "Correlation Between Expected Growth in the Next 5 Years and Trailing PE", col = 'navy blue')

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio, z = clean_data$Average.Unlevered.Beta, angle = 30, scale.y = .3, xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")

# Look at PE Ratio here:
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio, z = clean_data$Average.Unlevered.Beta, angle = 280, scale.y = .3, xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")

# Multiple angles
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta, angle = 200, scale.y = .3,
              xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta, angle = 230, scale.y = .3,
              xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta, angle = -400, scale.y = .3,
              xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years, clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta, angle = 200, scale.y = .3,
              xlab = "Expected Growth in Next 5 Years", ylab = "PE Ratio", zlab = "Beta")
