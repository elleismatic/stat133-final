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
clean_data <- read.csv("clean_data/clean_data.csv", header = TRUE,
                       stringsAsFactors = FALSE)
industries_only <- read.csv("clean_data/industries_only.csv", header = TRUE,
                            stringsAsFactors = FALSE)

#### ELLEN ####
# Scatter plot of Current PE to Forward PE
ggplot(clean_data, aes(x = Current.PE, y = Forward.PE)) +
    geom_point(color = rgb(100, 200, 200, 100, maxColorValue = 255)) + 
    ggtitle("Current PE Ratio vs. Forward PE Ratio") + xlab("Current PE Ratio") +
    ylab("Foward PE Ratio") + theme_classic()

# Bar plot of Current PE, Forward PE, Total Levered Beta, and Expected
# Growth in the Next 5 Years
barplot(clean_data$Current.PE, border = NA, xlab = "Current PE", ylab = "Frequency",
        col = rgb(200, 100, 100, 100, maxColorValue = 255),
        main = "Bar Plot of Current PE Ratio")
barplot(clean_data$Forward.PE, border = NA, xlab = "Foward PE", ylab = "Frequency",
        col = rgb(200, 100, 200, 100, maxColorValue = 255),
        main = "Bar Plot of Forward PE Ratio")
barplot(clean_data$Total.Levered.Beta, border = NA, xlab = "Total levered beta",
        ylab = "Frequency", col = rgb(0, 150, 150, 100, maxColorValue = 255),
        main = "Bar Plot of Total Levered Beta")
barplot(clean_data$Expected.Growth.Next.5.Years, border = NA,
        xlab = "Expected growth rate (next 5 years)", ylab = "Frequency",
        col = rgb(150, 150, 250, 100, maxColorValue = 255),
        main = "Bar Plot of Forward PE Ratio")

# Analyze which industries are in the highest and lowest beta interval
beta_intervals <- seq(from = 0, to = 1.5, by = 0.3)
interval_fun <- function(beta) {
    for (i in 1:5) {
        if (beta_intervals[i] < beta & beta < beta_intervals[i + 1]) {
            return (i)
        }
    }
}
beta_interval <- unlist(lapply(clean_data$Average.Unlevered.Beta,
                               FUN = interval_fun))
as.vector(clean_data$Industry[unlist(beta_interval) == 5])
as.vector(clean_data$Industry[unlist(beta_interval) == 1])

# Make a histogram of the beta intervals created above
dfbeta_interval <- data.frame(table(unlist(beta_interval)))
names(dfbeta_interval) <- c("Interval", "Frequency")
hist(beta_interval, main = "Histogram of Beta Intevals",
     xlab = "Beta intervals", col = rgb(200, 50, 150, 100, maxColorValue = 255),
     border = NA) 

# Plot of Beta versus PE Ratio by given beta intervals
dat1 <- data.frame(PE = industries_only$Current.PE,
                   beta = industries_only$Average.Unlevered.Beta)
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
plot(density(clean_data$Current.PE), main = "Density Plot of Current PE")
plot(density(clean_data$Expected.Growth.Next.5.Years),
     main = "Density Plot of Expected Growth in the Next 5 Years")

# Scatter plot of Current PE to Expected Growth in the Next 5 Years
plot(clean_data$Current.PE, clean_data$Expected.Growth.Next.5.Years,
     col = rgb(50, 200, 200, 100, maxColorValue = 255), pch = 16,
     bg = rgb(200, 200, 200, maxColorValue = 255),
     main = "Bar Plot of Forward PE Ratio")

# Make a linear regression of Current PE to Expected Growth in the Next 5 Years
fit1 <- lm(Expected.Growth.Next.5.Years ~ Current.PE, data = clean_data)
abline(fit1, col = rgb(50, 200, 200, maxColorValue = 255))
summary(fit1)

# Remove outliers and find the linear regression of Current PE to Expected
# Growth in the Next 5 Years
sorted_pe <- unlist(sort(clean_data$Current.PE,
                         decreasing = TRUE))
indices <- unlist(sort(clean_data$Current.PE,
                       decreasing = TRUE,
                       index.return = TRUE)[[2]])
sorted_growth <- clean_data$Expected.Growth.Next.5.Years[indices]
sorted_pe_growth <- data.frame(pe = sorted_pe, growth = sorted_growth)
dat3 <- subset(sorted_pe_growth, sorted_pe < 300)
plot(dat3$pe, dat3$growth, main = "Current PE Ratio vs. Expected Growth",
     xlab = "Current PE ratio", ylab = "Expected growth (next 5 years)",
     col = rgb(200, 50, 200, 100, maxColorValue = 255), pch = 16)
fit2 <- lm(growth ~ pe, data = dat3)
abline(fit2, col = rgb(200, 50, 200, maxColorValue = 255))
summary(fit2)

# Account for beta when sorting PE ratios and growth by taking the PE ratios
# and growths of industries with lower beta
sorted_beta <- unlist(sort(clean_data$Average.Unlevered.Beta,
                          index.return = TRUE)[[2]])
beta_pe <- clean_data$Current.PE[sorted_beta]
beta_growth <- clean_data$Expected.Growth.Next.5.Years[sorted_beta]
end <- length(beta_pe)
cutoff <- round(end/2)
beta1 <- data.frame(pe = beta_pe[1:cutoff], growth = beta_growth[1:cutoff])
plot(beta1$pe, beta1$growth, main = paste("Current PE Ratio vs. Expected Growth",
                                          "For Industries with Lower Beta"),
     xlab = "Current PE ratio", ylab = "Expected growth (next 5 years)",
     col = rgb(50, 50, 200, 100, maxColorValue = 255), pch = 16)
fit3 <- lm(growth ~ pe, data = beta1)
abline(fit3, col = rgb(50, 50, 200, maxColorValue = 255))
summary(fit3)

# Industries with lower beta
print("Industries with lower beta:")
print(clean_data$Industry[sorted_beta][1:cutoff])

# Account for beta when sorting PE ratios and growth by taking the PE ratios
# and growths of industries with higher beta
beta2 <- data.frame(pe = beta_pe[(cutoff+1):end],
                    growth = beta_growth[(cutoff+1):end])
plot(beta2$pe, beta2$growth, main = paste("Current PE Ratio vs. Expected Growth",
                                          "For Industries with Higher Beta"),
     xlab = "Current PE ratio", ylab = "Expected growth (next 5 years)",
     col = rgb(200, 50, 50, 100, maxColorValue = 255), pch = 16)
fit4 <- lm(growth ~ pe, data = beta2)
abline(fit4, col = rgb(200, 50, 50, maxColorValue = 255))
summary(fit4)

# Industries with higher beta
print("Industries with higher beta:")
print(clean_data$Industry[sorted_beta][(cutoff+1):end])

# Graph Current PE to Expected Growth in the Next 5 Years
ggplot(data = clean_data, aes(x = Current.PE,
                              y = Expected.Growth.Next.5.Years)) +
    geom_line(color = "#548edf") + xlab("Current PE") +
    ylab("Expected Growth in the Next 5 Years") +
    ggtitle("Plot of Current PE to Expected Growth") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries with
# growth, g, fulfilling g < 300
ggplot(dat3, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity", color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries with
# growth, g, fulfilling 100 <= g < 200
dat4 <- subset(sorted_pe_growth, sorted_pe >= 100 & sorted_pe < 200)
ggplot(dat4, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity", color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") +
    theme_classic()

# Make a histogram of Current PE to Expected Growth for industries
# with growth, g, fulfilling g < 100
dat5 <- subset(sorted_pe_growth, sorted_pe < 100)
ggplot(dat5, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity",
             color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") + theme_classic()

# Make bubble plots of Current PE and Expected Growth in the
# Next 5 Years for different industries
symbols(industries_only$Current.PE,
        industries_only$Expected.Growth.Next.5.Years,
        circles = industries_only$Number.of.Firms,
        inches = 0.35, fg="white", bg="#facccc8d",
        xlab="Current PE", ylab="Expected Growth in 5 Years",
        main = "Current PE vs Expected Growth in the Next 5 Years")
text(industries_only$Current.PE, industries_only$Expected.Growth.Next.5.Years, 
     labels = industries_only$Number.of.Firms, cex = 0.2)

# Make bubble plots of Current PE and Expected Growth in the
# Next 5 Years for different industries accounting for outliers
industry_pe <- unlist(sort(industries_only$Current.PE, decreasing = TRUE))
industry_indices <- unlist(sort(industries_only$Current.PE, decreasing = TRUE,
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
# Graph the scatterplot of unlevered beta's impact on expected growth
# when accounting for their regression
reg1 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta,
               data = clean_data)
summary(reg1)
ggplot(clean_data, aes(x = Average.Unlevered.Beta,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Average Unlevered Beta") +
    ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg1)["Average.Unlevered.Beta"]),
                    intercept = unname(coef(reg1)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Average Unlevered Beta's Impact on Expected Growth") +
    theme_classic()

# Find the multivariate regression of expected growth to unlevered beta
# and current PE ratio
reg2 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE, data = clean_data)
summary(reg2)

# Graph the scatterplot of unlevered beta's impact on expected growth
ggplot(clean_data, aes(x = Average.Unlevered.Beta,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Average Unlevered Beta") + ylab("Expected Growth (%)") +
    ggtitle("Scatterplot of Average Unlevered Beta vs Expected Growth") +
    theme_classic()

# Graph the scatterplot of PEG ratio's impact on expected growth after
# finding the multivariate regression of expected growth in the next 5
# years to average unlevered beta, the current PE ratio, and the PEG Ratio.
# The graph only concerns the regression of expected growth to beta.
reg3 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE + PEG.Ratio, data = clean_data)
summary(reg3)
ggplot(clean_data, aes(x = Current.PE,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Current PE Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg3)["Average.Unlevered.Beta"]),
                    intercept = unname(coef(reg3)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Current PE Ratio's Impact on Expected Growth") +
    theme_classic()

# Graph the scatterplot of PEG ratio's impact on expected growth and the
# regression line of expected growth to PEG ratio and the regression line
# of the multivariate regression found in reg3
reg4 <- lm(Expected.Growth.Next.5.Years ~ PEG.Ratio, data = clean_data)
summary(reg4)
ggplot(clean_data, aes(x = PEG.Ratio,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point(color = rgb(223, 84, 84, 100, maxColorValue = 255)) +
    xlab("PEG Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg3)["PEG.Ratio"]),
                    intercept = unname(coef(reg3)["(Intercept)"])),
                color = rgb(84, 233, 233, maxColorValue = 255)) +
    geom_abline(aes(slope = unname(coef(reg4)["PEG.Ratio"]),
                    intercept = unname(coef(reg4)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("PEG Ratio's Impact on Expected Growth") +
    theme_classic()

#### CHRISTIAN ####
plot(clean_data$Trailing.PE, clean_data$Forward.PE,
     xlab = "Trailing PE", ylab = "Forward PE",
     main = "Correlation Between Trailing PE and Forward PE",
     col = 'blue')

plot(clean_data$Forward.PE, clean_data$Trailing.PE,
     xlab = "Forward PE", ylab = "Trailing PE",
     main = "Correlation Between Trailing PE and Forward PE",
     col = 'dark green')

plot(clean_data$Average.Unlevered.Beta, clean_data$PEG.Ratio,
     xlab = "Average Unlevered Beta", ylab = "PE Ratio",
     main = "Correlation Between Beta and PE Ratio",
     col = 'red')

plot(clean_data$PEG.Ratio, clean_data$Average.Unlevered.Beta,
     xlab = "PE Ratio", ylab = "Average Unlevered Beta",
     main = "Correlation Between PE Ratio and Beta",
     col = 'dark red')

plot(clean_data$Trailing.PE, clean_data$Expected.Growth.Next.5.Years,
     xlab = "Trailing PE", ylab = "Expected Growth in the Next 5 Years",
     main = paste("Correlation Between Traling PE and Expected",
                  "Growth in the Next 5 Years"), col = 'purple')

plot(clean_data$Expected.Growth.Next.5.Years, clean_data$Trailing.PE,
     xlab = "Expected Growth in the Next 5 Years", ylab = "Trailing PE",
     main = paste("Correlation Between Expected Growth in the",
                  "Next 5 Years and Trailing PE"), col = 'navy blue')

# Make 3D scatterplot of Expected Growth in the Next 5 Years, PEG Ratio,
# and Average Unlevered Beta, then examine the graph from multiple angles
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta,
              angle = 30, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "PEG Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PEG Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$PEG.Ratio,
              z = clean_data$Average.Unlevered.Beta,
              angle = 300, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "PEG Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PEG Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$Average.Unlevered.Beta,
              z = clean_data$PEG.Ratio,
              angle = 100, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Beta", zlab = "PEG Ratio",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PEG Ratio, and Beta")

# Make 3D scatterplot of Expected Growth in the Next 5 Years, Current PE,
# and Average Unlevered Beta, then examine the graph from multiple angles
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 30, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 300, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 200, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")