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
                       row.names = 1, stringsAsFactors = FALSE)
industries_only <- read.csv("clean_data/industries_only.csv", header = TRUE,
                            row.names = 1, stringsAsFactors = FALSE)

# Scatter plot of Current PE to Forward PE
ggplot(clean_data, aes(x = Current.PE, y = Forward.PE)) +
    geom_point(color = rgb(100, 200, 200, 100, maxColorValue = 255)) + 
    ggtitle("Current PE Ratio vs. Forward PE Ratio") +
    xlab("Current PE Ratio") + ylab("Foward PE Ratio") + theme_classic()

# Histogram of Forward PE, Total Levered Beta, and Expected Growth in the
# Next 5 Years
hist(clean_data$Forward.PE, border = NA,
     main = "Histogram of Forward PE Ratio", xlab = "Foward PE",
     ylab = "Frequency", col = rgb(200, 100, 200, 100, maxColorValue = 255))
hist(clean_data$Total.Levered.Beta, border = NA, xlab = "Total levered beta",
     ylab = "Frequency", col = rgb(0, 150, 150, 100, maxColorValue = 255),
     main = "Histogram of Total Levered Beta")
hist(clean_data$Expected.Growth.Next.5.Years, border = NA,
     xlab = "Expected growth rate (next 5 years)", ylab = "Frequency",
     col = rgb(150, 150, 250, 100, maxColorValue = 255),
     main = "Histogram of Expected Growth Rate in the Next 5 Years")

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
hist(beta_interval, border = NA, main = "Histogram of Beta Intevals",
     xlab = "Beta intervals", ylab = "Frequency",
     col = rgb(200, 50, 150, 100, maxColorValue = 255))

# Histogram of PE Ratio and bar plot of Beta versus PE Ratio by given beta
# intervals
axis(side = 1, at = seq(0,1500,by = 300))
hist(clean_data$Current.PE, border = NA,
     main = "Histogram of Current PE Ratio", xlab = "Current PE",
     ylab = "Frequency", col = rgb(250, 150, 150, 100, maxColorValue = 255))
dat1 <- data.frame(PE = industries_only$Current.PE,
                   beta = industries_only$Average.Unlevered.Beta)
ggplot(dat1, aes(x = beta, y = PE)) + xlab("Average unlevered beta") +
    geom_line(stat = "identity", color = beta_interval) + 
    ggtitle("Beta vs PE Ratio") + theme_classic() + 
    scale_x_continuous(breaks = beta_intervals) + ylab("Current PE ratio") +
    scale_y_continuous(breaks = seq(from = 0, to = 1500, by = 100)) 
summary(industries_only$Current.PE)

# Bar plot of Beta versus Expected Growth Rate in the Next 5 Years
dat2 <- data.frame(beta = clean_data$Average.Unlevered.Beta,
                   growth = clean_data$Expected.Growth.Next.5.Years)
ggplot(dat2, aes(x = beta, y = growth)) + xlab("Average unlevered beta") +
    geom_bar(position = "identity", stat = "identity") +
    ggtitle("Beta vs Expected Growth Rate") + theme_classic() +
    ylab("Expected growth (in the next 5 years)")

# Graph the scatterplot of unlevered beta's impact on expected growth
# when accounting for their regression
reg1 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta,
           data = clean_data)
summary(reg1)
ggplot(clean_data, aes(x = Average.Unlevered.Beta,
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Average Unlevered Beta") +
    ylab("Expected Growth (%)") + theme_classic() +
    geom_abline(aes(slope = unname(coef(reg1)["Average.Unlevered.Beta"]),
                    intercept = unname(coef(reg1)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Average Unlevered Beta's Impact on Expected Growth")

# Make a scatter plot and regression of beta to growth accounting
# for outliers
sorted_beta <- sort(clean_data$Average.Unlevered.Beta,
                    index.return = TRUE)[[2]]
sort_beta <- sort(clean_data$Average.Unlevered.Beta)
sorted_growth1 <- clean_data$Expected.Growth.Next.5.Years[sorted_beta]
sorted_beta_growth <- data.frame(beta = sort_beta, growth = sorted_growth1)
dat3 <- subset(sorted_beta_growth, sorted_growth1 > 0)
plot(dat3$growth, dat3$beta,
     col = rgb(100, 100, 100, 150, maxColorValue = 255), pch = 16,
     bg = rgb(200, 200, 200, maxColorValue = 255), main = "Beta vs Growth",
     xlab = "Expected growth (%)", ylab = "Average unlevered beta")
fit_growth_beta <- lm(beta ~ growth, data = dat3)
abline(fit_growth_beta, col = rgb(100, 100, 100, maxColorValue = 255))
summary(fit_growth_beta)

# Density curve of Current PE and of Expected Growth in the Next 5 Years
plot(density(clean_data$Current.PE), main = "Density Plot of Current PE",
     xlab = "Current PE", ylab = "Density",
     col = rgb(50, 200, 200, maxColorValue = 255))
polygon(density(clean_data$Current.PE), border = NA,
        col = rgb(50, 200, 200, maxColorValue = 255))
plot(density(clean_data$Expected.Growth.Next.5.Years),
     main = "Density Plot of Expected Growth in the Next 5 Years",
     xlab = "Expected growth (%)", ylab = "Density", border = NA,
     col = rgb(200, 50, 50, maxColorValue = 255))
polygon(density(clean_data$Expected.Growth.Next.5.Years),
        border = NA, col = rgb(200, 50, 50, maxColorValue = 255))

# Scatter plot of Current PE to Expected Growth in the Next 5 Years
plot(clean_data$Current.PE, clean_data$Expected.Growth.Next.5.Years,
     col = rgb(50, 200, 200, 100, maxColorValue = 255), pch = 16,
     bg = rgb(200, 200, 200, maxColorValue = 255), xlab = "Current PE",
     ylab = "Expected growth (%)", main = "Current PE vs Expected Growth")

# Make a linear regression of Current PE to Expected Growth in the Next 5 Years
fit1 <- lm(Expected.Growth.Next.5.Years ~ Current.PE, data = clean_data)
abline(fit1, col = rgb(50, 200, 200, maxColorValue = 255))
summary(fit1)

# Remove outliers and find the linear regression of Current PE to Expected
# Growth in the Next 5 Years
sorted_pe <- sort(clean_data$Current.PE, decreasing = TRUE,
                  index.return = TRUE)[[2]]
sort_pe <- sort(clean_data$Current.PE, decreasing = TRUE)
sorted_growth2 <- clean_data$Expected.Growth.Next.5.Years[sorted_pe]
sorted_pe_growth <- data.frame(pe = sort_pe, growth = sorted_growth2)
dat4 <- subset(sorted_pe_growth, sort_pe < 300)
plot(dat4$pe, dat4$growth, main = "Current PE Ratio vs. Expected Growth",
     xlab = "Current PE ratio", ylab = "Expected growth (next 5 years)",
     col = rgb(200, 50, 200, 100, maxColorValue = 255), pch = 16)
fit2 <- lm(growth ~ pe, data = dat4)
abline(fit2, col = rgb(200, 50, 200, maxColorValue = 255))
summary(fit2)

# Account for beta when sorting PE ratios and growth by taking the PE ratios
# and growths of industries with lower beta
beta_pe <- clean_data$Current.PE[sorted_beta]
beta_growth <- clean_data$Expected.Growth.Next.5.Years[sorted_beta]
end <- length(beta_pe)
cutoff <- round(end/2)
beta1 <- data.frame(pe = beta_pe[1:cutoff], growth = beta_growth[1:cutoff])
plot(beta1$pe, beta1$growth,
     main = paste("Current PE Ratio vs. Expected Growth",
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
plot(beta2$pe, beta2$growth,
     main = paste("Current PE Ratio vs. Expected Growth",
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
    ggtitle("Plot of Current PE to Expected Growth") + theme_classic()

# Make a bar plot of Current PE to Expected Growth for industries with
# pe, p, fulfilling p < 300
ggplot(dat4, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity", color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") + xlab("Current PE ratio") +
    ylab("Expected growth (%)") + theme_classic()

# Make a bar plot of Current PE to Expected Growth for industries with
# pe, p, fulfilling 100 <= p < 200
dat5 <- subset(sorted_pe_growth, pe >= 100 & pe < 300)
ggplot(dat5, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity", color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") + xlab("Current PE ratio") +
    ylab("Expected growth (%)") + theme_classic()

# Make a bar plot of Current PE to Expected Growth for industries with
# pe, p, fulfilling p < 100
dat6 <- subset(sorted_pe_growth, pe < 100)
ggplot(dat6, aes(x = pe, y = growth)) + geom_line(color = "#ebcefb") +
    geom_bar(position = "identity", stat = "identity",
             color = "#000000") +
    ggtitle("Beta vs Expected Growth Rate") + xlab("Current PE ratio") +
    ylab("Expected growth (%)") + theme_classic()

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
dat7 <- subset(industry_pe_growth, industry_pe < 150)
symbols(dat7$pe, dat7$growth, circles = dat7$num_firms,
        inches=0.35, fg="white", bg="#facccc8d",
        xlab="Current PE", ylab="Expected Growth in 5 Years",
        main = paste("Current PE Less Than 150 vs",
                     "Expected Growth in the Next 5 Years"))
text(dat7$pe, dat7$growth, labels = dat7$num_firms, cex = 0.3)

# Find the multivariate regression of expected growth to unlevered beta
# and current PE ratio
reg2 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE, data = clean_data)
summary(reg2)

# Graph the scatterplot of current PE ratio versus expected growth
ggplot(clean_data, aes(x = Current.PE, y = Expected.Growth.Next.5.Years)) +
    geom_point() + xlab("Current PE Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg2)["Current.PE"]),
                    intercept = unname(coef(reg2)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Current PE Ratio vs Expected Growth") + theme_classic()

# Graph the scatterplot of the log of the current PE ratio to the growth rate
# with the corresponding multivariate regression
log_reg2 <- lm(Expected.Growth.Next.5.Years ~ log(Current.PE) +
                   Average.Unlevered.Beta, data = clean_data)
summary(log_reg2)
ggplot(clean_data, aes(x = log(Current.PE),
                       y = Expected.Growth.Next.5.Years)) +
    geom_point() + ggtitle("Scatterplot of PE Ratio vs Expected Growth") +
    xlab("Current PE Ratio (log)") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(log_reg2)["log(Current.PE)"]),
                    intercept = unname(coef(log_reg2)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    theme_classic()

# Remove the outliers of expected growth and current PE from the graph
indices_no_outliers <- sort(clean_data$Expected.Growth.Next.5.Years,
                            index.return = TRUE)[[2]]
growth_no_outliers <- unlist(sort(clean_data$Expected.Growth.Next.5.Years))
pe_no_outliers <- clean_data$Current.PE[indices_no_outliers]
dat8 <- data.frame(growth = growth_no_outliers, pe = pe_no_outliers)
dat8 <- subset(dat8, growth_no_outliers > 6.32 & pe_no_outliers < 300)

# Make a multivariate regression of growth to PE when there are no outliers
reg2_no_outliers <- lm(growth ~ pe , data = dat8)
summary(reg2_no_outliers)

# Plot the scatterplot and regression of current PE to expected growth
ggplot(dat8, aes(x = pe, y = growth)) + theme_classic() +
    geom_point() + xlab("Current PE Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg2_no_outliers)["pe"]),
                    intercept = unname(coef(reg2_no_outliers)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("Current PE Ratio's Impact on Expected Growth")

# Graph the scatterplot of PEG ratio's impact on expected growth and the
# regression line of expected growth to PEG ratio and the regression line
# of the multivariate regression of expected growth in the next 5
# years to average unlevered beta, the current PE ratio, and the PEG Ratio.
reg3 <- lm(Expected.Growth.Next.5.Years ~ Average.Unlevered.Beta +
               Current.PE + PEG.Ratio, data = clean_data)
summary(reg3)
reg4 <- lm(Expected.Growth.Next.5.Years ~ PEG.Ratio, data = clean_data)
summary(reg4)
ggplot(clean_data, aes(x = PEG.Ratio, y = Expected.Growth.Next.5.Years)) +
    geom_point(color = rgb(223, 84, 84, 100, maxColorValue = 255)) +
    xlab("PEG Ratio") + ylab("Expected Growth (%)") +
    geom_abline(aes(slope = unname(coef(reg3)["PEG.Ratio"]),
                    intercept = unname(coef(reg3)["(Intercept)"])),
                color = rgb(84, 233, 233, maxColorValue = 255)) +
    geom_abline(aes(slope = unname(coef(reg4)["PEG.Ratio"]),
                    intercept = unname(coef(reg4)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("PEG Ratio's Impact on Expected Growth") +
    coord_cartesian(ylim=c(0,40)) + theme_classic()

# Graph the scatterplot and regression of PEG ratio and expected growth
# accounting for outliers
peg_no_outliers <- clean_data$PEG.Ratio[indices_no_outliers]
dat9 <- data.frame(growth = growth_no_outliers, peg = peg_no_outliers)
dat9 <- subset(dat9, growth > 6.32 & peg < 2.5 & !is.na(peg))
reg4_no_outliers <- lm(growth ~ peg, data = dat9)
summary(reg4_no_outliers)
ggplot(dat9, aes(x = peg, y = growth)) + xlab("PEG Ratio") +
    ylab("Expected Growth (%)") + coord_cartesian(ylim=c(0,40)) +
    geom_point(color = rgb(223, 84, 84, 100, maxColorValue = 255)) +
    geom_abline(aes(slope = unname(coef(reg4_no_outliers)["peg"]),
                    intercept = unname(coef(reg4_no_outliers)["(Intercept)"])),
                color = rgb(223, 84, 84, maxColorValue = 255)) +
    ggtitle("PEG Ratio's Impact on Expected Growth") + theme_classic()

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
              angle = 100, scale.y = .3, pch = 16,
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

# Make 3D scatterplot of Expected Growth in the Next 5 Years, Current PE,
# and Average Unlevered Beta, then examine the graph from multiple angles
# Make 3D scatterplot of Expected Growth in the Next 5 Years, Current PE,
# and Average Unlevered Beta, then examine the graph from multiple angles
scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 30, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 100, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")

scatterplot3d(x = clean_data$Expected.Growth.Next.5.Years,
              y = clean_data$Current.PE, z = clean_data$Average.Unlevered.Beta,
              angle = 300, scale.y = .3, pch = 16,
              xlab = "Expected Growth in Next 5 Years",
              ylab = "Current PE Ratio", zlab = "Beta",
              color = rgb(83, 223, 223, 100, maxColorValue = 255),
              main = "3D Scatterplot of Expected Growth, PE Ratio, and Beta")
