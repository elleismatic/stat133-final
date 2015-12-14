# Set up ggplot2
library(ggplot2)

# Set up readr
library(readr)

# Set up scatterplot3d
library(scatterplot3d)

# Set up stringr
library(stringr)

# Set correct working directory again
setwd("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/")

# Read data files
clean_data <- read.csv("clean_data/clean_data.csv", header = TRUE,
                       row.names = 1, stringsAsFactors = FALSE)
industries_only <- read.csv("clean_data/industries_only.csv", header = TRUE,
                            row.names = 1, stringsAsFactors = FALSE)

# Analyze which industries are in the highest and lowest beta interval
beta_intervals <- seq(from = 0, to = 1.5, by = 0.3)

# Places an input of beta into the correct and corresponding bin
# within beta_intervals
# input: beta (numeric) - a value specifying a certain beta
# output: correct bin (numeric) - a number 1 - 5 corresponding to the bin
interval_fun <- function(beta) {
    for (i in 1:5) {
        if (beta_intervals[i] < beta & beta < beta_intervals[i + 1]) {
            return (i)
        }
    }
}

# Place the average unlevered betas into correct bins of
# beta_intervals
beta_interval <- unlist(lapply(clean_data$Average.Unlevered.Beta,
                               FUN = interval_fun))

# Return a vector of the industries that are in a specified beta
# interval (or bin)
# input: beta_num (numeric) - the specified beta interval or bin
# output: industries (vector) - industries under the specified bin
beta_vector <- function(beta_num) {
    as.vector(clean_data$Industry[unlist(beta_interval) == beta_num])
}

# Test cases
beta_vector(5)
beta_vector(1)

# Return the percentage of total industries with a PEG ratio less than
# a specified value
# input: value (numeric) - a specified PEG ratio
# output: percent (numeric) - percent of total industries with PEG less
#                             than the input value
peg_percent_indus <- function(value) {
  peg_ratios <- clean_data$PEG.Ratio[!is.na(clean_data$PEG.Ratio)]
  length(peg_ratios[peg_ratios < value]) / length(clean_data$Industry)
}
  
# Testing the function
peg_percent_indus(2)
peg_percent_indus(1.5)
peg_percent_indus(1)
peg_percent_indus(.5)
peg_percent_indus(0)

# Return the industries that have higher magnitude of expected growth
# than a specified standard deviation
# input: num_sd (numeric) - a specified standard deviation
# output: industries (vector) - industries that have an absolute
#                               growth rate above the specified
#                               standard deviation
industry_growth_outliers <- function(num_sd) {
    abs_growth <- abs(clean_data$Expected.Growth.Next.5.Years)
    mean_growth <- mean(clean_data$Expected.Growth.Next.5.Years)
    std_growth <- sd(clean_data$Expected.Growth.Next.5.Years)
    if(num_sd >= 0 & num_sd < 4) {
        return (clean_data$Industry[abs_growth >
                                        (mean_growth +
                                             num_sd * std_growth)])
    } else {
        stop(paste("Please provide standard deviations",
                   "between the values 0 and 4 as",
                   "they provide more insight."))
    }
}

# Test cases
industry_growth_outliers(1)
industry_growth_outliers(2.2)
industry_growth_outliers(-0.83)

# Remove the outliers of two vectors and return the result as a data frame.
# input: x (vector) - input to be sorted
#        y (vector) - second input to be sorted
#        sort_x (logical) - whether to by sort x
#        sort_y (logical) - whether to by sort y
#        num_sd (int) - cutoff standard deviation for sorting
# output: removed outliers (data.frame) - two-column data frame that only
#                                         contains corresponding rows that
#                                         do not contain specified outliers
remove_outliers <- function(x, y, sort_x = TRUE, sort_y = TRUE,
                            num_sd = 2) {
    sorted_x <- sort(x, index.return = TRUE)[[2]]
    sort_x <- sort(x)
    sort_y <- y[sorted_x]
    sort_x_y <- data.frame(x = sort_x, y = sort_y)
    xm <- mean(x)
    xstd <- sd(x) * num_sd
    ym <- mean(y)
    ystd <- sd(y) * num_sd
    if (!sort_x && !sort_y) {
        stop("You have to sort by x or by y or both.")
    } else if (sort_x && sort_y) {
        dat <- subset(sort_x_y, sort_x < (xm + xstd) & sort_x > (xm - xstd) &
                          sort_y < (ym + ystd) & sort_y > (ym - ystd))
    } else if (sort_x) {
        dat <- subset(sort_x_y, sort_x < (xm + xstd) & sort_x > (xm - xstd))
    } else if (sort_y) {
        dat <- subset(sort_x_y, sort_y < (ym + ystd) & sort_y > (ym - ystd))
    }
    return (dat)
}

# Test cases
remove_outliers(clean_data$Average.Unlevered.Beta,
                clean_data$Expected.Growth.Next.5.Years, num_sd = 1)

# Check our test case
mean(clean_data$Average.Unlevered.Beta) +
    sd(clean_data$Average.Unlevered.Beta)
mean(clean_data$Average.Unlevered.Beta) -
    sd(clean_data$Average.Unlevered.Beta)
sort(clean_data$Average.Unlevered.Beta)

# Create a graph of a specified type and give users the option of 
# saving the generated image in PNG and PDF format
# input: type (func) - function for high-level graphics
#        main (string) - title for plot
#        xlab (string) - label for x axis
#        ylab (string) - label for y axis
#        col (string) - color of the graph
#        ... (varies) - other input parameters for graph
#        save (logical) - whether to save graph
# output: if saved (logical) - returns if it saved
graph <- function(type, main = "", xlab = "", ylab = "",
                  col = "#000000", ..., save = FALSE) {
    if (save) {
        png(file = paste0("images/", main, ".png"))
        type(..., main = main, xlab = xlab, ylab = ylab,
             col = col)
        dev.off()
        
        pdf(file = paste0("images/", main, ".pdf"))
        type(..., main = main, xlab = xlab, ylab = ylab,
             col = col)
        dev.off()
    } else {
        type(..., main = main, xlab = xlab, ylab = ylab,
             col = col)
    }
    return (save)
}

# Test case - make a scatter plot and regression of
# beta to growth accounting for outliers
sorted_beta <- sort(clean_data$Average.Unlevered.Beta,
                    index.return = TRUE)[[2]]
sort_beta <- sort(clean_data$Average.Unlevered.Beta)
sorted_growth1 <- clean_data$Expected.Growth.Next.5.Years[sorted_beta]
sorted_beta_growth <- data.frame(beta = sort_beta, growth = sorted_growth1)
dat3 <- subset(sorted_beta_growth, sorted_growth1 > 0)
graph(type = plot, dat3$growth, dat3$beta,
     col = rgb(70, 150, 80, 150, maxColorValue = 255), pch = 16,
     bg = rgb(200, 200, 200, maxColorValue = 255), save = TRUE,
     main = "function_test_scatterplot_beta_vs_growth",
     xlab = "Expected growth (%)", ylab = "Average unlevered beta")
