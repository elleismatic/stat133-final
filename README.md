# Statistics 133 Final Project Fall 2015

<img src="http://www.hpcwire.com/wp-content/uploads/2011/02/financialmodeling.jpg" alt="Financial Modeling" height="400"/>

This project contains the source files required to reproduce the results in our Statistics 133 Final Project. This README will explain how to use these files. For full details, please visit `report/write_up.Rmd` or `report/write_up.pdf`.

## Directories and Files

This project contains the following folders:
- `clean_data`: contains preprocessed data files
- `code`: contains code used for preprocessing and analyzing our data
- `images`: contains images generated through our analysis
- `raw_data`: contains raw data downloaded from online
- `report`: contains our write up and other final writings or presentations
- `resources`: contains sources of our inspiration

This project contains the following files:
- `README.md`: gives an overview of this project
- `skeleton.R`: creates all directories in the project
- `clean_data/clean_data.csv`: contains the cleaned data with the row "Total Market"
- `clean_data/industries_only.csv`: contains the cleaned data without the row "Total Market"
- `code/preprocessing.R`: contains the code for preprocessing all data
- `code/analysis.R`: contains the code for analyzing our data
- `final.Rproj`: contains the RStudio Project for this project
- `images/*.pdf`: pdf version of images generated in `code/analysis.R`
- `images/*.png`: png version of images generated in `code/analysis.R`
- `raw_data/pe_data.csv`: PE ratio data from a CSV file downloaded online
- `raw_data/total_beta.csv`: beta data of different industries from a CSV file downloaded online
- `report/extra_credit_presentation.pptx`: final PowerPoint for extra credit
- `report/functions.Rmd`: an R markdown file that contains a summary of our functions
- `report/functions.pdf`: a PDF version of our `report/functions.Rmd`
- `report/write_up.Rmd`: an R markdown file that contains a synthesis of our write up and code
- `report/write_up.pdf`: a PDF version of our 'report/write_up.Rmd'
- `resources/inspirations.txt`: a plain text file of different sources that inspired the project

## Research Problem

Our findings and analysis were lead by our hypothesis:

- Technology industries show higher growth and higher beta, and therefore lower P/E ratios.

To test our hypothesis, we answered the following questions:

- What is the relationship between beta and the payout percentage?
- What is the relationship between beta and expected growth rate?

Before we begin introducing anything else, here are some financial terminologies that may be useful:

- **PE ratio:** market value per share divided by earnings per share (EPS). It is a ratio for valuing a company through its current share price relative to its per-share earnings
- **Forward PE:** a measure of PE using forcasted earnings as a part of the claculation. Often called the "estimated price to earnings", it is calculated using the market price per share over expected earnings per share
- **Trailing PE:** the most commonly used PE measure and is based on actual earnings, and therefore more accurate. It is calculated by dividing current share price by the trailing twelve months' earnings per share
- **Beta:** a measure of volatility or risk of a company or industry in comparison to the market as a whole. There are two types of betas: levered and unlevered. The unlevered beta is the beta of a company without any debt or the measure of risk when removing the financial effects from adding debt to a firm's capital structure (finances). Levered beta is the beta of a company as a whole when accounting for debt
- **PEG ratio:** a stock's PE ratio divided by the growth rate of its earnings for a specified time period. The ratio is used to determine a stock's value when taking into account a company's earnings growth and is considered to provide a more complete picture than PE
- **Payout ratio:** a proportion of earnings paid out as dividends to shareholders and is calculated by dividing divendens per share over earnings per share. It is known that the payout ratio is directly correlated with the PE ratio

## Data Sets

- [Total Beta](http://people.stern.nyu.edu/adamodar/New_Home_Page/datafile/totalbeta.html/)
- [PE Ratio](http://people.stern.nyu.edu/adamodar/New_Home_Page/datafile/pedata.html/)

## Preprocessing

We downloaded two datasets as CSV files from the NYU Stern Business School's data archives and placed it in our `raw_data` folder. Before we begin, we set working directory through the following R code. For this to work on your computer, you will need to change the path of your working directory to the appropriate one of the `final` folder on your own device.

```r
setwd("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final/")
```

We then extra raw data and merge the two files through the following code:

```{r}
# Extract raw data
beta <- read.csv("raw_data/total_beta.csv", header = TRUE,  stringsAsFactors = FALSE)
pe <- read.csv("raw_data/pe_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Combine data from the two files
raw_data <- merge(beta, pe, by = intersect(names(beta), names(pe)))
```

Next, we inspect our data through methods such as:

```{r}
# Inspect merged data
head(raw_data)
summary(raw_data)
names(raw_data)
str(raw_data)
```

Then, we make a copy of raw_data to a new data frame called "clean_data".

```{r}
# Duplicate raw_data to make edits
clean_data <- raw_data
```

Next, we renamed columns, converted character columns to numeric vectors, and removed extraneous rows with missing values. Because we have now cleaned our data set, we can now inspect clean_data through finding the summaries, plots, min and max values, and histograms. To see the actual code, please visit `code/preprocessing.R`.

Finally, we create CSV files for our cleaned data and placed it into the clean_data directory.

```{r}
# Create CSV files for clean data
file.create("clean_data/clean_data.csv")
write.csv(clean_data, file = "clean_data/clean_data.csv")
file.create("clean_data/industries_only.csv")
write.csv(industries_only, file = "clean_data/industries_only.csv")
```

## Methods & Analysis

To see our complete methods and analysis, please see `code/analysis.R`. You can alternatively visit `report/write_up.Rmd` or `report/write_up.pdf` to see a narrated version of our methods and analysis.

To give a brief overview, we used histograms, 2D and 3D scatterplots, bubble plots, and linear regressions to analyze relationships within our data.

- **Histograms:** Used to graphically display the distribution of a variable and its corresponding value to another variable
    - Looked at: Current PE, Forward PE, Total Levered Beta, Growth; PE Ratio to Average Unlevered Beta; Average Unlevered Beta to Expected Growth Rate; Current PE to Expected Growth Rate
    - We found that industries with high beta are growing industries such as online retail, real estate, and software and those with the lowest beta are stable markets such as finance or trucking
- **Bubble Plots and 2D Scatterplots:** Used to graphically display the relationship between two variables and add emphasis to a third
    - Graphed multiple charts, some of which controlled for outliers
    - Looked at: Current PE ratio to Expected Growth in the Next 5 Years, with circles of size corresponding to the size of the specific industry
    - We found that most industries had PE ratios that were below 150, but a few had extremely high Current PE, which skewed the data. When accounting for these outliers, we find that most larger industries have PE values between 50 and 100
- **Linear regressions:** Used to illustrate the correlation and relationship between variables
    - Had multiple scatterplots, some that controlled for outliers or other related variables to maintain accuracy
    - Looked at the scatterplots and linear regressions of: Current PE to Expected Growth in the Next 5 Years; Expected Growth to Average Unlevered Beta, Current PE, PEG Ratio
    - We found that there was a strong statistical significance of the PEG ratio to growth, as shown in the figure to the right
- **3D Scatterplots:** Used to analyze the relationship between multiple variables
Examined graph from multiple angles
    - Looked at the 3D plot of: Expected Growth in the Next 5 Years, PEG Ratio, and Average Unlevered Beta; Expected Growth in the Next 5 Years, Current PE Ratio, and Average Unlevered Beta
    - We found that higher beta corresponded with expected growth (as predicted), higher beta was correlated with higher PEG ratio, and that lower expected growth equated with higher PEG ratios

## Findings & Reports

Through our analysis, we tried to answer our hypothesis through our guiding questions, which we answer below.

- We found from our analysis that PE is negatively correlated with beta. That is, as beta increases, PE decreases. Thus, there is a negative correlation between beta and the payout ratio.
- Industries with higher beta also have higher growth rates, as we saw in our findings. Thus, industries such as technology -- software and online retail -- have higher growth but are less stable than industries such as finance, which also have a lower beta.

Our findings state that industries with higher beta also has higher growth. However, because higher beta is negatively correlated with payout ratio, then industries with higher beta also has a low PE. Thus, for the technology industry with a higher beta, it also has high growth, which also leads the industry to have a low PE value.

## Members
- Christian Alarcio: <christianalarcio@berkeley.edu>
- Ellen Chan: <ellenchan@berkeley.edu>
- Anais Sidhu: <asidhu@berkeley.edu>
- Ruomeng (Michelle) Yang: <michelleyang@berkeley.edu>

## License

The MIT License (MIT)

Copyright (c) 2015 Ruomeng (Michelle) Yang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
