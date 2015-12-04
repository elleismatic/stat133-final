# Set the working directory
setwd("~/Documents/UC\ Berkeley\ 2015-2016/Statistics\ 133/projects/final")

# Make sure that the directory to be created does not already exist
# Make directory "clean_data" to contain preprocessed data files
if (!dir.exists("clean_data")) {
    dir.create("clean_data")
}

# Make directory "code" to contain code used for preprocessing
# and analyzing data
if (!dir.exists("code")) {
    dir.create("code")
}

# Make directory "images" to contain images generated through
# our analysis
if (!dir.exists("images")) {
    dir.create("images")
}

# Make directory "raw_data" to contain raw data downloaded online
if (!dir.exists("raw_data")) {
    dir.create("raw_data")
}

# Make directory "report" to contain our write up and other final
# writings or presentations
if (!dir.exists("report")) {
    dir.create("report")
}

# Make directory "resources" to contain sources of our inspiration
if (!dir.exists("resources")) {
    dir.create("resources")
}