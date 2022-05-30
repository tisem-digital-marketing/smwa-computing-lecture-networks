# script.R
# 
# What this file does:
#  - Introduce some network descriptive stats
#

# --- Libraries --- # 
library(readr)
library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(tibble)
library(igraph)

# --- Download the Data --- #

url <- "https://github.com/rfordatascience/tidytuesday/raw/master/tidytuesday_tweets/data.rds"
out_file <- "data/tt_tweets.rds"
download.file(url, destfile = out_file, mode = "wb")

# --- Load the data into R --- #

tweets <- read_rds('data/tt_tweets.rds')
