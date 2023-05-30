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
library(graphlayouts)

# --- Load the data into R --- #

tweets <- read_rds('data/3437.edges')

# --- Convert to tidygraph --- #


# --- Plot the network --- # 

bb <- layout_as_backbone(tg, keep = 0.4)
E(tg)$col <- FALSE
E(tg)$col[bb$backbone] <- TRUE


# --- Properties of Network --- #


# --- Node Influentiality --- #



# --- Community Detection --- #
