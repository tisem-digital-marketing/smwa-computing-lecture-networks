# script.R
# 
# What this file does:
#  - Introduce some network descriptive stats
#  - Community detection
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

# --- Get Connections --- # 
connections <-
    tweets %>%
    filter(mentions_screen_name != 'NA') %>%
    select(from = screen_name, to = mentions_screen_name)

# unnest column to
connections <-
    connections %>%
    unnest_longer(to) %>%
    filter(from != to)

# --- Sample from the network --- #
# We'll start the sampling by drawing 250 random users
set.seed(42)

tweet_authors <-
    as_tibble(
        unique(connections$from)
    )

seed_users <-
    sample_n(tweet_authors, 250)

# find everyone seed user mentions
connections_sample <-
    connections %>%
    filter(from %in% seed_users$value)

first_step <- 
    unique(connections_sample$to)

# Add those folks to the seed users
sample_users <- 
    unique(c(seed_users$value, first_step))

# keep only tweets with to or from these users
edgelist <-
    connections %>%
    filter(
        from %in% sample_users,
        to   %in% sample_users
    ) %>%
    distinct()

# convert table of connections to network that is
# - undirected - direction of from-to doesnt matter
# - simple - doesn't count number of times mentioned
tg <-
    as_tbl_graph(edgelist) %>%
    convert(to_undirected) %>%
    convert(to_simple)

# --- Visualize network --- #

tg %>%
    ggraph(layout = 'fr') +
    geom_node_point() +
    geom_edge_link(alpha = 0.2) +
    theme_void()

# --- Properties of Network --- #

# number of nodes
gorder(tg)

# how many connections are there 
gsize(tg)

# max number of connections in a network? 
max_conn <- 0.5 * (gorder(tg) * (gorder(tg) - 1))

# network density
gsize(tg) / max_conn

edge_density(tg)

# How likely are adjacent nodes connected
transitivity(tg, type = 'undirected')

# --- Node Influentiality --- #
# Compute influentiality
tg <-
    tg %>%
    activate(nodes) %>%
    mutate(
        degree = centrality_degree(),
        betweenness = centrality_betweenness(),
        closeness = centrality_closeness(),
        eigen = centrality_eigen(),
        pagerank = centrality_pagerank()
    )

# return nodes to dataframe to look at measures
centrality_measures <- 
    tg %>%
    activate(nodes) %>%
    as_tibble()

## note: in lab you'll need to do similar and then compute ranks based on these measures

# --- Community Detection --- #
# Use louvain method
# alternatives: group_infomap(), group_walktrap() among others

tg <-
    tg %>%
    activate(nodes) %>%
    mutate(
        group_lv = group_louvain()
    )

# return nodes to a dataframe to look at grouping
grp_lv <-
    tg %>%
    activate(nodes) %>%
    as_tibble()

# How many groups?
grp_lv %>%
    summarise(max_grp = max(group_lv))

# How large are each Group?
grp_lv %>%
    group_by(group_lv) %>%
    count()

# --- Visualize communities in a network --- #

tg %>%
    # if you want to look at certain groups keep this bit
    activate(nodes) %>%
    filter(group_lv %in% c(1,2,3,4,5,6)) %>%
    # if not start from here
    ggraph(layout = 'fr') +
    geom_node_point(aes(color = as.factor(group_lv))) +
    geom_edge_link(alpha= 0.1) +
    theme_void()
