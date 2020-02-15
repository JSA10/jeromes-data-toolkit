
# Visualising site structure with tree map of website URLs


#install.packages("collapsibleTree")

library(tidyverse)
library(stringr)
library(collapsibleTree)
library(networkD3)



# Read in the csv (sourced using free version of screaming frog)
sofar_file <- read_csv("data_visualisation/sofar_url_all.csv", skip = 1)

# Remove query strings, remove all before + including .com, remove duplicates
sofar_tidied <- sofar_file %>%
    mutate(add = str_replace_all(Address, c("\\?.*" = ""))) %>%
    mutate(add = str_replace_all(add, ".*\\.com\\/", "")) %>%
    select(add, title_1 = `Title 1`) %>%
    distinct() %>%
    arrange(add)

sofar_tidied %>% head(50)


sofar_levels <- sofar_tidied %>%
    separate(add, sep = "\\/",
             into = paste0("level", 1:5)) %>%
    mutate_at(vars(contains('level')), list(str_to_lower))
sofar_levels


sofar_tree <- collapsibleTree(
    df = sofar_levels,
    hierarchy = paste0("level", 1:5),
    tooltip = TRUE,
    fill = "#64ABC2", # Let's use the highlight colour from sofar's site for parent nodes
    width = 1500,
    height = 1000,
    zoomable = TRUE
)
sofar_tree



