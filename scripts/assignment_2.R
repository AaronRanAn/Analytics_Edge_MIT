library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)

cc <- read_csv("../data/week 2/climate_change.csv")

cc %>% 
        filter(Year <= 2006) -> cc_train

iris %>% summarise(cor(Sepal.Length, Sepal.Width))

