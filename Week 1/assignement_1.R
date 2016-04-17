### Assignment 1: Part 1

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)

mvt <- read_csv("../data/mvtWeek1.csv")

dim(mvt)
max(mvt$ID)
min(mvt$Beat)

View(mvt)

mvt %>% count(Arrest==T)
mvt %>% count(LocationDescription == "ALLEY")

head(mvt$Date)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

median(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

mvt %>% 
        count(Month) %>% 
        filter(n==min(n))

mvt %>% 
        count(Weekday) %>% 
        filter(n==max(n))

mvt %>% 
        filter(Arrest==T) %>% 
        group_by(Month) %>% 
        summarise(n=n()) %>% 
        filter(n==max(n))

mvt %>% 
  ggplot(aes(Date)) + 
        geom_histogram(bins = 100, alpha = 0.8) +
        fte_theme() 

mvt %>% 
        mutate(half = Year>2007) %>% 
        group_by(half) %>% 
        summarise(n=n())

mvt %>% 
        group_by(Year, Arrest) %>%
        filter(Year==2012) %>% 
        summarise(n=n())

mvt %>% 
        group_by(LocationDescription) %>% 
        summarise(n=n()) %>% 
        arrange(desc(n)) %>% 
        head(6)

mvt %>% 
        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
        dim()

mvt %>% 
        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
        group_by(LocationDescription) %>% 
        summarise(n=n(), 
                  n_arrest = count(Arrest=="TRUE"))




        
