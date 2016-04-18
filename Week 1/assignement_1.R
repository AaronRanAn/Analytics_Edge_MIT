### Assignment 1

######################### Part 1: Chicago Crime

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
        group_by(LocationDescription, Arrest) %>% 
        summarise(n=n()) %>% 
        left_join(
                mvt %>% 
                        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
                        group_by(LocationDescription) %>% 
                        summarise(n_sub = n())
        ) %>% 
        filter(Arrest==T) %>% 
        mutate(arst_rt = n/n_sub)

# Gas Station

mvt %>% 
        filter(LocationDescription == "GAS STATION") %>% 
        group_by(Weekday) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

# Saturday

mvt %>% 
        filter(LocationDescription == "DRIVEWAY - RESIDENTIAL") %>% 
        group_by(Weekday) %>% 
        summarise(n = n()) %>% 
        arrange(n)

# Saturday

######################### Part 2: Stock Market

library(lubridate)

ibm = read_csv("../data/week 1/IBMStock.csv", col_types = "cd")
ge = read_csv("../data/week 1/GEStock.csv", col_types = "cd")
pg = read_csv("../data/week 1/ProcterGambleStock.csv", col_types = "cd")
cc = read_csv("../data/week 1/CocaColaStock.csv", col_types = "cd")
bi = read_csv("../data/week 1/BoeingStock.csv", col_types = "cd")


ibm$brand = "ibm"; ge$brand = "ge"; pg$brand = "pg"; cc$brand = "cc"; bi$brand = "bi"

st = rbind(ibm, ge, pg, cc, bi)
st$Date = mdy(st$Date)

st %>% 
        group_by(brand) %>% 
        summarise(n=n()) # 480

min(st$Date); max(st$Date); # 1970 and 2009

st %>% 
       group_by(brand) %>% 
       summarise(avg_st = mean(StockPrice), # 144.37503
                 min_st = min(StockPrice), # 9.293636
                 max_st = max(StockPrice), # 146.5843
                 md_st = median(StockPrice), # 44.88340
                 sd_st = sd(StockPrice)) # 18.19414

st %>% 
        filter(brand == "cc") %>% 
        filter(StockPrice == max(StockPrice) | StockPrice == min(StockPrice))

# max 
# min


st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        geom_vline(xintercept = as.numeric(as.POSIXct("2000-03-01")), color = "grey", lwd = 3, alpha= 0.3)

ggsave("stock.png")

st %>% 
        filter(brand %in% c("cc", "pg")) %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        # geom_rect(aes(xmin=as.POSIXct("1983-01-01"), 
        #            xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha = 0.1), size = 0, fill = "green")
        annotate("rect", xmin=as.POSIXct("1983-01-01"), xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha=0.2, fill="red")   

ggsave("stock_zoom.png")

st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        geom_vline(xintercept = as.numeric(as.POSIXct("2000-03-01")), color = "blue", lwd = 3, alpha= 0.3)

st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        # geom_rect(aes(xmin=as.POSIXct("1983-01-01"), 
        #            xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha = 0.1), size = 0, fill = "green")
        annotate("rect", xmin=as.POSIXct("1995-01-01"), xmax=as.POSIXct("2005-12-01"), ymin=0, ymax=Inf, alpha=0.2, fill="red")

# use group_by and filter to filter within each group 

st %>% 
        group_by(brand) %>% 
        filter(StockPrice == max(StockPrice)) %>% 
        
        
