library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)

#################### Part 1: Climate Change


cc <- read_csv("../data/week 2/climate_change.csv")

cc %>% 
        filter(Year <= 2006) -> cc_train

cc %>% 
        filter(Year > 2006) -> cc_test

# Next, build a linear regression model to predict the dependent variable Temp, 
# using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables 
# (Year and Month should NOT be used in the model). Use the training set to build the model.

cc_train %>% 
        lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, .) -> fit_0

fit_0 %>% summary()

cc_train %>% 
        cor(.) %>% 
        as.data.frame() %>% View()

cc_train %>% 
        lm(Temp ~ MEI + TSI + Aerosols + N2O, .) -> fit_1 

fit_1 %>% summary()

lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, cc_train) -> fit_0

step(fit_0)

fit_lm = lm(Temp ~ MEI + CO2 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, data = cc_train)

fit_lm %>% summary()

y_hat = predict(fit_lm, newdata = cc_test)

y = cc_test$Temp

# calculate the R-square 

SSE = sum((y_hat - y)^2)
SST = sum((mean(cc_train$Temp) - y)^2)

1 - SSE/SST

#################### Part 2: READING TEST SCORES

pisa_train = read_csv("../data/week 2/pisa2009train.csv")
pisa_test = read.csv("../data/week 2/pisa2009test.csv")

dim(pisa_train)

pisa_train %>% 
        group_by(male) %>% summarise(mean(readingScore))

pisa_train %>% 
        summarise_each(funs(anyNA)) %>% 
        gather("col_names", "NA", grade:readingScore) %>% 
        filter(`NA`==T)

pisa_train %<>% na.omit()
pisa_test %<>% na.omit()

dim(pisa_train)
dim(pisa_test)
