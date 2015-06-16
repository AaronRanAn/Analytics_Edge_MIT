# DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES

CPS <- read.csv("./Week 1/CPSData.csv", header = T)

str(CPS)

sort(table(CPS$Industry), decreasing = T)

sort(table(CPS$State), decreasing = T)

table(CPS$Citizenship!="Non-Citizen")

table(CPS$Race, CPS$Hispanic)

# Check NA value

table(is.na(CPS$Industry))

# Determine the pattern of missing values

table(CPS$Region, is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))

table(CPS$Age, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

non_metro <- subset(CPS, (is.na(MetroAreaCode)==T))

metro <- subset(CPS, (is.na(MetroAreaCode)==F))

        table(CPS$State, is.na(CPS$MetroAreaCode))

# How many states had all interviewees living in a non-metropolitan area?

        # FALSE = 0

# How many states had all interviewees living in a metropolitan area?

        # TRUE = 0

dim(non_metro); dim(metro)

str(non_metro$State); str(metro$State)

table(non_metro$Region)

## tapply and mean fucntion

t <- sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

## INTEGRATING METROPOLITAN AREA DATA

MetroAreaMap <- read.csv("./Week 1/MetroAreaCodes.csv", header = T)

CountryCodes <- read.csv("./Week 1/CountryCodes.csv", header = T)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

table(is.na(CPS$MetroArea))

sort(table(CPS$MetroArea), decreasing = T)

t <- sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing = T)

t[1]

a <- sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean), decreasing = T)

b <- data.frame(key=names(a), value=a)

c <- subset(b, value > 0.2)

d <- sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = T), decreasing = T)

## INTEGRATING METROPOLITAN AREA DATA

CPS1 = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

table(is.na(CPS1$Country))

sort(table(CPS1$Country), decreasing = T)

NY <- subset(CPS1, MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")

table(NY$Country == "United States")

d <- sort(tapply(CPS1$Country == "Somalia", CPS1$MetroArea, sum, na.rm = T), decreasing = T)

d[1:5]
