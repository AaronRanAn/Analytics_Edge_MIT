# No. 1 AN ANALYTICAL DETECTIVE

crime <- read.csv("./Week 1/mvtWeek1.csv", header = T)

View(crime)

str(crime)

max(crime$ID)

min(crime$Beat)

table(crime$Arrest)

table(crime$LocationDescription == "ALLEY")

crime[1,]

# Convert date and but do not varibale

DateConvert = as.Date(strptime(crime$Date, "%m/%d/%y %H:%M"))

crime$Month = months(DateConvert)

crime$Weekday = weekdays(DateConvert)

crime$Year = years(DateConvert)

crime$Date = DateConvert

sort(table(crime$Month)) # check for better answer

sort(table(crime$Weekday)) # check for better answer

table(crime$Month, crime$Arrest == T)

hist(crime$Date, breaks=100)

boxplot(Date ~ Arrest, data = crime)

crime2011 <- subset(crime, Year == 2001)

        table(crime2011$Arrest)

crime2007 <- subset(crime, Year == 2007)

        table(crime2007$Arrest)

crime2012 <- subset(crime, Year == 2012)

        table(crime2012$Arrest)

loc <- sort(table(crime$LocationDescription), decreasing = T)

        loc <- data.frame(loc)

        names(loc) <- c("location", "freq")

loc_top5 <- subset(crime, 
                   LocationDescription == "STREET" |
                   LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                   LocationDescription == "GAS STATION" |
                   LocationDescription == "DRIVEWAY - RESIDENTIAL" |
                   LocationDescription == "ALLEY"
                   )
dim(loc_top5)

loc_top5$LocationDescription = factor(loc_top5$LocationDescription)

tab <- table(loc_top5$Arrest == T, loc_top5$LocationDescription)

prop.table(tab, 2) # column percentage

crime_gas <- subset(crime, LocationDescription == "GAS STATION")

sort(table(crime_gas$Weekday), decreasing = T)

crime_drive <- subset(crime, LocationDescription == "DRIVEWAY - RESIDENTIAL")

sort(table(crime_drive$Weekday), decreasing = F)

# No.2 

