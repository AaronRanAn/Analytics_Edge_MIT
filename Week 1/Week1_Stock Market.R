# Problem Set 2: Stock Market

# Write a function to read 5 data into R

read_data <- function() {    # TBD
        
        
}


IBM <- read.csv("./Week 1/IBMStock.csv", header = T)

GE <- read.csv("./Week 1/GEStock.csv", header = T)

ProcterGamble <- read.csv("./Week 1/ProcterGambleStock.csv", header = T)

CocaCola <- read.csv("./Week 1/CocaColaStock.csv", header = T)

Boeing <- read.csv("./Week 1/BoeingStock.csv", header = T)

# Factor to Date Conversion! 

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

dim(IBM)

min(IBM$Date)

max(IBM$Date)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(CocaCola$StockPrice)

median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

# Visualizing Stock Dynamics

plot(StockPrice ~ Date, data = CocaCola, type="l", col = "red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2) # add line for P&G

abline(v=as.Date(c("2000-03-01")), lwd=2, col = "grey")

abline(v=as.Date(c("1983-01-01")), lwd = 2, col = "green")

# Visualizing 5 companies at all tiemes

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2) # add line for P&G

lines(IBM$Date, IBM$StockPrice, col = "black", lty = 2) # add line for IBM

lines(GE$Date, GE$StockPrice, col = "green", lty = 2) # add line for GE

lines(Boeing$Date, Boeing$StockPrice, col = "yellow", lty = 2) # add line for Boeing

abline(v=as.Date(c("2000-03-01")), lwd=2, col = "grey")

abline(v=as.Date(c("1995-01-01")), lwd=2, col = "pink")

abline(v=as.Date(c("2005-05-01")), lwd=2, col = "pink")

abline(v=as.Date(c("1997-09-01")), lwd=1, col = "blue")

abline(v=as.Date(c("1997-11-01")), lwd=1, col = "blue")

abline(v=as.Date(c("2004-01-01")), lwd=1, col = "blue")

abline(v=as.Date(c("2005-12-01")), lwd=1, col = "blue")

# Monthly Trend   # Use the tapply

mon_mean <- tapply(IBM$StockPrice, months(IBM$Date), mean, simplify = T)

mon_mean2 <- data.frame(key=names(mon_mean), value=mon_mean)

mon_mean2$avg_ind <- ifelse(mon_mean2$value > mean(mon_mean2$value), 1, 0)

mon_IBM <- mon_mean2[order(mon_mean2$value), ]

# Write a function to complete the process:

sort_mean <- function (data) {
        
        mon_mean <- tapply(data$StockPrice, months(data$Date), mean, simplify = T)
        
        mon_mean2 <- data.frame(key=names(mon_mean), value=mon_mean)
        
        mon_mean2$avg_ind <- ifelse(mon_mean2$value > mean(mon_mean2$value), 1, 0)
        
        mon_final <- mon_mean2[order(mon_mean2$value), ]
        
        mon_end <- subset(mon_final, key == "January" | key == "December")
        
        mon_end
        
}

sort_mean(GE)

sort_mean(CocaCola)

l <- list(sort_mean(GE), sort_mean(IBM), sort_mean(CocaCola), sort_mean(Boeing), sort_mean(ProcterGamble))



