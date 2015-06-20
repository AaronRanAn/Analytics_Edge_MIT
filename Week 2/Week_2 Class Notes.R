# Course Note

  # RMSE is good beacuase it is normalized by N and in the same unit as the dependent variable

wine <- read.csv("./Week 2/wine.csv", header = T)

wine_test <- read.csv("./Week 2/wine_test.csv", header = T)

fit1 <- lm(Price ~ HarvestRain + WinterRain, data = wine)

names(fit1)

summary(fit1)

SSE <- sum(fit1$residuals^2); SSE

RMSE <- sqrt(SSE/25); RMSE

cor(wine$HarvestRain, wine$WinterRain)

# Remove the variable one by one # High correlation could cause un-intuitive sign

predictTest = predict(fit1, newdata = wine_test)

SSE = sum((wine_test$Price - predictTest)^2)

SST = sum((wine_test$Price - mean(wine$Price))^2)

Rsq <- 1 - (SSE / SST); Rsq

# Moneyball

baseball <- read.csv("./Week 2/baseball.csv", header = T)

moneyball <- subset(baseball, Year < 2002)

moneyball$RD = moneyball$RS - moneyball$RA

plot(moneyball$RD, moneyball$W)

WinsReg = lm(W~RD, data = moneyball)

predict(WinsReg, data.frame(RD=99))

fit2 <- lm(RS ~ OBP + SLG + BA, data = moneyball)

fit3 <- lm(RS ~ OBP + SLG, data = moneyball)

predict(fit3, data.frame(OBP = 0.311, SLG =0.405))

fit4 <- lm(RS ~ OOBP + OSLG, data = moneyball)

predict(fit4, data.frame(OOBP = 0.297, OSLG =0.370))


