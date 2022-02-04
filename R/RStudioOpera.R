library(ggplot2)
library(dplyr)
library(opera)

DataMNH11 <- read.csv(file='ENGIE/FRMHN11.csv', sep = ';', dec = ",", stringsAsFactors=FALSE)
head(DataMNH11)

ForD = DataMNH11[1:3000,2]
head(ForD)

ForM = DataMNH11[1:3000,3]
head(ForM)


DataTime = DataMNH11[1:3000,1]
head(DataTime)

ActivePower = DataMNH11[1:3000,4]
head(ActivePower)

Train <- data.frame(ExpertM = ForM, ExpertD = ForD)
head(Train)

plot(ForD, type="l", col='green')
lines(ForM, type="l", col='orange')
lines(ActivePower, type="l", col='royalblue3')

Y <- ActivePower
X <- cbind(ForM, ForD)
matplot(cbind(Y, X), type="l", col =1:6)

oracle.convex <- oracle(Y = Y, experts = X, loss.type = "square", model = "convex")
print(oracle.convex)
plot(oracle.convex)


MLpol0 <- mixture(model = "MLpol", loss.type = "square")

MLpol <- MLpol0

for (i in 1:length(Y)) {
  MLpol <- predict(MLpol, newexperts = X[i, ], newY = Y[i])
}

summary(MLpol)

plot(MLpol, pause = TRUE)
