library(lattice)

data(Investment, package="sandwich")

# probka danych i podsumowanie
head(Investment)
summary(Investment)

Investment <- as.data.frame(Investment)

splom(~Investment, auto.key=TRUE, aspect=0.5)

# Wykresy każdej zmiennej od czasu

p1 <- xyplot(Investment[, "GNP"], pch = 19, ylab = "GNP")
p2 <- xyplot(Investment[, "Investment"], pch = 19, ylab = "Investment")
p3 <- xyplot(Investment[, "Price"], pch = 19, ylab = "Price")
p4 <- xyplot(Investment[, "Interest"], pch = 19, ylab = "Interest")
p5 <- xyplot(Investment[, "RealGNP"], pch = 19, ylab = "Real GNP")
p6 <- xyplot(Investment[, "RealInv"], pch = 19, ylab = "Real Investment")
p7 <- xyplot(Investment[, "RealInt"], pch = 19, ylab = "Real Interest Risk")
print(p1, split = c(1, 1, 2, 2), more = TRUE)
print(p2, split = c(2, 1, 2, 2), more = TRUE)
print(p3, split = c(1, 2, 2, 2), more = TRUE)
print(p4, split = c(2, 2, 2, 2), more = FALSE)
print(p5, position=c(0, 2/3, 1, 1), more=TRUE)
print(p6, position=c(0, 1/3, 1, 2/3), more=TRUE)
print(p7, position=c(0, 0, 1, 1/3), more=TRUE)


# Wykres Investment+Interest od czasu

Investment[,"Investment"] = Investment[,"Investment"]/10
print(xyplot(Investment + Interest ~ seq(1963,1982,1), data = Investment, type="o", pch=1:1, auto.key = list(border=TRUE)))



# Wykres Investment od Interest

xyplot(Investment~Interest, data=Investment,
       panel=function(x, y) {
         panel.lmline(x, y)
         panel.xyplot(x, y)
       })
