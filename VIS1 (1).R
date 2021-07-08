# 1. Próbka danych
head(LifeCycleSavings)

# 2. Macierz korelacji zmiennych
pairs(LifeCycleSavings, panel=panel.smooth, main = "LifeCycleSavings")
cor(LifeCycleSavings)

# 3. Ilość oszczędności w zależności od wieku
plot(LifeCycleSavings[,1], LifeCycleSavings[,2], ylab="% ludzi poniżej 15 roku życia", xlab="Oszczędności")
plot(LifeCycleSavings[,1], LifeCycleSavings[,3], ylab="% ludzi powyżej 75 roku życia", xlab="Oszczędności")

# 4. Dochód dyspozycyjny w zależności od wieku
plot(LifeCycleSavings[,4], LifeCycleSavings[,2], ylab="% ludzi poniżej 15 roku życia", xlab="Dochód dyspozycyjny")
plot(LifeCycleSavings[,4], LifeCycleSavings[,3], ylab="% ludzi powyżej 75 roku życia", xlab="Dochód dyspozycyjny")

# 5. Ilość dochodu dyspozycyjnego
boxplot(LifeCycleSavings[,4], ylab="Dochód dyspozycyjny")
hist(LifeCycleSavings[,4],breaks=10, ylab="Częstość występowania", xlab="Dochód dyspozycyjny", main="")


# 6. Top państwa i ich dochód dyspozycyjny
top_dpi <- LifeCycleSavings[order(-LifeCycleSavings[,4]),]
top_dpi <- top_dpi[1:10,]
pie(top_dpi[,4], labels = rownames(top_dpi), main="Top 10 państw z największym dochodem dyspozycyjnym")

lowest_dpi <- LifeCycleSavings[order(LifeCycleSavings[,4]),]
lowest_dpi <- lowest_dpi[1:10,]
pie(lowest_dpi[,4], labels = rownames(lowest_dpi), main="Top 10 państw z najmniejszym dochodem dyspozycyjnym")

# 7. Dochód dyspozycyjny w zależności od współczynnika wieku społeczeństwa
plot(LifeCycleSavings[,3]/LifeCycleSavings[,2], LifeCycleSavings[,4], xlab="Współczynnik wieku społeczeństwa", ylab="Dochód dyspozycyjny")