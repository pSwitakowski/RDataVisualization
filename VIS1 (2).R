library(BCA)
library(car)
library(corrplot)

data(Eggs)

# 1. Próbka danych
head(Eggs)

# 2. Macierz korelacji
data_Eggs <- data.frame(as.numeric(Eggs$Month), as.numeric(Eggs$First.Week), as.numeric(Eggs$Easter), Eggs$Cases, Eggs$Egg.Pr, Eggs$Beef.Pr, Eggs$Pork.Pr, Eggs$Chicken.Pr, Eggs$Cereal.Pr)
colnames(data_Eggs) <- c("Month", "First Week", "Easter", "Cases", "Egg.Pr", "Beef.Pr", "Pork.Pr", "Chicken.Pr", "Cereal.Pr")
b<-cor(data_Eggs)
corrplot(b)
corrplot(b, method="number")

# 3. Wykres słupkowy ilości sprzedanych opakowań po jajkach, a numer tygodnia w roku
eggs_df<-data.frame(Eggs$Week,Eggs$Cases)
bar<-barplot(names.arg=eggs_df$Eggs.Week, 
             height=eggs_df$Eggs.Cases, ylim = c(0,200000), 
             cex.names=0.7,
             col=ifelse(Eggs$Easter=="Pre Easter","lightblue",
                 ifelse(Eggs$Easter=="Easter","blue",
                 ifelse(Eggs$Easter=="Post Easter","red", "grey"))), 
             xlab = "Numer tygodnia",
             ylab = "Ilość sprzedanych opakowań po jajkach", 
             main = "Ilość sprzedanych jajek, a numer tygodnia")

# 4. Wykres cen poszczególnych produktów w czasie
prices <- cbind(Eggs$Egg.Pr, Eggs$Beef.Pr, Eggs$Pork.Pr, Eggs$Chicken.Pr, Eggs$Cereal.Pr)
names(prices) <- c("Eggs", "Beef", "Pork", "Chicken", "Cereal")

matplot(Eggs$Week, prices, pch=23, bg=c("green",  "yellow", "gray", "purple", "blue"), 
        type="o", col="black", ylim=c(40,  200), xlab='Numer tygodnia', ylab='Cena produktu')
legend('topleft', legend=c("Eggs", "Beef", "Pork", "Chicken", "Cereal"), 
       fill = c("green",  "yellow", "gray", "purple", "blue"))

# 5. Wykres pudełkowy ilości sprzedanych jajek w zależności od miesiąca
boxplot(data_Eggs$Cases~data_Eggs$Month,
        col="lightblue", xlab="Miesiąc",
        ylab= "Sprzedaż",
        main="Sprzedaż jajek w zależności od miesiąca.")

# 6. Ilość sprzedawanych jajek w poszczególnych okresach Wielkanocy (Przed, w trakcie i po Wielkanocy)
eggsPreEasterTime <- subset(Eggs, Easter=="Pre Easter")
numberOfCasesPreEaster <- sum(eggsPreEasterTime$Cases)

eggsEasterTime <- subset(Eggs, Easter=="Easter")
numberOfCasesEaster <- sum(eggsEasterTime$Cases)

eggsPostEasterTime <- subset(Eggs, Easter=="Post Easter")
numberOfCasesPostEaster <- sum(eggsPostEasterTime$Cases)

eggsCases = c(numberOfCasesPreEaster, numberOfCasesEaster, numberOfCasesPostEaster)

x <- barplot(eggsCases, main='Sprzedaż jajek w poszczególnych okresach Wielkanocy', col=c('lightblue', 'darkblue', 'lightgreen'),
             names=c("Pre Easter", "Easter", "Post Easter"), ylim=c(0, 600000), ylab='Sprzedaż jajek', xlab='Okres Wielkanocy')
text(x,y=eggsCases+20000,labels=as.character(eggsCases))

# Boxplot
Boxplot(Cases~Easter, data=Eggs)
