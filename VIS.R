# ------------------------------------------------------------------LAB6 WYKRESY
library(lattice)
library(grid)
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

print(p2)
popViewport() 

grid.rect(x = unit(0.62, "npc"), y = unit(0.4, "npc"), 
          
          width = unit(0.08, "npc"), height = unit(.06, "npc"), 
          
          gp=gpar(col="green",  fill = NA, lwd = 2))
grid.rect(x = unit(0.83, "npc"), y = unit(0.75, "npc"), 
          
          width = unit(0.06, "npc"), height = unit(.06, "npc"), 
          
          gp=gpar(col="green",  fill = NA, lwd = 2))
grid.rect(x = unit(0.89, "npc"), y = unit(0.8, "npc"), 
          
          width = unit(0.08, "npc"), height = unit(.15, "npc"), 
          
          gp=gpar(col="red",  fill = NA, lwd = 2))

print(p7)
popViewport() 
grid.rect(x = unit(0.62, "npc"), y = unit(0.35, "npc"), 
          
          width = unit(0.15, "npc"), height = unit(.35, "npc"), 
          
          gp=gpar(col="red",  fill = NA, lwd = 2))


# Wykres Investment+Interest od czasu

Investment[,"Investment"] = Investment[,"Investment"]/10

p8 <- xyplot(Investment + Interest ~ seq(1963,1982,1), data = Investment, type="o", pch=1:1, auto.key = list(border=TRUE))


# Wykres Investment od Interest

p9 <- xyplot(Investment~Interest, data=Investment,
       panel=function(x, y) {
         panel.lmline(x, y)
         panel.xyplot(x, y)
       })
# -------------------------------------------------------------------------- LAB8

library(ggplot2)
library(ggplot2movies)
library(dplyr)


# Modyfikacja zbioru danych
# 1. Zamiana kolumn Action-Short na jedną kolumnę genre
data <- movies %>% select(-(Action:Short))
genres <- movies %>% select(Action:Short)
i <- which(rowSums(genres)==0)

data$genre <- names(genres)[max.col(genres)]
data$genre[i] <- 'Other'

head(data)

# Sprawdzenie wartości w kolumnach budget,mpaa
data %>% count(budget, sort = TRUE)
data %>% count(mpaa, sort = TRUE)
# Pozbycie się kolumn budget,mpaa
data <- subset (data, select = -c(budget,mpaa))

head(data)
# 2. Wykres przedstawiający ocenę filmu w zależności od jego długości

p <- ggplot(data)
p + geom_point(aes(x=length, y=rating)) +
  scale_y_continuous(name="Ocena") +
  scale_x_continuous(name="Długość filmu", limits=c(0, 360))

# 3. Wykres przedstawiający ilość wyprodukowanych filmów na przestrzeni lat
p100 <- ggplot(data, aes(x = year)) + 
  geom_bar(fill = "darkgrey", color = "black") +
  labs(x = "Rok", y = "Ilość filmów")

print(p100)
popViewport() 
grid.rect(x = unit(0.92, "npc"), y = unit(0.55, "npc"), 
          
          width = unit(0.12, "npc"), height = unit(.85, "npc"), 
          
          gp=gpar(col="green",  fill = NA, lwd = 2))



# 4. Wykres pokazujący zależność między ilością głosów oddawanych na filmy, a ich długością
movies_under5h = data[which (data$length < 600),]
qplot(length, votes, data=movies_under5h, colour=rating)

# 5. Wykres przedstawiający ilość wyprodukowanych filmów w zależności od gatunku
len = nrow(data)
ggplot(data, aes(genre, len)) +
  geom_col(color = "darkgrey") + 
  labs(x = "Gatunek filmu", y = "Ilość filmów",
       title = "Liczba filmow w zaleznosci od gatunku") + coord_flip()

# 6. Wykres panelowy

p <- ggplot(data)
print(
  p + geom_point(aes(x=rating, y=votes)) +
    facet_wrap(~ genre, nrow=1)
)
popViewport() 
grid.rect(x = unit(0.14, "npc"), y = unit(0.54, "npc"), 
          
          width = unit(0.105, "npc"), height = unit(.90, "npc"), 
          
          gp=gpar(col="blue",  fill = NA, lwd = 2))
grid.rect(x = unit(0.60, "npc"), y = unit(0.54, "npc"), 
          
          width = unit(0.105, "npc"), height = unit(.90, "npc"), 
          
          gp=gpar(col="blue",  fill = NA, lwd = 2))
grid.lines(x = unit(c(0.23, 0.18), "npc"), 
           
           y = unit(c(0.8, 0.9), "npc"), 
           
           gp = gpar(fill="magenta", col="magenta"), 
           
           arrow = arrow(length = unit(0.15, "inches"),  
                         
                         ends="last", type="closed")) 
grid.text("Best rated movie", .3, .78, gp=gpar(col="magenta")) 


