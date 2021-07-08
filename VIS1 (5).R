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
ggplot(data, aes(x = year)) + 
  geom_bar(fill = "darkgrey", color = "black") +
  labs(x = "Rok", y = "Ilość filmów")

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
