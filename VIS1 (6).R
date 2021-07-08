library(grid)

# Dane z LAB6
library(lattice)
data(Investment, package="sandwich")

# Dane z LAB8
library(ggplot2)
library(ggplot2movies)
library(dplyr)

data <- movies %>% select(-(Action:Short))
genres <- movies %>% select(Action:Short)
i <- which(rowSums(genres)==0)

data$genre <- names(genres)[max.col(genres)]
data$genre[i] <- 'Other'

# Sprawdzenie wartości w kolumnach budget,mpaa
data %>% count(budget, sort = TRUE)
data %>% count(mpaa, sort = TRUE)
# Pozbycie się kolumn budget,mpaa
data <- subset (data, select = -c(budget,mpaa))





# wykres 1
w1 <- splom(~Investment, auto.key=TRUE, aspect=0.5)

grid.newpage()
pushViewport(viewport(x=0, width=1, just="left"))
print(w1, newpage=FALSE)
popViewport()

# Panel 2
p1 <- xyplot(Investment[, "RealGNP"], pch = 19, ylab = "Real GNP")
p2 <- xyplot(Investment[, "RealInv"], pch = 19, ylab = "Real Investment")
p3 <- xyplot(Investment[, "RealInt"], pch = 19, ylab = "Real Interest Risk")
p4 <- xyplot(Investment[, "GNP"], pch = 19, ylab = "GNP")
p5 <- xyplot(Investment[, "Investment"], pch = 19, ylab = "Investment")
p6 <- xyplot(Investment[, "Price"], pch = 19, ylab = "Price")
p7 <- xyplot(Investment[, "Interest"], pch = 19, ylab = "Interest")

grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1, just="top")))
pushViewport(viewport(layout.pos.row=1))
print(p1,newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=2))
print(p2,newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.row=3))
print(p3,newpage=FALSE)
popViewport()


grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2, just="top")))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
print(p4,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
print(p5,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
print(p6,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
print(p7,newpage=FALSE)
popViewport()


# Wykres Investment+Interest od czasu

Investment <- as.data.frame(Investment)

Investment[,"Investment"] = Investment[,"Investment"]/10
p8 <- xyplot(Investment + Interest ~ seq(1963,1982,1), data = Investment, type="o", pch=1:1, auto.key = list(border=TRUE))

# Wykres Investment od Interest

p9 <- xyplot(Investment~Interest, data=Investment,
       panel=function(x, y) {
         panel.lmline(x, y)
         panel.xyplot(x, y)
       })

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1, just="top")))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
print(p8,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
print(p9,newpage=FALSE)
popViewport()



# Wykresy z LAB8


p10 <- ggplot(data) + geom_point(aes(x=length, y=rating)) +
  scale_y_continuous(name="Ocena") +
  scale_x_continuous(name="Długość filmu", limits=c(0, 360))

p11 <- ggplot(data, aes(x = year)) + 
  geom_bar(fill = "darkgrey", color = "black") +
  labs(x = "Rok", y = "Ilość filmów")

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1, just="top")))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
print(p10,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
print(p11,newpage=FALSE)
popViewport()



movies_under5h = data[which (data$length < 600),]
p12 <- qplot(length, votes, data=movies_under5h, colour=rating)

# 5. Wykres przedstawiający ilość wyprodukowanych filmów w zależności od gatunku
len = nrow(data)
p13 <- ggplot(data, aes(genre, len)) +
  geom_col(color = "darkgrey") + 
  labs(x = "Gatunek filmu", y = "Ilość filmów",
       title = "Liczba filmow w zaleznosci od gatunku") + coord_flip()


grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1, just="top")))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
print(p12,newpage=FALSE)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
print(p13,newpage=FALSE)
popViewport()




p14 <- ggplot(data) + geom_point(aes(x=rating, y=votes)) +
  facet_wrap(~ genre, nrow=1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1, just="top")))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
print(p14,newpage=FALSE)
popViewport()
