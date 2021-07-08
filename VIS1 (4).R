library(ggplot2)

economics = as.data.frame(economics)

p = ggplot(economics)

# 1. Probka danych i podsumowanie danych
head(economics)
summary(economics)

# Wykres 1 - Populacja na przestrzeni lat
p + geom_point(mapping = aes(x=date, y=pop, colour=pop)) + ggtitle("Populacja na przestrzeni lat")

# Wykres 2 - Wydatki osobiste na przestrzeni lat
p + geom_point(mapping = aes(x=date, y=pce, colour=pce)) + ggtitle("Wydatki osobiste na przestrzeni lat")

# Wykres 3 - Oszczędności na przestrzeni lat
p + geom_point(mapping = aes(x=date, y=psavert, colour=psavert)) + ggtitle("Oszczędności na przestrzeni lat")

# Wykresy 4 i 5 - Liczba bezrobotnych i czas bezrobocia na przestrzeni lat
p + geom_point(mapping = aes(x=date, y=unemploy, colour=date)) + ggtitle("Zmiana liczby osób bezrobotnych na przestrzeni lat")
p + geom_point(mapping = aes(x=date, y=uempmed, colour=uempmed)) + ggtitle("Zmiana czasu bezrobocia na przestrzeni lat")


# Wykres 6 - Wykres pudełkowy Procent oszczędności na przestrzeni lat 2000-2015
ggplot(economics, aes(x=date, y=psavert))+ 
  geom_boxplot(aes(fill = factor(format(date, format='%Y'))))+
  scale_x_date(date_breaks='1 year',
               labels = date_format("%Y"),
               guide = guide_axis(angle = 90),
               limits=as.Date(c('2000-01-01','2015-04-01')))+
  labs(title='Procent oszczędności na przestrzeni lat 2000-2015',
       x='Rok',
       y='Procent oszczędności',
       fill = 'Rok')


# Wykres 7 - Wykres ukazujący liczbę bezrobotnych oraz oszczędności na przestrzeni lat
ggplot(data=economics)+
  geom_bar(aes(x=date,y=unemploy),stat="identity",fill="lightgrey")+
  geom_line(aes(x=date,y=psavert*1000),color="Purple")+
  scale_y_continuous(
    name="Bezrobocie",
    sec.axis = sec_axis(~./1000,name="PSAVERT")
  )
