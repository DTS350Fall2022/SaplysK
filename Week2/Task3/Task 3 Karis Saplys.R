?iris
head(iris)

#Plot1

ggplot(iris) +
  geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species))

#Plot2

ggplot(iris) +
  geom_point(mapping = aes(x = Petal.Width, y = Petal.Length, color = Species, shape = Species)) +
  facet_wrap(~Species)

#Plot3

ggplot(iris) +
  geom_point(mapping = aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +
  geom_smooth(method = 'lm', mapping = aes(x = Petal.Length, y = Petal.Width))


#Plot4
ggplot(iris, mapping = aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(binwidth = 0.2, color = 'black')

#Driving question:
WHat is the correlation between petal length and petal width? Do the flowers with the longest petals also have the widest petals?
  
  My visualizations plot this relationship in several different ways, offering unique perspectives.


