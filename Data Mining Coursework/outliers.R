

ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$fixed.acidity)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = scale(wine$volatile.acidity)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$citric.acid)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$residual.sugar)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$chlorides)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$free.sulfur.dioxide)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$total.sulfur.dioxide)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$density)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$pH)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$sulphates)))
ggplot(wine,  aes(x = wine$quality))+
  geom_boxplot(mapping = aes(y = (wine$alcohol)))

