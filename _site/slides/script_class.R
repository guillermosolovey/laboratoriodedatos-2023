library(tidyverse)
library(rpart)
library(rpart.plot)
library(parttree)
library(class)

## cargo datos
load("./temp/datos_class.RData")

## separo train-test
nobs = nrow(d)
ind.train = sample(x = c(F,T), size = nobs, replace = T, prob = c(20,80))
d.train = d[ind.train,]
d.test  = d[!ind.train,]

## exploratorio
ggplot(data = d.train, aes(x = X1, y = X2, color = class)) + 
  geom_point() + 
  theme_classic()

## arbol de decision
fit = rpart(class ~ X1 + X2, data = d.train, minsplit = 10)

# plot 1
ggplot(d.train, aes(y = X2, x = X1)) +
  geom_parttree(data = fit, alpha = 0.1, aes(fill = class)) + 
  geom_point(aes(col = class)) +
  theme_minimal()

# plot 2
rpart.plot(fit)

# predecir para un dato arbitrario
predict(fit, data.frame(X1=0.9, X2=13), type = "class")

# predecir 
d.test$pred = predict(fit, d.test, type = "class")

# accuracy
accu = mean(d.test$class == d.test$pred)
print(accu)

# matriz de confusion dentro de la muestra de entrenamiento
table(d.test$class, d.test$pred)

## knn
d.test$pred = knn(
  train = d.train[,c("X1", "X2")], 
  cl    = d.train$class, 
  test  = d.test[,c("X1", "X2")],
  k     = 5
)

# accuracy
accu = mean(d.test$class == d.test$pred)
print(accu)

# matriz de confusion dentro de la muestra de entrenamiento
table(d.test$class, d.test$pred)

