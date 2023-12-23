# Cargar bibliotecas
library(MASS)
library(ggplot2)

# Definir la media y la matriz de covarianza
mu <- c(2, 4, 6, 8) # Media para X
C1 <- matrix(c(1, 0.5, 0.5, 0.2, 0.5, 1, 0.3, 0.1, 0.5, 0.3, 1, 0.4, 0.2, 0.1, 0.4, 1), nrow = 4) # Matriz de covarianza 1
C2 <- matrix(c(1, 0.8, 0.3, 0.1, 0.8, 1, 0.2, 0.5, 0.3, 0.2, 1, 0.4, 0.1, 0.5, 0.4, 1), nrow = 4) # Matriz de covarianza 2
C_mala <- matrix(c(1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 1), nrow = 3)
# Simular muestras de X
set.seed(123) # Establecer semilla para reproducibilidad
Y <- matrix(rnorm(1000*4), ncol = 4) # Muestras de Y con distribución normal estándar
X1 <- mu + Y %*% t(chol(C1)) # Muestras de X con matriz de covarianza C1
X2 <- mu + Y %*% t(chol(C2)) # Muestras de X con matriz de covarianza C2
X_mala <-  mu + Y[, 1:3] %*% t(chol(C_mala))  # Muestras de X con la matriz de covarianza "mala"

# Visualizaciones de dispersión para diferentes elecciones de C
par(mfrow = c(1, 2)) # Dividir la ventana gráfica en 1 fila y 2 columnas
plot(X1[, 1], X1[, 2], main = "Dispersión para C1", xlab = "X1", ylab = "X2", pch = 16, col = "blue")
plot(X2[, 1], X2[, 2], main = "Dispersión para C2", xlab = "X1", ylab = "X2", pch = 16, col = "red")
plot(X_mala[, 1], X_mala[, 2], main = "Dispersión para C_mala", xlab = "X1", ylab = "X2", pch = 16, col = "purple")

# Visualizaciones adicionales utilizando ggplot2
df1 <- data.frame(X1)
df2 <- data.frame(X2)
df3 <- data.frame(X_mala)

ggplot(df1, aes(x = X1, y = X2)) + geom_point(color = "blue") + ggtitle("Dispersión para C1") +
  xlab("X1") + ylab("X2")

ggplot(df2, aes(x = X1, y = X2)) + geom_point(color = "red") + ggtitle("Dispersión para C2") +
  xlab("X1") + ylab("X2")

ggplot(df3, aes(x = X1, y = X2)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Gráfico de dispersión para datos de df3", x = "X1", y = "X2")
