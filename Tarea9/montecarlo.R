library(ggplot2)

# Definir la función que queremos integrar
f <- function(x) {
  return( exp(-x*x)) 
}

# Definir los límites de integración
a <- -1 # Límite inferior
b <- 1 # Límite superior

# Número de muestras
n <- 500

# Generar muestras aleatorias  entre a y b
x_uniform1 <- runif(n, min = a, max = b)
x_uniform2 <- runif(n, min = a, max = b)
x_uniform3 <- runif(n, min = a, max = b)

# Evaluar la función en las muestras aleatorias generadas
y_uniform1 <- f(x_uniform1)
y_uniform2 <- f(x_uniform2)
y_uniform3 <- f(x_uniform3)

# Calcular la integral aproximada
integral_uniform1 <- rep(0, n)
integral_uniform2 <- rep(0, n)
integral_uniform3 <- rep(0, n)

for (i in 1:n) {
  integral_uniform1[i] <- (b - a) * mean(y_uniform1[1:i])
  integral_uniform2[i] <- (b - a) * mean(y_uniform2[1:i])
  integral_uniform3[i] <- (b - a) * mean(y_uniform3[1:i])
}

# Crear un gráfico para comparar la convergencia
df <- data.frame(
  Iteration = 1:n,
  Integral_Uniform1 = integral_uniform1,
  Integral_Uniform2 = integral_uniform2,
  Integral_Uniform3 = integral_uniform3
)

ggplot(df, aes(x = Iteration)) +
  geom_line(aes(y = Integral_Uniform1, color = "Uniform Distribution 1")) +
  geom_line(aes(y = Integral_Uniform2, color = "Uniform Distribution 2")) +
  geom_line(aes(y = integral_uniform3, color = "Uniform Distribution 3")) +
  labs(title = "Convergencia de Monte Carlo con Diferentes Distribuciones",
       x = "Iteración",
       y = "Valor Aproximado de la Integral") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()
