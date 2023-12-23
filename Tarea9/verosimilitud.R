# Datos de la muestra
x <- c(1.2, 0.75, -0.39, -0.01, 0.51)

# Función de verosimilitud
likelihood <- function(theta, x) {
  prod(1 / (sqrt(2 * pi) * theta) * exp(-x^2 / (2 * theta^2)))
}

# Valores de theta para evaluar
theta_values <- seq(0.1, 2, by = 0.01)
# Calcular L(theta) para cada valor de theta
likelihood_values <- sapply(theta_values, function(t) likelihood(t, x))

# Encontrar el estimador de máxima verosimilitud (EMV)
theta_hat <- theta_values[which.max(likelihood_values)]

# Graficar L(theta)
plot(theta_values, likelihood_values, type = "l", xlab = expression(theta), ylab = expression(L(theta)), 
     main = "Función de Verosimilitud L(θ)")
abline(v = theta_hat, col = "red", lty = 2)

text(theta_hat, 0.002, labels = paste("θ_hat =", round(theta_hat, 2)), pos = 4, col = "black")

# Imprimir el valor de theta que maximiza la verosimilitud
cat("El estimador de máxima verosimilitud (EMV) es:", theta_hat, "\n")
