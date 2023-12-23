# Instala y carga el paquete readr si no está instalado
# install.packages("readr")
library(readr)
library(GGally)

# Cargar el archivo CSV
data <- read_csv("penguins.csv")

# Resumen estadístico básico
summary(data)
str(data)

# Explorar columnas categóricas
species_counts <- table(data$species)
island_counts <- table(data$island)

# Crear un espacio gráfico más grande para mostrar todas las etiquetas
par(mar = c(10, 4, 4, 2) + 0.1)

# Gráfico de barras para especies con todas las etiquetas
barplot(species_counts, main = "Distribución de especies", xlab = "Especies", ylab = "Frecuencia",
        col = "lightblue", las = 2, cex.names = 0.7)

# Gráfico de barras para islas con todas las etiquetas
barplot(island_counts, main = "Distribución por isla", xlab = "Isla", ylab = "Frecuencia",
        col = "lightgreen", las = 2, cex.names = 0.7)

# Filtra las columnas numéricas que quieres incluir en el ggpairs
numeric_data <- data[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]

# Crea la matriz de gráficos con ggpairs
ggpairs(numeric_data)
