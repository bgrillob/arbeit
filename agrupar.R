library(plotly)
library(tidyverse)
library(cluster)
library(purrr)
data(mtcars)
# STRINGS ----
intTestar <- 2:5
# BASE
baseR <- mtcars %>%
     select(gear, cyl, hp)
baseR <- scale(baseR, center = T, scale = T)
# DETERMINAR ORDEM K POR SILHUETA
sil_width <- map_dbl(intTestar,  function(k){
  modelo <- pam(x = baseR, k = k)
  modelo$silinfo$avg.width
})
# MODELO COOM ORDEM ESCOLHIDA
modelo <- pam(x = baseR, k = which.max(sil_width))

# GRÁFICOS ----
  # GRÁFICOS: SILHUETA CLUSTER ESCOLHIDO ----
#sil_plot <- silhouette(modelo)
#plot(sil_plot)

  # GRÁFICOS: SILHUETA EM FUNÇAO DE K ----
sil_df <- data.frame(
  k = intTestar,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = intTestar)

  # GRÁFICOS: SCATTER 3D COM CLUSTERS ----
baseScatter <- data.frame(
  baseR, Cluster = as.factor(modelo$clustering)
)
plot_ly(x = baseScatter$gear, y = baseScatter$cyl, z = baseScatter$hp, type="scatter3d", mode="markers", color = baseScatter$Cluster)
