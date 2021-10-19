# ----- Paquetes -----

library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(highcharter)
library(htmlwidgets)

# Para fuentes
library(extrafont)
# font_import()
loadfonts(device = "win")

# ----- Carga de datos -----

load("canciones.RData")
head(canciones)

names(canciones)[5:12] <- c("Duración", "Tempo", "Positiva", "Bailable",
                            "Enérgica", "Acústica", "Instrumental", "En directo")

# ----- PCA -----

# Con duración y tempo
pca <- PCA(canciones[5:12], scale.unit = TRUE, ncp = 2)

# Con tempo, sin duración
#Kpca <- PCA(canciones[6:12], scale.unit = TRUE, ncp = 2)

# Sin tempo ni duración
#pca <- PCA(canciones[7:12], scale.unit = TRUE, ncp = 2)

# Resumen
summary(pca) # Las dos primeras PC explican el 54% de la varianza

# Opción para ggrepel
set.seed(314)
options(ggrepel.max.overlaps = 3)

# General
fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("gray", "royalblue", "blue"),
             repel = T) +
  theme_classic() +
  theme(title = element_blank(),
        text = element_text(family = "Perpetua", size = 14))
ggsave("pca_general.png", width = 8, height = 5)

# Por album
fviz_pca_biplot(pca, repel = TRUE, col.var = "blue", geom.var = c("arrow", "text"),
                label = "var", habillage = factor(canciones$album)) +
  theme_classic() +
  theme(title = element_blank(),
        text = element_text(family = "Perpetua", size = 14))
ggsave("pca_album.png", width = 8, height = 5)

# Opción para ggrepel
options(ggrepel.max.overlaps = 8)

# Enseñando algunas canciones
rownames(pca$ind$coord) <- canciones$name
fviz_pca_biplot(pca, label = c("ind", "var"), repel = TRUE,
                col.ind = "gray50", col.var = "blue", geom = "point") +
  geom_label_repel(mapping = aes(x = pca$ind$coord[,1], y = pca$ind$coord[,2], label = rownames(pca$ind$coord)),
                   family = "Perpetua", color = alpha("black", .5), label.size = NA, fill = NA) +
  theme_classic() +
  theme(title = element_blank(),
        text = element_text(family = "Perpetua"))

ggsave("pca_canciones.png", width = 8, height = 5)

# ----- Gráfico interactivo -----

df <- tibble(
  canciones$name,
  pca$ind$coord[, 1], pca$ind$coord[, 2])
names(df) <- c("cancion", "PCA1", "PCA2")

hchart(df, "point", hcaes(x = PCA1, y = PCA2, group = cancion), color = "darkblue",
       legendIndex = 140) %>% 
  hc_yAxis(title = list(text = "PCA2")) %>% 
  hc_xAxis(title = list(text = "PCA1")) %>% 
  hc_add_theme(hc_theme_ft()) %>% 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical"
    ) %>% 
  hc_plotOptions(
    line = list(
      marker = list(
        enabled = F,
        symbol = "triangle",
        enabledThreshold = 0,
        fillColor = "black",
        lineWidth = 2,
        lineColor = "black"
      )
    )
  ) -> grafico

for(i in 1:8){
  grafico <- grafico %>% 
    hc_add_series(data = rbind(c(0, 0), pca$var$coord[i,]), type = "line", color = rainbow(8)[i],
                  lineWidth = 3, name = rownames(pca$var$coord)[i], legendIndex = i)
}

grafico 

saveWidget(grafico, file = "los_planetas_PCA_interactivo.html")

