# ----- Paquetes -----

library(dplyr)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(htmltools)
library(radarchart)
library(htmlwidgets)

# Para fuentes
library(extrafont)
# font_import()
loadfonts(device = "win")

# ----- Carga de datos y descriptivo -----

load("canciones.RData")
head(canciones)

# Nombre de canción más corto
canciones %>% arrange(nchar(name)) %>% mutate(n = nchar(name)) %>% 
  head(3) %>% select(name, album, n)

# Nombre de canción más largo
canciones %>% arrange(desc(nchar(name))) %>% mutate(n = nchar(name)) %>% 
  head(3) %>% select(name, album, n)

# Canción más corta
canciones %>% arrange(duration) %>% head(3) %>% select(name, album, duration)

# Canción más larga
canciones %>% arrange(desc(duration)) %>% head(3) %>% select(name, album, duration)

# Canciones más positivas
canciones %>% arrange(desc(valence)) %>% head(3) %>% select(name, album, valence)

# Canciones menos positivas
canciones %>% arrange(valence) %>% head(3) %>% select(name, album, valence)

# Canciones más bailables
canciones %>% arrange(desc(danceability)) %>% head(3) %>% select(name, album, danceability)

# Canciones menos bailables
canciones %>% arrange(danceability) %>% head(3) %>% select(name, album, danceability)

# Canciones más enérgicas
canciones %>% arrange(desc(energy)) %>% head(3) %>% select(name, album, energy)

# Canciones menos enérgicas
canciones %>% arrange(energy) %>% head(3) %>% select(name, album, energy)

# Canciones más acousticness
canciones %>% arrange(desc(acousticness)) %>% head(3) %>% select(name, album, acousticness)

# Canciones menos acousticness
canciones %>% arrange(acousticness) %>% head(3) %>% select(name, album, acousticness)

# Canciones más en directo
canciones %>% arrange(desc(liveness)) %>% head(3) %>% select(name, album, liveness)

# Canciones menos en directo
canciones %>% arrange(liveness) %>% head(3) %>% select(name, album, liveness)

# ----- Características medias de cada disco -----
canciones %>%
  select(-c("id", "name", "album_id")) %>% 
  group_by(album) %>%
  summarise(across(everything(), mean)) 

# ----- Gráfico con características medias de cada disco -----

# Para geom_jitter, pero no es obligatorio
set.seed(314)

canciones %>%
  select(-c("id", "name", "album_id", "duration", "tempo", "speechiness")) %>% 
  group_by(album) %>%
  summarise(across(everything(), mean)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value, color = album, label = album)) + 
  geom_jitter(size = 2, height = 0, width = 0.075) +    
  ggtitle("Cualidades asignadas por Spotify para cada álbum de Los Planetas",
          "danielredondo.com") + 
  scale_y_continuous(name = "Porcentaje", expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, 10)) + 
  scale_x_discrete(name = "\nCualidades asignadas por Spotify",
                   labels = c("Positiva", "Bailable", "Enérgica", "Acústica", "Instrumental", "En directo")) +
  scale_color_manual(values = rainbow(9), name = "") +
  theme_bw() + 
  theme(
    text = element_text(family = "Perpetua"),
    axis.text = element_text(color = "black"), 
    axis.text.x = element_text(face = "bold"),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", hjust = .5),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = .5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename = "cualidades_medias_album.png", width = 8, height = 5)

# ----- Gráfico interactivo de radar -----

canciones %>%
  select(-c("id", "name", "album_id", "duration", "tempo", "speechiness")) %>% 
  group_by(album) %>%
  summarise(across(everything(), mean)) -> df_radar

# Mejorar etiquetas
df_radar[2:7] <- round(df_radar[2:7], 0)
names(df_radar)[2:7] <- c("Positiva", "Bailable", "Enérgica", "Acústica", "Instrumental", "En directo")

chartJSRadar(
  scores = df_radar,
  polyAlpha = 0,
  maxScale = 80,
  scaleLineWidth = 20,
  main = "Cualidades medias asignadas por Spotify",
  labelSize = 13,
  responsive = TRUE,
  colMatrix = grDevices::col2rgb(rainbow(7))) -> grafico

grafico

saveWidget(grafico, file = "los_planetas_radar_interactivo.html")

