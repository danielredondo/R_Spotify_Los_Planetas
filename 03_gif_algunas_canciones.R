# ----- Paquetes -----

# install_github("tiagomendesdantas/Rspotify")
library(Rspotify)
library(dplyr)
library(ggplot2)
library(reshape2)
library(magick)

# ----- Tokens para la aplicación de Spotify -----

# Claves Spotify
app_name <- "[NOMBRE DE LA APLICACIÓN EN SPOTIFY]"
client_id <- "[CLIENT_ID DE LA APLICACIÓN EN SPOTIFY]"
client_secret <- "[CLIENT_SECRET DE LA APLICACIÓN EN SPOTIFY]"
keys <- spotifyOAuth(app_name, client_id, client_secret)

# ----- Función que realiza el gráfico -----

# Para fuentes
library(extrafont)
# font_import()
loadfonts(device = "win")

preparar_grafico <- function(id, corregir_nombre = ""){
  if(substr(id, 1, 14) == "spotify:track:") id <- substr(id, 15, nchar(id))
  song <- getFeatures(id, token = keys) %>%
    cbind(getTrack(id, token = keys)[, 2:8]) %>%
    select(id, name, artists, duration_ms, tempo,
           "Positiva" = valence,
           "Bailable" = danceability,
           "Enérgica" = energy,
           "Acústica" = acousticness,
           "Instrumental" = instrumentalness,
           "En directo" = liveness
           )
  
  song[, 5] <- round(song[, 5])
  song[, 6:11] <- round(song[, 6:11] * 100, 0)
  song <- melt(song, id.vars = c("id", "tempo", "duration_ms", "name", "artists"))
  
  print(paste0("La canción es ", song$name[1]))
  
  # Para corregir nombre en el gráfico si en Spotify no aparece correctamente (tildes, mayúsculas, ...)
  if(corregir_nombre != "") song$name[1] <- corregir_nombre
  
  ggplot(song) +
    geom_col(aes(x = variable, y = value), fill = "darkblue") +
    geom_text(aes(x = variable, y = value, label = paste0(value, "%")), vjust = -0.5) + 
    xlab("\nCualidades asignadas por Spotify") +
    ggtitle(paste0(song$artists[1], " - ", song$name[1]),
            "danielredondo.com") + 
    theme_classic() + 
    scale_y_continuous(name = "Porcentaje", expand = c(0, 0), limits = c(0, 100)) + 
    theme(
      text = element_text(family = "Perpetua"),
      axis.text = element_text(color = "black", face = "bold", size = 12), 
      axis.title.x = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(face = "italic", size = 10),
      plot.subtitle = element_text(face = "italic", hjust = 0.5)) -> grafico
  
  grafico
  ggsave(filename = paste0(song$artists[1], "-", song$name[1], ".png"), width = 6, height = 6)
  print(grafico)
}

# ----- Gráficos para 3 canciones -----

# Un buen día
preparar_grafico("spotify:track:53AdGgVPCP8P4CutTFKJ1V", corregir_nombre = "Un buen día") 

# Pesadilla en el parque de atracciones
preparar_grafico("spotify:track:1dU5WwKslqch2IkJWtbYcH", corregir_nombre = "Pesadilla en el parque de atracciones") 

# Linea 1
preparar_grafico("spotify:track:5aSIuW8wodePrH4gCSLqup", corregir_nombre = "Línea 1")

# ----- Hacer gif con los 3 gráficos -----

# https://www.datanovia.com/en/blog/easy-image-processing-in-r-using-the-magick-package/
pic1 <- image_read("Los Planetas-Un buen día.png")
pic2 <- image_read("Los Planetas-Línea 1.png")
pic3 <- image_read("Los Planetas-Pesadilla En El Parque De Atracciones.png")
image_resize(c(pic1, pic2, pic3)) %>%
  image_background('white') %>%
  image_animate(fps = 0.25) %>% 
  image_write(path = "losplanetas.gif", format = "gif", quality = 10)
