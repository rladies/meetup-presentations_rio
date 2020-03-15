# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#       Job - mapa R ladies               #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

# Carregando pacotes:
pacotes <- c("data.table",
             "dplyr",
             "stringr",
             "lubridate",
             "ggplot2",
             "ggmap",
             'readxl',   # ok
             'leaflet',
             'htmlwidgets')
lapply(X = pacotes, FUN = require, character.only = TRUE)

# Lendo as informações:
library(readxl)
infos <- as.data.frame(read_excel('Mapa_Rladies.xlsx',
                                  sheet = 'dados'))
head(infos)
nrow(infos)

# Gerando as coordenadas:
library(ggmap)
lat_long <- geocode(infos$Endereco)

length(which(is.na(lat_long$lon))) # 19
nrow(lat_long) # 71

# Criando etiqueta mapa:
infos$Etiqueta_mapa <- paste0('loja ',
                              infos$Cidade)
head(infos)

# # # # Fazendo o mapa! # # # #
# # Tipo 1 # # #
library(leaflet)

mapa_1 <- leaflet(data = infos) %>% addTiles() %>%
    addMarkers(~lon, ~lat, popup = ~as.character(Etiqueta_mapa))

mapa_2 <- leaflet(data = infos) %>% addTiles() %>%
    addMarkers(~lon, ~lat, label = ~as.character(Etiqueta_mapa))

# # Marker personalizado # #
# # Criando ?cones # #
icon.tipoA <- makeAwesomeIcon(icon = '', markerColor = 'red',
                              library = 'ion')
icon.tipoL <- makeAwesomeIcon(icon = '', markerColor = 'orange',
                              library = 'ion')
icon.tipoF <- makeAwesomeIcon(icon = '', markerColor = 'gray',
                              library = 'ion')

# # Mapa awesome! # #

mapa_3 <- leaflet(data = infos) %>% addTiles() %>%
  addAwesomeMarkers(lng = infos[which(infos$Status == 'tipo A'), 'lon'],
                    lat = infos[which(infos$Status == 'tipo A'), 'lat'],
                    label = as.character(infos[which(infos$Status == 'tipo A'),
                                               'Etiqueta_mapa']),
                    icon = icon.tipoA) %>%
  addAwesomeMarkers(lng = infos[which(infos$Status == 'tipo F'), 'lon'],
                    lat = infos[which(infos$Status == 'tipo F'), 'lat'],
                    label = as.character(infos[which(infos$Status == 'tipo F'),
                                               'Etiqueta_mapa']),
                    icon = icon.tipoF) %>%
  addAwesomeMarkers(lng = infos[which(infos$Status == 'tipo L'), 'lon'],
                    lat = infos[which(infos$Status == 'tipo L'), 'lat'],
                    label = as.character(infos[which(infos$Status == 'tipo L'),
                                               'Etiqueta_mapa']),
                    icon = icon.tipoL) %>%
  addLegend("bottomright",
            colors = c("red", "gray", "orange"),
            labels = c("tipo A", "tipo F", "tipo L"),
            title = "Status",
            opacity = 1)

library(htmlwidgets)
saveWidget(mapa_3, file = "Mapa_Lojas_Rladies_awesome3.html", selfcontained = TRUE)
