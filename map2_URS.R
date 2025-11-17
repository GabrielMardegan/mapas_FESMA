# mapa 2 - unidades regionais de saúde do Maranhão

# bibliotecas
library(geobr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(dplyr)

# dados dos estados brasileiros
estadosBR <- read_state(year = 2020, simplified = TRUE)

# unidades regionais de saúde (URS) do Maranhão
URS_MA <- subset(read_health_region(year = 2013, simplified = FALSE), abbrev_state == "MA")
URS_MA_linhas <- st_cast(URS_MA, "MULTILINESTRING") #convertendo para linhas

# Corrigir geometrias inválidas
URS_MA <- URS_MA %>%
  mutate(geom = st_make_valid(geom))  # Corrige problemas nos polígonos

URS_MA <- URS_MA %>% 
  mutate(centroid = st_centroid(geom),  # Criar centróides
         longitude = st_coordinates(centroid)[, 1],  # Extrair longitude
         latitude = st_coordinates(centroid)[, 2])   # Extrair latitude

#URSs atendidas na segunda etapa da FESMA
destacar <- c("Santa Inês", "Viana", "Zé Doca", "Pinheiro", "Rosário", "Chapadinha", "Itapecuru Mirim",
              "Caxias", "Barra do Corda", "Pedreiras", "Açailândia", "Bacabal")

URS_MA$destacada <- ifelse(URS_MA$name_health_region %in% destacar, "Sim", "Não")


## MAPA 2
ggplot() +
  geom_sf(data = estadosBR) +
  geom_sf(data = URS_MA, aes(fill = ifelse(destacada == "Sim", name_health_region, NA)), 
          color = "black", size = 0.5, alpha = 0.27, show.legend = FALSE) +
  scale_fill_viridis_d(na.value = "gray95", option = "turbo") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "aliceblue")) +
  geom_text_repel(data = URS_MA, aes(x = longitude, y = latitude, label = name_health_region), force = 2) +
  annotate(geom = "text", x = -48.3, y = -2, label = "Pará",
           fontface = "italic", color = "grey22", size = 4.5) +
  annotate(geom = "text", x = -48.5, y = -9.3, label = "Tocantins",
           fontface = "italic", color = "grey22", size = 4.5) +
  annotate(geom = "text", x = -43, y = -8, label = "Piauí",
           fontface = "italic", color = "grey22", size = 4.5) +
  geom_point(aes(x = -44.3, y = -2.5), color = "black", size = 3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), height = unit(2, "cm"),
                         width = unit(2, "cm"),
                         style = north_arrow_fancy_orienteering(
                           line_width = 1,         # Adjust line thickness
                           text_size = 10,         # Adjust text size
                           line_col = "black",     # Adjust line color
                           fill = c("black", "white")
                         )
  ) +
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.8, line_col = "black") + #escala
  coord_sf(xlim = c(-49.0, -41.5), ylim = c(-1.0, -10.0), expand = TRUE)

#a imagem utilizada na dissertação foi obtida a partir da opção zoom na aba 'plots', e depois foi realizada uma captura de tela
