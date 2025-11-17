# mapa 3 - macrorregionais de saúde do Maranhão

#bibliotecas
library(ggplot2)
library(geobr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(dplyr)

# dados dos estados brasileiros
estadosBR <- read_state(year = 2020, simplified = TRUE)

# macrorregionais de saúde do Maranhão
macro_MA <- subset(read_health_region(year = 2013, macro = TRUE, simplified = FALSE), abbrev_state == "MA")

#alterando o nome das macrorregiões
macro_MA <- macro_MA %>%
  mutate(name_health_macroregion = case_when(
    name_health_macroregion == "MACRORREGIAO SUL" ~ "Macrorregião SUL",
    name_health_macroregion == "MACRORREGIAO NORTE" ~ "Macrorregião NORTE",
    name_health_macroregion == "MACRORREGIAO LESTE" ~ "Macrorregião LESTE",
    TRUE ~ name_health_macroregion  # mantém os nomes que não foram alterados
  ))

#centróide das macrorregiões (para rotular os nomes)
macro_MA_centroids <- st_point_on_surface(macro_MA)

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


## MAPA 3
ggplot() +
  geom_sf(data = estadosBR) +
  geom_sf(data = macro_MA, aes(fill = name_health_macroregion), color = "black", alpha = 0.49, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  geom_sf_text(data = macro_MA_centroids,
               aes(label = name_health_macroregion),
               size = 3.8, color = "black", fontface = "bold") +
  geom_sf(data = URS_MA, fill = NA, color = "grey53", linetype = "dashed") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "aliceblue")) +
  annotate(geom = "text", x = -48.3, y = -2, label = "Pará",
           fontface = "italic", color = "grey22", size = 4.5) +
  annotate(geom = "text", x = -48.5, y = -9.3, label = "Tocantins",
           fontface = "italic", color = "grey22", size = 4.5) +
  annotate(geom = "text", x = -43, y = -8, label = "Piauí",
           fontface = "italic", color = "grey22", size = 4.5) +
  geom_point(aes(x = -44.3, y = -2.5), color = "black", size = 3) +
  annotate(geom = "text", x = -43.9, y = -2.1, label = "São Luís",
           fontface = "bold", color = "black", size = 4, angle = 30) +
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