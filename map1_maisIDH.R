# mapa 1 - municípios MAIS IDH

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

# filtrando o estado do Maranhão
maranhao <- estadosBR[estadosBR$code_state == "21", ]

# filtrando municipios do Maranhão
cidades_MA <- read_municipality(code_muni = 21, year = 2022, simplified = TRUE)

# capitais dos estados
capitais <- read_capitals(as_sf = TRUE)

# conjunto dos municípios do Plano Mais IDH
maisIDH <- c("Lagoa Grande do Maranhão", "Marajá do Sena", "Jenipapo dos Vieiras", "Itaipava do Grajaú", "Fernando Falcão",
             "São Roberto", "Santa Filomena do Maranhão", "Arame", "Brejo de Areia", "Conceição do Lago-Açu",
             "Satubinha", "São Raimundo do Doca Bezerra", "Santana do Maranhão", "Aldeias Altas", "Araioses",
             "Milagres do Maranhão", "São João do Soter", "Belágua", "Água Doce do Maranhão", "Afonso Cunha",
             "Primeira Cruz", "São Francisco do Maranhão", "Santo Amaro do Maranhão", "São João do Carú", "Amapá do Maranhão",
             "Centro Novo do Maranhão", "Pedro do Rosário", "Cajari", "Serrano do Maranhão", "Governador Newton Bello")

## MAPA 1
ggplot() +
  geom_sf(data = estadosBR) +
  geom_sf(data = cidades_MA, aes(fill = name_muni %in% maisIDH), show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "gray95", "TRUE" = "salmon")) +
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

#a imagem utilizado na dissertação foi obtida a partir da opção zoom na aba 'plots', e depois foi realizada uma captura de tela
