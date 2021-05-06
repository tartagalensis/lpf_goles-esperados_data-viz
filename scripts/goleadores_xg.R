### Goles-XGs TOP Scorers LPF ###
#Fecha 06/05/2021
#Autor: Franco Galeano

# Paquetes
library(cowplot)
library(ggrepel)
library(googlesheets4)
library(magick)
library(png)
library(tidyverse)

# Cargo data
googlesheets4::gs4_deauth()

goleadores <- read_sheet("https://docs.google.com/spreadsheets/d/1UDwZbQBGzpvTUwy7wD93VyxqobF4HEJjxH4mLUq4SFs/edit?usp=sharing") %>% 
  mutate(xg = as.numeric(gsub(",", ".", gsub("\\.", "", xg))),
         goles_sin_penales = (goles - penal)) %>% 
  print()

plot_xg <- goleadores %>% 
  select(jugador, Actuales = goles, goles_sin_penales, Esperados = xg, pos) %>%
  print()

# Comparativa arriba abajo ####
p1 <- plot_xg %>% 
  pivot_longer(cols = Actuales:Esperados, names_to = "tipo", values_to = "count") %>% 
  filter(tipo != "goles_sin_penales") %>% 
  ggplot(aes(count, pos)) +
  geom_point(color = "black", size = 4) +
  theme_linedraw() +
  labs(title = "",
       x = "Goles esperados (xG)") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 35),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 35),
        strip.text.y = element_text(size = 30),
        plot.caption = element_text(hjust = 0, face= "bold", size = 20),
        panel.grid.minor = element_blank(),
        text = element_text(family =  "Encode Sans Normal")) +
  geom_label_repel(size = 7, aes(label = jugador, family = "Encode Sans Wide"), vjust=1) +
  facet_grid(tipo ~ .)

# Scatter ####
p2 <- plot_xg %>% 
  ggplot(aes(Actuales, Esperados)) +
  geom_point(color = "white") +
  theme_linedraw() +
  labs(title = "") + 
  theme(axis.title.y = element_text(size = 35),
        axis.title.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.text.x = element_text(size = 35),
        strip.text.y = element_text(size = 30),
        plot.caption = element_text(hjust = 0, face= "bold", size = 20),
        panel.grid.minor = element_blank(),
        text = element_text(family =  "Encode Sans Normal")) +
  geom_label_repel(size = 7, aes(label = jugador, family = "Encode Sans Wide"), vjust=1)

# Agrego logos y exporto plots 
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("plots/1600px-Logo_Liga_Profesional_de_Fútbol_(Argentina).png")

# Creo un plot para el logo y la caption
p3 <- ggplot(mapping = aes(x = 0:1, y = 1)) +
  theme_void() +
  labs(caption = "Franco Galeano - @Tartagalensis") +
  theme(plot.caption = element_text(hjust = 0, vjust=1, size = 20),
        text = element_text(family =  "Encode Sans Normal")) +
  annotation_custom(l, xmin = .35, xmax = 1.2) 

# Uno logo y caption
p4 <- gridExtra::grid.arrange(p1, p3, heights = c(.53, .10))

#Exporto
ggsave(p4, filename = "xg_fecha12.png",
       width = 12, height = 10, units = "in", path = "plots/")


p4a <- gridExtra::grid.arrange(p2, p3, heights = c(.53, .10))

ggsave(p4a, filename = "xg_scatter_fecha12.png",
       width = 12, height = 10, units = "in", path = "plots/")
