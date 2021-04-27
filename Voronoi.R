library(sf)
library(raster)
library(dismo)
library(deldir)
library(viridis)
library(extrafont)
library(tidyverse)
library(here)
library(ggrepel)
library(cowplot)
library(ggsn)
library(ggsflabel)
library(RColorBrewer)
library(grid)

ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette = "Dark2")

station_list = c(01440032, 01341029, 01440009, 01441000, 52405000)

estacoes <- st_read(dsn = here("rede_hidrometeorologica", "estacoes.shp")) %>% 
    filter(Código %in% station_list[1:4]) %>% 
    dplyr::select(geometry, Estação, Código) %>% 
    st_transform(4674) %>% 
    mutate(Estação = as_factor(Estação))

estacoes_fluv <- st_read(dsn = here("rede_hidrometeorologica", "estacoes.shp")) %>% 
    filter(Código %in% station_list[5]) %>% 
    dplyr::select(geometry, Estação, Código) %>% 
    st_transform(4674) %>% 
    mutate(Estação = as_factor(Estação))

bacia <- st_read(dsn = here("bacias", "ribeirao_da_caveira.shp")) %>% 
    st_transform(st_crs(estacoes))

# create the voronoi object
voronoi <- estacoes %>% 
    st_geometry() %>% # to get sfc from sf
    st_union() %>% # to get a sfc of MULTIPOINT type
    st_voronoi(envelope = st_geometry(bacia)) %>% # NC sized Voronoi polygon
    st_collection_extract(type = "POLYGON") %>% # a list of polygons
    st_sf() %>% # from list to sf object
    st_intersection(bacia) %>% 
    bind_cols(tibble(Estação = c("1341029 - Ituaçu", 
                                 "1440032 - Areião",
                                 "1441000 - Santo Antônio",
                                 "1440009 - Lucaia"
                                 ))) %>% 
    dplyr::select(-c(nunivotto5))

bahia <- st_read(dsn = here("bahia", "Bahia.shp")) %>% 
    st_transform(4674) %>% 
    st_combine()

brasil <- st_read(dsn = here("brasil", "Brasil.shp")) %>% 
    st_transform(4674) %>% 
    st_combine()

paises <- st_read(dsn = here("paises", "GEOFT_PAIS.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -75.5, xmax = -33, ymin = -35, ymax = 6)) %>% 
    select(PAI_NM, geometry)

bahia_crop <- st_read(dsn = here("bahia", "29MUE250GC_SIR.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -41.4, xmax = -40.4, ymin = -15.0, ymax = -13.7))

brasil_crop <- st_read(dsn = here("brasil", "Brasil.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -47.4, xmax = -37, ymin = -19, ymax = -8))

cidades <- st_read(dsn = here("cidades", "capitais.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -47.4, xmax = -37, ymin = -19, ymax = -8))

rios <- st_read(dsn = here("rios", "rios.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -41.4, xmax = -40.4, ymin = -14.9, ymax = -13.7)) %>% 
    filter(REGIME == "Permanente", NOME != "<NA>") %>% 
    group_by(NOME) %>% 
    summarise(geometry = st_union(geometry),
              length = st_length(geometry)) %>% 
    filter(length > quantile(length, 0.95))

oceano_crop <- st_read(dsn = here("rios", "oceano.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -47.4, xmax = -37, ymin = -19, ymax = -8)) %>% 
    mutate(area = st_area(geometry)) %>% 
    arrange(area) %>% 
    filter(area == max(area))
    
labels_estados <- cbind(brasil_crop, st_coordinates(st_centroid(brasil_crop)))
labels_capitais <- cbind(cidades, st_coordinates(st_centroid(cidades)))
labels_municipios <- cbind(bahia_crop, st_coordinates(st_centroid(bahia_crop)))
labes_oceano <- cbind(oceano_crop, st_coordinates(st_centroid(oceano_crop)))

caveira <- st_read(dsn = here("rios", "rios.shp")) %>% 
    st_transform(4674) %>% 
    st_crop(c(xmin = -41.4, xmax = -40.4, ymin = -14.9, ymax = -13.7)) %>%
    filter(NOME %in% c("Ribeirão da Caveira", "Rio de Contas",
                       "Ribeirão da Mata")) %>% 
    st_intersection(bacia)


sr <- grobTree(textGrob("Sirgas 2000 UTM Zone 24S", x = 0.799,  y = 0.08, hjust = 0,
                       gp = gpar(col = "black", fontsize = 13, fontfamily = "Times New Roman")))

bac <- 
ggplot() +
    geom_sf(data = rios, color = "dodgerblue2", alpha = 0.4) +
    geom_sf(data = bacia, lwd = .75, fill = NA) + 
    geom_sf(data = bahia_crop, fill = NA, color = "grey", linetype = "dashed") +
    geom_sf(data = voronoi, aes(fill = Estação), alpha = .9, show.legend = T) +
    geom_sf(data = estacoes, aes(color = "Pluviométrica"), size = 6, fill = NA, key_glyph = "point") +
    geom_sf(data = estacoes_fluv, aes(color = "Fluviométrica"), size = 6, key_glyph = "point") +
    geom_sf(data = caveira, fill = NA, color = "navy", alpha = 0.8) +
    geom_sf_text_repel(data = bind_rows(estacoes, estacoes_fluv), 
                 aes(label = Código),
                 family = "Times New Roman",
                 size = 8,
                 color = "black", nudge_x = 0.025,
                 nudge_y = 0.025,
                 bg.color = "white", bg.r = 0.25,
                 segment.color = NA) +
    geom_sf_text_repel(data = bahia_crop, 
                 aes(label = NM_MUNICIP),
                 family = "Times New Roman",
                 size = 5,
                 color = "grey", 
                 nudge_x = 0.045, nudge_y = 0.028,
                 fontface = "italic",
                 alpha = 0.6,
                 segment.color = NA) +
    geom_sf_text_repel(data = caveira %>% 
                     group_by(NOME) %>%
                     summarise(NOME = unique(NOME)), 
                 aes(label = NOME),
                 family = "Times New Roman",
                 size = 7,
                 color = "black", nudge_x = 0.2,
                 nudge_y = 0.040,
                 bg.color = "white", bg.r = 0.25) +
    scale_discrete_manual(aesthetics = c("fill"),
                          values = c("#ffc800", "#0ac499", "#6500d9", "#ee0af2"),
                          name = "Área de Influência") + 
    theme_bw() +
    geom_rect(aes(xmin = -41.4, xmax = -40.4, ymin = -15.0, ymax = -13.7), color = "black", fill = NA) +
    theme(text = element_text(family = "Times New Roman"),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(vjust = 15, size = 17),
          axis.text.y = element_text(size = 17),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text = element_text(size = 21),
          legend.title = element_text(size = 22),
          legend.position = c(.57, .105),
          legend.box.background = element_rect(color = "black",fill = "white"),
          legend.box.margin = margin(0.2, 7.5, 0.1, 0.1,"cm")
          ) +
    annotation_custom(sr) +
    labs(colour = "Estações") +
    coord_sf(datum = 31984, expand = F) +
    ggsn::scalebar(x.min = -41, x.max = -40.43, 
                   y.min = -14.94, y.max = -13.5,
                   dist = 10, st.size = 5, height = 0.01, transform = TRUE, dist_unit = "km") +
    ggsn::north(bahia_crop, location = "topright", symbol = 3, scale = 0.08)

loc <- 
ggplot() +
    geom_rect(aes(xmin = -47.4, xmax = -37, ymin = -19, ymax = -8), color = "black", fill = "white") +
    geom_sf(data = oceano_crop, lwd = .55, fill = "turquoise2", color = "grey", alpha = 0.3) +
    geom_sf(data = brasil_crop, lwd = .55, fill = NA, color = "grey") +
    geom_sf(data = bahia, alpha = .6) +
    geom_sf(data = bacia, lwd = 2, fill = "#ab0349", color = "#ab0349") +
    geom_sf(data = cidades %>% filter(NOMEABREV != "Aracaju"), fill = NA, color = "blue", size = 4) +
    geom_sf(data = cidades %>% filter(NOMEABREV != "Aracaju"), fill = NA, color = "blue", size = 5.5, pch = 1) +
    coord_sf() +
    theme_bw() + 
    geom_text_repel(data = labels_estados %>% filter(UF != "DF" & UF != "PB"), aes(label = UF, x = X, y = Y), 
                    size = 6, bg.color = "white",
                    bg.r = 0.25, alpha = 0.6) +
    geom_text_repel(data = labels_capitais %>% filter(NOMEABREV != "Aracaju"),
                    aes(label = NOME, x = X, y = Y), size = 6,
                    nudge_x = -0.5, nudge_y = -0.4, bg.color = "white",
                    bg.r = 0.25) +
    geom_text_repel(data = labes_oceano,
                    aes(label = NOME, x = X, y = Y), size = 3.8, fontface = "italic",
                    nudge_x = 0.0, nudge_y = 1.3, bg.color = "white", bg.r = 0.25,
                    segment.color = NA) +
    theme(text = element_text(family = "Times New Roman"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank()
          ) +
    geom_rect(aes(xmin = -47.4, xmax = -37, ymin = -19, ymax = -8), color = "black", fill = NA)

cont <- ggplot() +
    geom_rect(aes(xmin = -75.5, xmax = -33, ymin = -35, ymax = 6), color = "black", 
              fill = "turquoise2", alpha = 0.3) +
    geom_sf(data = paises) +
    geom_sf(data = brasil, fill = "gray54") +
    geom_sf(data = bahia, fill = "#ab0349") +
    geom_sf_text_repel(data = brasil, 
                  aes(label = "Brasil"),
                  family = "Times New Roman",
                  size = 7,
                  color = "black", nudge_x = 9,
                  nudge_y = -2, bg.color = "white", bg.r = 0.25,
                  segment.color = NA, alpha = 0.6,) +
    theme(text = element_text(family = "Times New Roman"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank()
    )

ggdraw() +
    draw_plot(bac, 0, 0, 1, 1)  +
    draw_plot(cont, 0.190, 0.005, 0.258, 0.30) +
    draw_plot(loc, 0.169, 0.27, 0.30, 0.32)






