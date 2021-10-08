library(pacman)
p_load(tidyverse, sf, janitor, classInt, Hmisc, cowplot, patchwork)

mapa_a <- read.csv("~/Downloads/19_nuevoleon/catalogos/localidades_urbanas_y_rurales_amanzanadas.csv",
                  sep=";", stringsAsFactors = F, check.names = F) %>% 
          clean_names() %>% 
          filter(clave_de_entidad==19)
mapa_loc <- st_read("~/Downloads/19_nuevoleon/conjunto_de_datos/19l.shp",
                    stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326) %>% 
  tibble() %>% 
  clean_names() %>% 
  select(-geometry) %>% 
  group_by(cve_mun, ambito) %>% 
  summarise(total=n()) %>%
  ungroup() %>% 
  group_by(cve_mun) %>% 
  mutate(totales=sum(total, na.rm=T)) %>% 
  ungroup() %>% 
  filter(ambito=="Rural") %>% 
  mutate(porc=(total/totales))

data <- read_csv("~/Downloads/RESAGEBURB_19CSV20.csv") %>% 
  clean_names() %>% 
  filter(ageb!="0000") %>% 
  filter(mza!="000") 

tempo <- data %>% 
  select(mun,nom_mun) %>% 
  unique()

final <- left_join(mapa_loc, tempo, by=c("cve_mun"="mun"))

para_mapa <- final %>% 
            select(cve_mun, porc)
mapa_mun <- st_read("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2020/19 - Nuevo León/MUNICIPIO.shp",
                    stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326)%>% 
  clean_names() %>% 
  mutate(municipio=formatC(municipio, width = 3,format = "d",flag = "0"))

mapa_mun <- left_join(mapa_mun, para_mapa, by=c("municipio"="cve_mun"))
# mapa_mun$porc[is.na(mapa_mun$porc)] <-0

mapa_mun$porc <- round(mapa_mun$porc*100, 2)
mapa_mun$quin <- cut2(mapa_mun$porc  , g =4)


g <- ggplot() +
  # geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  geom_sf(data=mapa_mun, aes(fill=quin), 
          size=0.5, color="grey", alpha=0.8) +
  # geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black") +
  scale_fill_brewer(palette = "BuGn") +
  labs(title="Municipios según porcentaje de localidades rurales",
       fill="", 
       caption="Fuente: Censo 2020 (INEGI)")+
  theme_void(base_size = 18) 




g <- g +
  geom_rect(
    mapping=aes(xmin = -100.6,
                ymin = 25.48,
                xmax = -100,
                ymax = 26,
                fill = NA), 
    color = "black",
    size = 0.6
  )

g<-   ggdraw(g) +
  draw_plot(
    {
      g + 
        coord_sf(
          xlim = c(-100.6, -100),
          ylim = c(25.48, 26),
          expand = FALSE) +
        theme(legend.position = "none", 
              title = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.5, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.6,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.6, 
    height = 0.3)

g


ggsave(g, filename = "~/Documents/otros/localidades.png", width =15, height = 15, units = "in")
