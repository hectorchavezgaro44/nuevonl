library(pacman)
p_load(tidyverse, sf, here,janitor, classInt, Hmisc, cowplot, patchwork)


# X11(type="cairo")


secc <- st_read(here("inp", "SHP", "SECCION.shp"),
                stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326)%>% 
  clean_names()

mapa_mun <- st_read(here("inp", "SHP", "MUNICIPIO.shp"),
                    stringsAsFactors = FALSE, quiet = TRUE) %>%
  st_transform(4326)%>% 
  clean_names()
zm <- c("MONTERREY", "SAN PEDRO GARZA GARCIA", "GARCIA", "SANTA CATARINA", 
        "GRAL. ESCOBEDO", "SAN NICOLAS DE LOS GARZA", "GUADALUPE", "JUAREZ", "APODACA")

nombres <- mapa_mun %>% 
           summarise(nombre, centroide=st_centroid(geometry))


nombres <- cbind(mapa_mun, st_coordinates(st_centroid(mapa_mun$geometry)))

nombres <- nombres %>% 
           tibble() %>% 
           select(nombre, X,Y)

nombres <- nombres %>% 
  filter(nombre %in% zm) %>% 
  mutate(nombre=gsub("DE LOS GARZA", "", nombre), 
        nombre=gsub("GARZA GARCIA", "", nombre),
        nombre=gsub("GRAL.", "", nombre))

data <- read_csv(here("inp", "eceg", "INE_SECCION_2020.csv")) %>% 
        clean_names() %>% 
        filter(entidad==19)

tempo <- data %>% 
  select(seccion, pobtot, pobmas, p_0a2, p_15ymas, p_6a11, 
         p_12a14, p6a11_noa, p12a14noa, p15ym_an, p15ym_se, p15pri_in, 
         p15pri_co, p15sec_in, psinder, vivparh_cv, vph_pisoti, 
         vph_pisodt, vivpar_hab, tvivparhab, vivparh_cv, pro_ocup_c, vph_autom,
         vph_s_elec, vph_aguafv, vph_excsa, vph_letr, vph_nodren, vph_refri, vph_lavad )



# Población de 15 años o más analfabeta
tempo$i_analf    <- round((tempo$p15ym_an / tempo$p_15ymas )*100, 2)


# Población de 6 a 14 años que no asiste a la escuela
tempo$i_asistesc <- round(((tempo$p6a11_noa + tempo$p12a14noa) / ( tempo$p_6a11+ tempo$p_12a14))*100, 2)
# tempo$quin_asiste <- cut2(tempo$i_asistesc , g =5)
# Población de 15 años y más con educación básica incompleta
tempo$i_edbasinc <- round(((tempo$p15ym_se + tempo$p15pri_in + tempo$p15pri_co + tempo$p15sec_in) / tempo$p_15ymas)*100, 2)

# Viviendas con piso de tierra
tempo$i_ptierra  <- round((tempo$vph_pisoti / tempo$vivparh_cv)*100,2)

# Viviendas que no disponen de energía eléctrica
tempo$i_noelec   <- round((tempo$vph_s_elec / tempo$vivparh_cv)*100,2)


# Viviendas que no disponen de energía eléctrica
tempo$i_sinauto   <- round((1- (tempo$vph_autom / tempo$vivparh_cv))*100,2)
tempo[is.na(tempo)] <- 0

foo <- tempo %>% 
       select(seccion, i_asistesc)

foo <-  left_join(secc,foo, by="seccion")
foo$i_asistesc[is.na(foo$i_asistesc)] <- 0
foo <- foo %>% 
  filter(i_asistesc>mean(i_asistesc))

foo$quin_asis <- cut2(foo$i_asistesc , g =5)

g <- ggplot() +
  geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  geom_sf(data=foo, aes(fill=quin_asis), 
          size=0, color="white", alpha=0.8) +
  geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black") +
  scale_fill_brewer(palette = "OrRd") +
  labs(title="Secciones con un porcentaje mayor al promedio estatal \nde población de 6 a 14 años que no asiste a la escuela",
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
  
g1<-   ggdraw(g) +
  draw_plot(
    {
      g + 
        geom_text(data=nombres, aes(x=X, y=Y, label=nombre)) +
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
rm(foo)

# educincomp -------------------------------------------------------------------

  foo <- tempo %>% 
    select(seccion, i_edbasinc)
  
  foo <-  left_join(secc,foo, by="seccion")
  foo$i_edbasinc[is.na(foo$i_edbasinc)] <- 0
  foo <- foo %>% 
    filter(i_edbasinc>mean(i_edbasinc))
  foo$quin <- cut2(foo$i_edbasinc , g =5)
  
 g<-  ggplot() +
   
    geom_sf(data=foo, aes(fill=quin), 
            size=0, color="white", alpha=0.8) +
    geom_sf(data=mapa_mun, fill=NA, size=0.3, color="black")+
   geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
   
    scale_fill_brewer(palette = "OrRd") +
    labs(title="Secciones con un porcentaje mayor al promedio estatal \nde población de 15 años y más con educación básica incompleta",
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
  
g2<-   ggdraw(g) +
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
  
  rm(foo)



ggsave(g1, filename = "~/Documents/otros/no_asiste.png", width =15, height = 15, units = "in")
ggsave(g2, filename = "~/Documents/otros/basincompleta.png", width =15, height = 15, units = "in")


# Piso de tierra ----------------------------------------------------------

foo <- tempo %>% 
  select(seccion, i_ptierra)

foo <-  left_join(secc,foo, by="seccion")
foo$i_ptierra[is.na(foo$i_ptierra)] <- 0

foo <- foo %>% 
       filter(i_ptierra>10)
foo$quin <- cut2(foo$i_ptierra , g =5)

g <- ggplot() +
  geom_sf(data=foo, aes(fill=quin), 
          size=0, color="white", alpha=0.8) +
  geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black")+
  geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  scale_fill_brewer(palette = "OrRd") +
  labs(title="Secciones con un porcentaje mayor al 5% \nde viviendas con piso de tierra",
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

g3<-   ggdraw(g) +
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
rm(foo)


# Sin electricidad  ----------------------------------------------------------

foo <- tempo %>% 
  select(seccion, i_noelec)

foo <-  left_join(secc,foo, by="seccion")
foo$i_noelec[is.na(foo$i_noelec)] <- 0

foo <- foo %>% 
  filter(i_noelec>5)
foo$quin <- cut2(foo$i_noelec , g =5)

g <- ggplot() +
  geom_sf(data=foo, aes(fill=quin), 
          size=0, color="white", alpha=0.8) +
  geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black")+
  geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  scale_fill_brewer(palette = "OrRd") +
  labs(title="Secciones con un porcentaje mayor al 5% \nde viviendas que no disponen de energía eléctrica",
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

g4<-   ggdraw(g) +
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


ggsave(g3, filename = "~/Documents/otros/piso_tierra.png", width =15, height = 15, units = "in")
ggsave(g4, filename = "~/Documents/otros/no_elec.png", width =15, height = 15, units = "in")


# analf -------------------------------------------------------------------



foo <- tempo %>% 
  select(seccion, i_analf)

foo <-  left_join(secc,foo, by="seccion")
foo$i_analf[is.na(foo$i_analf)] <- 0

foo <- foo %>% 
  filter(i_analf>5)
foo$quin <- cut2(foo$i_analf , g =5)

g <- ggplot() +
  geom_sf(data=foo, aes(fill=quin), 
          size=0, color="white", alpha=0.8) +
  geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black")+
  geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  scale_fill_brewer(palette = "OrRd") +
  labs(title="Secciones con un porcentaje mayor al 5% \nde población de 15 años o más analfabeta",
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

g5<-   ggdraw(g) +
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
ggsave(g5, filename = "~/Documents/otros/analfabeta.png", width =15, height = 15, units = "in")



# sin auto ----------------------------------------------------------------

foo <- tempo %>% 
  select(seccion, i_sinauto)

foo <-  left_join(secc,foo, by="seccion")
foo$i_sinauto[is.na(foo$i_sinauto)] <- 0
foo <- foo %>% 
  filter(i_sinauto>mean(i_sinauto))

foo$quin <- cut2(foo$i_sinauto , g =5)

g <- ggplot() +
  geom_sf(data=secc, fill=NA, size=0.1, color="grey")+
  geom_sf(data=foo, aes(fill=quin), 
          size=0, color="white", alpha=0.8) +
  geom_sf(data=mapa_mun, fill=NA, size=0.25, color="black")+
  scale_fill_brewer(palette = "OrRd") +
  labs(title="Secciones con un porcentaje mayor al promedio estatal \nde viviendas sin automóvil",
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

g6<-   ggdraw(g) +
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
rm(foo)

ggsave(g6, filename = "~/Documents/otros/sinauto.png", width =15, height = 15, units = "in")
