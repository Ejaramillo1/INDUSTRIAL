##########################################################################
######################## SCRIPT-DENUE-INEGI ##############################
##########################################################################

library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "geosphere", "scales", "sf", "ggmap", "ggrepel", "cowplot")
libraries(my_packages)

# Descarga la base de datos de DENUE

dbdenue <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv", guess_max = 1000000)

# Descarga la base de datos del CENSO

dbcenso <- read.csv("C:/Users/pvelazquez/Desktop/CENSO VIVIENDA/resultados_ageb_urbana_02_cpv2010/conjunto_de_datos/resultados_ageb_urbana_02_cpv2010.csv", na.strings = c("*"))

serv_pub <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02sip.shp")

maphosp <- serv_pub %>%
  mutate_if(., is.character, funs(stringi::stri_trans_general(.,"latin-ascii"))) %>%
  filter(str_detect(GEOGRAFICO, "Centro") | str_detect(NOMBRE, "Hospital")) %>%
  filter(!GEOGRAFICO %in% c("Centro Comercial")) %>%
  filter(CVE_MUN %in% c("004")) %>%
  st_transform(crs = "+init=epsg:4326")


rezago_social <- read_csv("Rezago Social en AGEB 2010 vf.csv") %>%
  select(`Clave de la AGEB`, `Grado de rezago social (clases latentes)`, `Entidad federativa`, `Nombre del municipio`) %>%
  mutate_if(., is.character, funs(stringi::stri_trans_general(.,"latin-ascii"))) %>%
  filter(`Entidad federativa`  %in% "Baja California" & `Nombre del municipio` %in% "Tijuana")


#########################################################################
### SCRIPT PARA OBTENER LATITUD Y LONGITUD DE LOS AGEB #################
#########################################################################

denue_ageb <- dbdenue %>%
  rename(., "CVE_AGEB" = "ageb") %>%
  filter(municipio %in% "Tijuana") %>%
  group_by(CVE_AGEB) %>%
  summarise("y" = mean(latitud),
            "x" = mean(longitud)) %>%
  distinct(CVE_AGEB, x, y)

#########################################################################
### SCRIPT PARA AGRUPAR EMPRESAS EN CLUSTERS EN TIJUANA #################
#########################################################################

denue_sf <- dbdenue %>%
  dplyr::select(latitud, longitud ,cve_loc,municipio ,id, nom_estab, raz_social, codigo_act, nombre_act, per_ocu, tipo_vial, tipo_asent, nomb_asent, tipoCenCom, nom_CenCom, localidad, ageb, manzana, fecha_alta)  %>%
  separate(fecha_alta, into = c("mes_alta", "year_alta"), sep = "\\s") %>%
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud),
         day = 01,
         fecha = ymd(as_date(paste0(year_alta, mes_alta, day))),
         codigo_industria = factor(str_sub(codigo_act,1,2))) %>%
  filter(municipio %in% "Tijuana" & 
           codigo_industria %in% c("31", "32", "33") &
           per_ocu %in% c("101 a 250 personas", "251 y más personas")) %>%
  rename_all(funs(str_to_lower(.))) %>%
  mutate(LATITUD = latitud,
         LONGITUD = longitud) %>%
  rename("y" = LATITUD,
         "x" = LONGITUD,
         "CVE_AGEB" = "ageb") %>%
  select(x, y, CVE_AGEB) %>%
  droplevels()


ci <- dbdenue %>%
  dplyr::select(latitud, longitud ,cve_loc,municipio ,id, nom_estab, raz_social, codigo_act, nombre_act, per_ocu, tipo_vial, tipo_asent, nomb_asent, tipoCenCom, nom_CenCom, localidad, ageb, manzana, fecha_alta)  %>%
  separate(fecha_alta, into = c("mes_alta", "year_alta"), sep = "\\s") %>%
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud),
         day = 01,
         fecha = ymd(as_date(paste0(year_alta, mes_alta, day))),
         codigo_industria = factor(str_sub(codigo_act,1,2))) %>%
  filter(municipio %in% "Tijuana" & 
           codigo_industria %in% c("32", "33") &
           per_ocu %in% c("251 y más personas")) %>%
  rename_all(funs(str_to_lower(.))) %>%
  mutate(LATITUD = latitud,
         LONGITUD = longitud) %>%
  rename("y" = LATITUD,
         "x" = LONGITUD,
         "CVE_AGEB" = "ageb") %>%
  select(x, y, CVE_AGEB) %>%
  droplevels()


PARQUES <- dbdenue %>%
  dplyr::select(latitud, longitud ,cve_loc,municipio ,id, nom_estab, raz_social, codigo_act, nombre_act, per_ocu, tipo_vial, tipo_asent, nomb_asent, tipoCenCom, nom_CenCom, localidad, ageb, manzana, fecha_alta)  %>%
  separate(fecha_alta, into = c("mes_alta", "year_alta"), sep = "\\s") %>%
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud),
         day = 01,
         fecha = ymd(as_date(paste0(year_alta, mes_alta, day))),
         codigo_industria = factor(str_sub(codigo_act,1,2))) %>%
  filter(municipio %in% "Tijuana" & str_detect(tipo_asent, "INDUSTRIAL")) %>%
  distinct(latitud, longitud, tipo_asent, nomb_asent) %>%
  rename_all(funs(str_to_lower(.))) %>%
  group_by(nomb_asent) %>%
  mutate(m.lat = mean(latitud, na.rm = TRUE),
         m.long = mean(longitud, na.rm = TRUE)) %>%
  distinct(nomb_asent, m.lat, m.long)
  mutate(LATITUD = latitud,
         LONGITUD = longitud) %>%
  rename("y" = LATITUD,
         "x" = LONGITUD) %>%
  droplevels() %>%
    



##########################################################################
#### SCRIPT PARA CONVERTIR LOS DATOS COMBINADOS DEL CENSO Y LA DENUE A ###
#### ESPACIALES ##########################################################
##########################################################################

denue_sf <- st_as_sf(denue_sf, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  st_transform(crs = 4326)

# Calcula la matriz de distancias

mdist <- st_distance(denue_sf)

# Establece los parametros del algoritmo de clusters

hc <- hclust(as.dist(mdist), method = "complete")

# Establece la distancia en metros donde se corta el algoritmo que clasifica
# los clusters

d = 8000

# Agrega la columna de clusters a los datos

denue_sf$clust <- cutree(hc, h = d)


denue_sf <- denue_sf %>%
  mutate(clust = fct_recode(as.factor(clust),
                            "Vía Rápida - Alamar" = "1",
                            "Paífico - Nórdika" = "2",
                            "Ciudad Industrial Otay" = "3",
                            "El Florido" = "4",
                            "UABC - FINSA" = "5",
                            "Playas - Centro" = "7",
                            "Ejido Ojo de Agua" = "8"))

##########################################################################
############ SCRIPT PARA LIMPIAR LA BASE DE DATOS DEL CENSO ##############
##########################################################################

datcenso <- dbcenso %>%
  rename_all(funs(str_to_lower(.))) %>%
  filter(mun == 4 & nom_loc %in% c("Total AGEB urbana")) %>%
  rename(., "CVE_AGEB" = "ageb",
         "CVE_LOC" = "loc",
         "CVE_ENT" = "ï..entidad",
         "CVE_MUN" = "mun")

#########################################################################
### SCRIPT PARA UNIR LOS DATOS DEL CENSO CON DENUE PARA #################
### OBTENER LAS COORDENADAS DE LOS AGEB #################################
#########################################################################

censo_spat_point <- left_join(denue_sf, datcenso, by = c("CVE_AGEB")) %>%
  filter(!is.na(nom_ent))

##########################################################################
### SCRIPT PARA LEER LOS AGEB DEFINIDOS PARA TIJUANA    ##################
##########################################################################

# Lees los datos de polígonos de AGEB de Baja California

maptj <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02a.shp")

# Filtra para los datos solo para el municipio de Tijuana

maptj <- maptj %>%
  filter(CVE_MUN == "004")

# define la proyeccion de los datos

maptj <- st_transform(maptj, crs = "+init=epsg:4326")

# Une las dos bases de datos

map_censo <- left_join(maptj, datcenso, by = c("CVE_AGEB")) %>%
  filter(!is.na(pea) )

# Mapa de ageb nivel socioeconómico

map_coneval <- left_join(maptj, rezago_social, by = c("CVE_AGEB" = "Clave de la AGEB"))

ggmap(tjmap) + 
  geom_sf(data = map_coneval, inherit.aes = FALSE, mapping = aes(fill = factor(`Grado de rezago social (clases latentes)`)))

##########################################################################
#### SCRIPT PARA OBTENER EL MAPA DE TIJUANA ##############################
##########################################################################
##########################################################################

tjlocation <- c(lon = -116.944333,
                lat = 32.491566)
tjmap <- get_map(tjlocation, zoom = 11, maptype = c("roadmap"))

##########################################################################
#### SCRIPT UNIR LOS DATOS DE DENUE CON EL MAPA DE LOS POLIGONOS POR AGEB #
###########################################################################

map_denue <- st_join(maptj, denue_sf)
##############################################################################
#### SCRIPT PARA CALCULAR EL CENTROIDE DE LOS SUBMERCADOS ####################
##############################################################################

centroid_denue <- denue_sf %>%
  mutate(counter = 1,
         empresas = sum(counter)) %>%
  group_by(clust) %>%
  summarise(mn = n()) %>%
  st_centroid()


##############################################################################
#### SCRIPT PARA CONVERTIR LOS DATOS DE PEA EN CATEGORICOS ###################
##############################################################################

xs  <- quantile(map_censo$pea, c(0, 1/4, 2/4, 3/4, 1))
xs[1] <- xs[1] - .00005

map_censo1 <- map_censo %>%
  mutate(category = cut(pea, breaks = xs, labels = c("low", "middle", "above middle", "high")))

##############################################################################
#### SCRIPT PARA CONVERTIR LOS DATOS DE Población de 15 y mas EN CATEGORICOS #
##############################################################################

xp  <- quantile(map_censo$p15sec_in, c(0, 1/4, 2/4, 3/4, 1), na.rm = TRUE)
xp[1] <- xp[1] - .00005

map_censo2 <- map_censo %>%
  mutate(category = cut(p15sec_in, breaks = xp, labels = c("low", "middle", "above middle", "high"))
         )


##############################################################################
#### S
########################################################################

centroid_censo <- censo_spat_point %>%
  group_by(clust) %>%
  summarise(pobtot = sum(pobtot)) %>%
  st_centroid()
  


xd <- quantile(centroid_censo$pobtot, c(0,1/4,2/4,3/4,1))
xd[1] <- xd[1] - .00005

centroid_censo1 <- centroid_censo %>%
  mutate(pobtot = cut(pobtot, breaks = xd, labels = c("low", "middle", "above middle", "high")),
         COORDS_X = map_dbl(geometry,1),
         COORDS_Y = map_dbl(geometry,2))

centroid_censo2 <- centroid_censo1 %>%
  filter(clust %in% c("Paífico - Nórdika")) %>%
  mutate(clust = fct_recode(clust, 
                            "ATISA INDUSTRIAL" = "Paífico - Nórdika"))
###############################################################
#### SCRIPT PARA REALIZAR MAPA DE ESCUELAS ####################
###############################################################

map_school <- serv_pub %>%
  filter(CVE_MUN %in% c("004") & GEOGRAFICO %in% c("Escuela")) %>%
  st_transform(maphosp, crs = "+init=epsg:4326")


# Principales aglomeraciones industriales en Tijuana

ggmap(tjmap)                                                                         +
  theme_minimal()                                                                    + 
  geom_sf(data               = map_denue, 
          mapping            = aes(fill           = clust), 
          inherit.aes        = FALSE, 
          alpha              = 0.6, 
          size               = 0.1,
          color = "Grey")                                                            + 
  geom_label_repel(data = centroid_censo2, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, 
                                 label = clust),
                   color      = "black", 
                   fontface   = "bold", 
                   size       = 2, 
                   alpha      = 0.7)                                     +
  scale_y_continuous(limits  = c(32.397, 
                                 32.56))                                             + 
  theme(legend.justification = c(1,0),
        legend.position      = c(1,0), 
        legend.text          = element_text(size   = 7), 
        legend.title         = element_text(size   = 8, 
                                            face   = "bold"), 
        legend.key.size      = unit(.5, 
                                    "line"), 
        plot.title           = element_text(face   = "bold", 
                                            size   = 16, 
                                            family = "sans"), 
        legend.background    = element_blank())                                      +
  scale_fill_manual(values   = c("#f6932f", 
                                 "#ca2128", 
                                 "#6ebe4c", 
                                 "#2e9fd9", 
                                 "#a74e9d", 
                                 "#22602c", 
                                 "#b7451d", 
                                 "#195772", 
                                 "#5e1e60"),
                    name     = "Submarket")                                         +
  labs(caption               = "Source: INEGI; DENUE, Census Data 2010")            +
  ggtitle("Main industrial aglommerations in Tijuana")
  dev.off()

ggsave("aglomeraciones_industriales.jpg")

# Población Económicamente Activa por submercado en Tijuana

ggmap(tjmap) + 
  theme_minimal() +
  geom_sf(data                =   map_censo1, 
          inherit.aes         =   FALSE, 
          mapping             =   aes(fill          = category), 
          size                =   0.1,
          alpha               =   0.6,
          color = "Grey")                                                            + 
  scale_fill_manual(values    = c("#2e9fd9", 
                                  "#f6932f", 
                                  "#6ebe4c", 
                                  "#ca2128"),
                    name      = "Workers density")                                   +
  scale_y_continuous(limits   = c(32.397, 
                                  32.56))                                            + 
  theme(legend.justification  = c(1,0), 
        legend.position       = c(1,0),
        legend.text           = element_text(size   = 7), 
        legend.title          = element_text(size   = 8, 
                                             face   = "bold"),
        legend.key.size       = unit(.5, "line"), 
        plot.title            = element_text(face   = "bold", 
                                             size   = 16, 
                                             family = "sans"), 
        legend.background     = element_blank())                                     +
  geom_sf(data                = centroid_censo1, 
          inherit.aes         = FALSE, 
          alpha               = .5, 
          mapping             = aes(size            = pobtot))                       + 
  scale_size_discrete(range   = c(3,15), 
                      breaks  = pretty_breaks(n     = 4))                            +
  geom_label_repel(data       = centroid_censo1, 
                   mapping    = aes(COORDS_X, 
                                    COORDS_Y, 
                                    label           = clust), 
                   color      = "#5e1e60", 
                   fontface   = "bold", 
                   size       = 2, 
                   alpha      = 0.7)                                                 +
  ggtitle("Distribution density of working population in Tijuana")                                +
  labs(caption                = "Source: INEGI; DENUE, Census Data 2010")
  dev.off()

ggsave("economically_active_population.jpg")

# Población Económicamente Activa por submercado en Tijuana

ggmap(tjmap) + 
  theme_minimal() +
  geom_sf(data                =   map_censo1, 
          inherit.aes         =   FALSE, 
          mapping             =   aes(fill          = category), 
          size                =   0.1,
          alpha               =   0.6,
          color = "Grey")                                                            + 
  scale_fill_manual(values    = c("#2e9fd9", 
                                  "#f6932f", 
                                  "#6ebe4c", 
                                  "#ca2128"),
                    name      = "Workers density")                                   +
  scale_y_continuous(limits   = c(32.397, 
                                  32.56))                                            + 
  theme(legend.justification  = c(1,0), 
        legend.position       = c(1,0),
        legend.text           = element_text(size   = 7), 
        legend.title          = element_text(size   = 8, 
                                             face   = "bold"),
        legend.key.size       = unit(.5, "line"), 
        plot.title            = element_text(face   = "bold", 
                                             size   = 16, 
                                             family = "sans"), 
        legend.background     = element_blank())                                     +
  geom_sf(data                = centroid_censo1, 
          inherit.aes         = FALSE, 
          alpha               = .5, 
          mapping             = aes(size            = pobtot))                       + 
  scale_size_discrete(range   = c(3,15), 
                      breaks  = pretty_breaks(n     = 4))                            +
  geom_label_repel(data       = centroid_censo1, 
                   mapping    = aes(COORDS_X, 
                                    COORDS_Y, 
                                    label           = clust), 
                   color      = "#5e1e60", 
                   fontface   = "bold", 
                   size       = 2, 
                   alpha      = 0.7)                                                 +
  ggtitle("Distribution density of working population in Tijuana")                                +
  labs(caption                = "Source: INEGI; DENUE, Census Data 2010")
dev.off()

ggsave("economically_active_population.jpg")

# Mapa de Escuelas en Tijuana
ggmap(tjmap)                                                                         + 
  theme_minimal()                                                                    +
  geom_sf(data               = map_denue, 
          mapping            = aes(fill           = clust), 
          inherit.aes        = FALSE, 
          alpha              = 0.2, 
          size               = 0.1,
          color = "Grey") +
geom_label_repel(data = centroid_censo2, 
                 mapping = aes(x = COORDS_X, y = COORDS_Y, 
                               label = clust),
                 color      = "black", 
                 fontface   = "bold", 
                 size       = 2, 
                 alpha      = 0.7)                                     + 
  scale_y_continuous(limits = c(32.397,
                                32.56))                                              +
  scale_fill_manual(values   = c("#f6932f", 
                                 "#ca2128", 
                                 "#6ebe4c", 
                                 "#2e9fd9", 
                                 "#a74e9d", 
                                 "#22602c", 
                                 "#b7451d",
                                 "#195772",
                                 "#5e1e60"), 
                    name     = "Submarket")                                          +
  theme(legend.justification = c(1,0),
        legend.position = c(1,0),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8,
                                    face = "bold"),
        legend.key.size = unit(.5,
                               "line"),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "sans"),
        legend.background = element_blank())                                        +
  labs(title = "Public services in Tijuana: PUBLIC SCHOOLS" ,
       caption = "Source: INEGI; DENUE, Census Data 2010") + 
  geom_sf(data = map_school, 
          inherit.aes = FALSE,
          alpha = 0.6,
          size = 1.5,
          colour = "#195772")

dev.off()
ggsave("escuelas_en_tijuana.jpg")  

# Mapa de Hospitales en Tijuana

ggmap(tjmap)                                                                        + 
  theme_minimal()                                                                   + 
  geom_sf(data = map_denue,
          mapping = aes(fill = clust),
          inherit.aes = FALSE, 
          alpha = 0.2, 
          size = 0.1,
          color = "Grey") + 
  geom_sf(data = maphosp, 
          inherit.aes = FALSE, 
          colour = "#195772", 
          size = 1.5) +
  geom_label_repel(data = centroid_censo2, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, 
                                 label = clust),
                   color      = "black", 
                   fontface   = "bold", 
                   size       = 2, 
                   alpha      = 0.7)                                     +
  scale_fill_manual(values   = c("#f6932f", 
                                 "#ca2128", 
                                 "#6ebe4c", 
                                 "#2e9fd9", 
                                 "#a74e9d", 
                                 "#22602c", 
                                 "#b7451d",
                                 "#195772",
                                 "#5e1e60"), 
                    name     = "Submarket")  +
  scale_y_continuous(limits = c(32.397, 32.56))  +
  theme(legend.justification = c(1,0),
        legend.position = c(1,0),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8,
                                    face = "bold"),
        legend.key.size = unit(.5,
                               "line"),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "sans"),
        legend.background = element_blank()) +
  labs(caption = "Source: INEGI; DENUE, Census Data 2010", 
       title = "Public services in Tijuana: HOSPITALS") 
  dev.off()

ggsave("hospitales_tijuana.jpg")


# Densidad de principales aglomeraciones industriales en Tijuana

ggmap(tjmap, extent = "panel") + 
  geom_density2d(data = ci, aes(x, y)) +
  stat_density2d(data = ci , aes(x, y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  theme_minimal() + 
  theme(legend.position = "none", axis.title = element_blank()) +
  scale_y_continuous(limits = c(32.397, 32.56)) +
  theme(legend.justification = c(1,0),
        legend.position = c(1,0),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8,
                                    face = "bold"),
        legend.key.size = unit(.5,
                               "line"),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  family = "sans"),
        legend.background = element_blank()) +
  labs(caption = "Source: INEGI; DENUE, Census Data 2010", 
       title = "Manufacturing industry density in Tijuana") +
  geom_label_repel(data = centroid_censo2, 
                   mapping = aes(x = COORDS_X, y = COORDS_Y, 
                                 label = clust),
                   color      = "black", 
                   fontface   = "bold", 
                   size       = 2, 
                   alpha      = 0.7,
                   force = 50)
dev.off()

ggsave("density.jpg")


censo_spat_point %>%
  filter(clust %in% c("Pacífico - Nordika")) %>%
  select(CVE_AGEB, graproes) %>%
  summary(mgrap = mean(graproes))
  





ggmap(tjmap) + 
  geom_point(data= PARQUES,aes(m.long, m.lat)) +
  geom_text_repel(data=PARQUES, aes(m.long, m.lat, label = nomb_asent))



censo_spat_point %>%
  group_by(clust) %>%
  summarise(Male = sum(pobmas, na.rm = TRUE),
            Female = sum(pobfem, na.rm = TRUE)) %>%
  filter(!clust %in% c("El Florido")) %>%
    gather(key = "sexo", value = "poblacion", Male, Female) %>%
  arrange(sexo) %>%
  ggplot() +
  geom_bar(aes(x = clust, y = poblacion, fill = factor(sexo)), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Total population by submarket and gender in Tijuana",
       caption = "Source: INEGI; DENUE, Census Data 2010") + 
  xlab("") + 
  coord_flip() + 
  ylab("Total population") +
  scale_fill_manual(values   = c("#ca2128", "#2e9fd9"), 
                    name     = "Gender") +
  scale_y_continuous(breaks = c(50000 , 100000 , 150000 ,200000, 250000 ,300000), labels = c("50,000", "100,000", "150,000", "200,000", "250,000", "300,00"))

dev.off()

ggsave("total_population_by_submarket.jpg")











# Grafica de poblacion economicamente activa por submercado


censo_spat_point %>%
  select(clust, pobtot) %>%
  group_by(clust) %>%
  summarise(spob = sum(pobtot, na.rm = TRUE)) %>%
  as_tibble() %>%
  ggplot() +
  geom_col(aes(x= clust , y = spob))
  





# Selecciona las coordenadas de las empresas que son utiles para el análisis

coordenadas <- denue %>%
  dplyr::select(x,y)

# Establece las características de proyección de los datos

pr <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Convertir la tabla de datos regular a una tabla de datos con caracteristicas geograficas

denue <- SpatialPointsDataFrame(matrix(c(coordenadas$x, coordenadas$y), ncol = 2), data =denue, proj4string = CRS(pr))

# Calcula la matriz de distancias entre las empresas

mdist <- distm(denue)

# Realiza el procedimiento de clustering jerarquico

hc <- hclust(as.dist(mdist), method="complete")

# Establece la distancia donde se cortara el algoritmo de clasificacion

d= 8000

# Combina los datos de los cluster generados con los datos del directorio

denue$clust <- cutree(hc, h = d)

# Define nueva proyección

pr2 <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Cambia la proyección de los datos espaciales

denue <- spTransform(denue, CRSobj = pr2)

library(sf)

# Transforma los datos a un objeto de tipo "simple features"

denue <- st_as_sf(denue, coords = c("x", "y"), crs = pr2) %>%
  st_transform(crs = "+init=epsg:4326" )


ggmap(tj.map) +
  geom_point(data = denue, aes(x = x, y = y))


rm(coordenadas)
rm(hc)
rm(mdist)




############################################################################
### SCRIPT PARA OBTENER LA UBICACION DE ATISA EN EL MAPA Y PROYECTAR #######
############################################################################

datatisa <- datos %>%
  filter(id %in% c("6757763")) %>%
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud))

cordatisa <- datos %>%
  filter(id %in% c("6757763")) %>%
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud)) %>%
  select(latitud, longitud) %>%
  rename("y" = "latitud",
         "x" = "longitud")

atisa <- SpatialPointsDataFrame(matrix(c(cordatisa$x, cordatisa$y), ncol = 2), data =datatisa, proj4string = CRS(pr))
atisa <- st_as_sf(atisa)
atisa <- st_transform(atisa,pr2)

#############################################################
### SCRIPT PARA ELABORAR LOS MAPAS CON LOS RESULTADOS #######
#############################################################


mis_colores <- c("#ca2128", "#f6932f", "#6ebe4c", "#2e9fd9", "#a74e9d", "#6f3da3", "#a0121d", "#ed6223", "#2b9245")

ggplot(denue_map) + 
  geom_sf(data = tijuana, aes(fill = sumapob)) +
  scale_fill_gradient(low = "grey", high = "#f6932f") +
  geom_sf(data = atisa, aes(size = 5)) +
  ggtitle("Distribución de ploblación en Tijuana por AGEB") +
  theme_minimal() 

ggsave("pop.jpg")




ggmap(denue_map) + 
  geom_sf(data = centroid, inherit.aes = FALSE, mapping = aes(colour = factor(clust)))

##############################################################################
### SCRIPT PARA ELABORAR LOS MAPAS DE CLUSTERS INDUSTRIALES EN TIJUANA #######
##############################################################################

ggplot(denuem) +
  geom_sf(mapping = aes(fill = factor(clust))) +
  theme_minimal() + 
  ggtitle("Principales áreas industriales en Tijuana 2017")

ggsave("indtij.jpg")

ggplot() +
  geom_sf(data = tijuana, mapping = aes(fill = sumapob)) +
  geom_sf(data = centroid, mapping = aes(colour = factor(clust), size = mn), alpha = .5) + 
  scale_size_continuous(range = c(3,21), breaks = pretty_breaks(7)) +
  theme(legend.position = "none")

library(ggmap)
library(ggalt)
library(sf)

Tijuana <- c(-116.699287, 32.370166)
tijuana.map <-  ggmap::get_map(location = Tijuana)

ggmap(tijuana.map)

denuem <- st_transform(denuem, crs = "+init=epsg:4326" )
