##########################################################################
######################## SCRIPT-DENUE-INEGI ##############################
##########################################################################

library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "geosphere", "scales", "sf", "ggmap")
libraries(my_packages)

# Descarga la base de datos de DENUE

dbdenue <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv")

# Descarga la base de datos del CENSO

dbcenso <- read.csv("C:/Users/pvelazquez/Desktop/CENSO VIVIENDA/resultados_ageb_urbana_02_cpv2010/conjunto_de_datos/resultados_ageb_urbana_02_cpv2010.csv", na.strings = c("*"))

serv_pub <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02sip.shp")

maphosp <- serv_pub %>%
  mutate_if(., is.character, funs(stringi::stri_trans_general(.,"latin-ascii"))) %>%
  filter(str_detect(GEOGRAFICO, "Centro") | str_detect(NOMBRE, "Hospital")) %>%
  filter(!GEOGRAFICO %in% c("Centro Comercial")) %>%
  filter(CVE_MUN %in% c("004")) %>%
  st_transform(crs = "+init=epsg:4326")


#########################################################################
### SCRIPT PARA OBTENER LATITUD Y LONGITUD DE LOS AGEB #################
#########################################################################

denue_ageb <- dbdenue %>%
  rename(., "CVE_AGEB" = "ageb") %>%
  filter(municipio %in% "Tijuana") %>%
  group_by(CVE_AGEB) %>%
  summarise("y" = mean(latitud),
            "x" = mean(longitud))

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

map_denue <- st_join(maptj, denue_sf) %>%
  mutate(clust = as.factor(clust)) 

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
  mutate(category = cut(p15sec_in, breaks = xp, labels = c("low", "middle", "above middle", "high")))


##############################################################################
#### SCRIPT PARA CALCULAR EL CENTROIDE DE LOS SUBMERCADOS ####################
##############################################################################

centroid_censo <- censo_spat_point %>%
  group_by(clust) %>%
  summarise(pobtot = sum(pobtot)) %>%
  st_centroid()


xd <- quantile(centroid_censo$pobtot, c(0,1/4,2/4,3/4,1))
xd[1] <- xd[1] - .00005

centroid_censo1 <- centroid_censo %>%
  mutate(pobtot = cut(pobtot, breaks = xd, labels = c("low", "middle", "above middle", "high")))

###############################################################
#### SCRIPT PARA REALIZAR MAPA DE ESCUELAS ####################
###############################################################

map_school <- serv_pub %>%
  filter(CVE_MUN %in% c("004") & GEOGRAFICO %in% c("Escuela")) %>%
  st_transform(maphosp, crs = "+init=epsg:4326")


# Principales aglomeraciones industriales en Tijuana

ggmap(tjmap) +
  geom_sf(data = map_denue, mapping = aes(fill = clust), inherit.aes = FALSE, alpha = 0.6, size = 0.1)+ 
  scale_y_continuous(limits = c(32.397, 32.56)) +
  theme(legend.position = "none") + 
  ggtitle("Distribución Industrial en Tijuana")

# Población Económicamente Activa por submercado en Tijuana

ggmap(tjmap) + 
  geom_sf(data = map_censo1, inherit.aes = FALSE, mapping = aes(fill = category), size = 0.1) + 
  scale_fill_manual(values = c("#2e9fd9", "#f6932f", "#6ebe4c", "#ca2128")) + 
  geom_sf(data = centroid_censo1, inherit.aes = FALSE, mapping = aes(size = pobtot), alpha = 0.6) +
  scale_size_discrete(range = c(3,15), breaks = pretty_breaks(n = 4)) + 
  scale_y_continuous(limits = c(32.397, 32.56)) +
  theme(legend.position = "none") + 
  ggtitle("Población Económicamente Activa en Tijuana")

# Mapa de población de 15 años y más con secundaria incompleta por submercado en Tijuana

ggmap(tjmap) + 
  geom_sf(data = map_censo2, inherit.aes = FALSE, mapping = aes(fill = category), size = 0.1) + 
  scale_fill_manual(values = c("#2e9fd9", "#f6932f", "#6ebe4c", "#ca2128")) + 
  geom_sf(data = centroid_censo1, inherit.aes = FALSE, mapping = aes(size = pobtot), alpha = 0.6) +
  scale_size_discrete(range = c(3,15), breaks = pretty_breaks(n = 4)) + 
  scale_y_continuous(limits = c(32.397, 32.56)) +
  theme(legend.position = "none") + 
  ggtitle("Población de 15 años y más con secundaria incompleta")

# Mapa de Escuelas en Tijuana

ggmap(tjmap) + 
  geom_sf(data = maptj, inherit.aes = FALSE, alpha = 0.2, size = 0.1) +
  geom_sf(data = map_school, inherit.aes = FALSE, alpha = 0.6, mapping = aes(colour = GEOGRAFICO), size = 1.5) + 
  scale_color_manual(values = c("#2e9fd9")) + 
  scale_y_continuous(limits = c(32.397, 32.56)) + 
  geom_sf(data = centroid_censo1, inherit.aes = FALSE, mapping = aes(size = pobtot), alpha = 0.6)  +
  scale_size_discrete(range = c(3,15), breaks = pretty_breaks(n = 4)) + 
  theme(legend.position = "none") +
  ggtitle("Distribución geográfica de escuelas en Tijuana")

# Mapa de Hospitales en Tijuana

ggmap(tjmap) + 
  geom_sf(data = maptj, inherit.aes = FALSE, alpha = 0.2, size = 0.1) + 
  geom_sf(data = maphosp, inherit.aes = FALSE, colour = "blue", size = 1.5) + 
  geom_sf(data = centroid_censo1, inherit.aes = FALSE, mapping = aes(size = pobtot), alpha = 0.6) + 
  scale_size_discrete(range = c(3,15), breaks = pretty_breaks(n = 4)) +
  scale_y_continuous(limits = c(32.397, 32.56)) + 
  theme(legend.position = "none") +
  ggtitle("Distribución de hospitales en Tijuana")







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
