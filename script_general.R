############################################################
########## SCRIPT-DENUE-INEGI ##############################
############################################################

##########################################################################
### SCRIPT PARA AGRUPAR EMPRESAS EN CLUSTERS EN TIJUANA ##################
##########################################################################

library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "sp", "geosphere", "scales")
my_packages2 <- c("tidyverse", "ggmap", "sf", "lubridate")
libraries(my_packages)

# Descarga la base de datos de DENUE

datos <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv")

# Limpiar la base de datos DENUE

denue <- datos %>%
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
  rename_all(funs(str_to_upper(.))) %>%
  mutate(latitud = LATITUD,
         longitud = LONGITUD) %>%
  rename("y" = LATITUD,
         "x" = LONGITUD) %>%
  droplevels()



denue_sf <- st_as_sf(denue, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  st_transform(crs = 4326)

denue_box <- st_bbox(denue_sf)

denue_location <- c(lon = -116.944333,
                    lat = 32.491566)


denue_map <- get_map(denue_location)

mdist <- st_distance(denue_sf)

hc <- hclust(as.dist(mdist), method = "complete")

d = 8000

denue_sf$clust <- cutree(hc, h = d)

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

tj_bbox <- st_bbox(denue)

tj_location <- c(lon = tj_bbox["xmin"]+tj_bbox["xmax"]/2,
                 lat = tj_bbox["ymin"] + tj_bbox["ymax"]/2)

tj.map <- get_map(Tijuana)


ggmap(tj.map) +
  geom_point(data = denue, aes(x = x, y = y))


rm(coordenadas)
rm(hc)
rm(mdist)


##########################################################################
### SCRIPT PARA LEER LOS AGEB DEFINIDOS PARA TIJUANA    ##################
##########################################################################


# Lees los datos de polígonos de AGEB de Baja California

maptj <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02a.shp")

# Filtra para los datos solo para el municipio de Tijuana

maptj <- maptj %>%
  filter(CVE_MUN == "004")

# Une las dos bases de datos

denuem <- st_join(maptj, denue)

# define la proyeccion de los datos

maptj <- st_transform(maptj, crs = "+init=epsg:4326")


#############################################################################
### SCRIPT PARA INCLUIR DATOS DEL CENSO ADEMAS DE DENUE    ##################
#############################################################################


dbcenso <- read_excel("C:/Users/pvelazquez/Desktop/CENSO VIVIENDA/RESAGEBURB_02XLS10/RESAGEBURB_02XLS10.xls")

dbc <- dbcenso %>%
  filter(MUN %in% "004") %>%
  mutate_all(funs(str_replace_all(., "\\*", "NA"))) %>%
  filter(!LOC %in% c("0000") & !AGEB %in% c("0000") & !MZA %in% c("000")) %>%
  select(NOM_ENT, NOM_LOC, AGEB, MZA, POBTOT, POBMAS, POBFEM, P_15YMAS, 
         P_15YMAS_M, P_15YMAS_F, REL_H_M, POB15_64, GRAPROES, GRAPROES_M, GRAPROES_F, TOTHOG, VIVTOT,
         VPH_1CUART, VPH_2CUART, VPH_3YMASC) %>%
  mutate(POBTOT     = as.numeric(POBTOT) , 
         POBMAS     = as.numeric(POBMAS), 
         POBFEM     = as.numeric(POBFEM), 
         P_15YMAS   = as.numeric(P_15YMAS), 
         P_15YMAS_M = as.numeric(P_15YMAS_M), 
         P_15YMAS_F = as.numeric(P_15YMAS_F), 
         REL_H_M    = as.numeric(REL_H_M), 
         POB15_64   = as.numeric(POB15_64), 
         GRAPROES   = as.numeric(GRAPROES), 
         GRAPROES_M = as.numeric(GRAPROES_M), 
         GRAPROES_F = as.numeric(GRAPROES_F), 
         TOTHOG     = as.numeric(TOTHOG), 
         VIVTOT     = as.numeric(VIVTOT),
         VPH_1CUART = as.numeric(VPH_1CUART), 
         VPH_2CUART = as.numeric(VPH_2CUART), 
         VPH_3YMASC = as.numeric(VPH_3YMASC),
         CVE_AGEB   = as.character(AGEB),
         spob = sum(POBTOT)) %>%
  group_by(AGEB) %>%
  summarise(sumapob = sum(POBTOT)) %>%
  rename(., "CVE_AGEB" = "AGEB")


tijuana <- left_join(maptj, dbc, by = "CVE_AGEB")


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


centroid <- denue_sf %>%
  mutate(counter = 1,
         empresas = sum(counter)) %>%
  group_by(clust) %>%
  summarise(m.x = mean(latitud, na.rm = TRUE),
            m.y = mean(longitud, na.rm = TRUE),
            mn = n())


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
