############################################################
########## SCRIPT-DENUE-INEGI ##############################
############################################################

##########################################################################
### SCRIPT PARA AGRUPAR EMPRESAS EN CLUSTERS EN TIJUANA ##################
##########################################################################

library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "sp", "geosphere", "scales")
my_packages2 <- c("tidyverse", "ggmap", "sf", "lubridate")
libraries(my_packages2)

# Descarga la base de datos de DENUE

datos <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv")

# Define las proyecciones

pr2 <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

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
  rename("y" = LATITUD,
         "x" = LONGITUD) %>%
  droplevels()



denue_sf <- st_as_sf(denue, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  st_transform(crs = 4326)

denue_box <- st_bbox(denue_sf)

denue_location <- c(lon = -116.901419,
                    lat = 32.486711)


mdist <- st_distance(denue_sf)

hc <- hclust(as.dist(mdist), method = "complete")

d = 8000

denue_sf$clust <- cutree(hc, h = d)

##########################################################################
### SCRIPT PARA LEER LOS AGEB DEFINIDOS PARA TIJUANA    ##################
##########################################################################


# Lees los datos de polígonos de AGEB de Baja California

maptj <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02a.shp")

# Filtra para los datos solo para el municipio de Tijuana

maptj <- maptj %>%
  filter(CVE_MUN == "004")

# Une las dos bases de datos

denuem <- st_join(maptj, denue_sf)

# define la proyeccion de los datos

maptj <- st_transform(maptj, crs = "+init=epsg:4326")


denue_map <- get_map(denue_location, zoom = 11)


ggmap(denue_map) + 
  geom_sf(data = denuem, inherit.aes = FALSE, aes(fill = factor(clust), alpha = 0.01))




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











