##########################################################################
######################## SCRIPT-DENUE-INEGI ##############################
##########################################################################

library(easypackages)
my_packages <- c("tidyverse", "readxl", "lubridate", "geosphere", "scales", "sf", "ggmap")
libraries(my_packages)



# Exploracion de datos del censo

# DESCARGA EL MAPA PARA BAJA CALIFORNIA

maptj <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02a.shp")

# DESCARGA LA BASE DE DATOS DEL CENSO

dbcenso <- read.csv("C:/Users/pvelazquez/Desktop/CENSO VIVIENDA/resultados_ageb_urbana_02_cpv2010/conjunto_de_datos/resultados_ageb_urbana_02_cpv2010.csv", na.strings = c("*"), stringsAsFactors = FALSE)

# LIMPIA LA BASE DE DATOS DEL CENSO PARA OBTENER LOS TOTALES POR AGEB

bdc <- dbcenso %>%
  rename_all(funs(str_to_upper(.))) %>%
  rename(., 
         "CVE_ENT"   = "Ï..ENTIDAD",
         "CVE_MUN"   = "MUN",
         "CVE_AGEB"  = "AGEB",
         "CVE_MZA"   = "MZA",
         "CVE_LOC"   = "LOC",
         "MUNICIPIO" = "NOM_MUN") %>%
  mutate(CVE_ENT     = as.character(CVE_ENT) ,
         CVE_MUN     = as.character(CVE_MUN),
         CVE_AGEB    = as.character(CVE_AGEB),
         CVE_MZA     = as.character(CVE_MZA),
         CVE_LOC     = as.character(CVE_LOC),
         CVE_ENT     = str_pad(CVE_ENT, pad = 0, side = c("left"), width = 2),
         CVE_MUN     = str_pad(CVE_MUN, pad = 0, side = c("left"), width = 3),
         CVE_MZA     = str_pad(CVE_MZA, pad = 0, side = c("left"), width = 3),
         CVE_LOC     = str_pad(CVE_LOC, pad = 0, side = c("left"), width = 4)) %>%
  filter(MUNICIPIO %in% c("Tijuana") & !CVE_LOC %in% c("0000") & !CVE_AGEB %in% c("0000") & !CVE_MZA %in% c("000")) %>%
  group_by(CVE_AGEB) %>%
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = funs(sum = "sum"), na.rm = TRUE)

# Exploracion de base de datos DENUE

# DESCARGA LA BASE DE DATOS DE LA DENUE

dbdenue <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv", guess_max = 100000)


denue_ageb <- dbdenue %>%
  rename_all(funs(str_to_upper(.))) %>%
  rename(.,
         "CVE_AGEB" = "AGEB",
         "CVE_MZA" = "MANZANA") %>%
  filter(MUNICIPIO %in% c("Tijuana")) %>%
  group_by(CVE_AGEB) %>%
  summarise("x" = mean(LONGITUD),
            "y" = mean(LATITUD))


denue_clust <- dbdenue %>%
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

denue_clust <- st_as_sf(denue_clust, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  st_transform(crs = 4326)

# Calcula la matriz de distancias

mdist <- st_distance(denue_clust)

# Establece los parametros del algoritmo de clusters

hc <- hclust(as.dist(mdist), method = "complete")

# Establece la distancia en metros donde se corta el algoritmo que clasifica
# los clusters

d = 6000

# Agrega la columna de clusters a los datos

denue_clust$clust <- cutree(hc, h = d)


denue_clust <- denue_clust %>%
  st_set_geometry(NULL) %>%
  distinct(CVE_AGEB, clust)


acdc <- left_join(bdc, denue_ageb, by = ("CVE_AGEB"))

# Unir los cluster con el mapa de poligonos de ageb de Tijuana

serv_pub <- st_read("C:/Users/pvelazquez/Google Drive/MEXICO MAPA/Baja California/conjunto de datos/02a.shp")


serv_pub <- serv_pub %>%
  filter(CVE_MUN %in% c("004"))

tj_poly <- left_join(serv_pub, denue_clust, by = "CVE_AGEB") %>%
  st_transform(crs = "+init=epsg:4326")

# Unir polígonos de cluster con datos del censo
tj_poly <- left_join(tj_poly, bdc, by = "CVE_AGEB")

# Gráficas por AGEB

tj_pea <- tj_poly %>%
  group_by(clust) %>%
  summarise(spea = sum(PEA_sum, na.rm = TRUE)) %>%
  mutate(clust = as.factor(clust)) %>%
  st_centroid()

##########################################################################
#### SCRIPT PARA OBTENER EL MAPA DE TIJUANA ##############################
##########################################################################
##########################################################################

tjlocation <- c(lon = -116.944333,
                lat = 32.491566)
tjmap <- get_map(tjlocation, zoom = 11, maptype = c("roadmap"))

ggmap(tjmap) + 
  geom_sf(data = tj_poly, inherit.aes = FALSE, mapping = aes(colour = POBTOT_sum), alpha = 0.6)
















