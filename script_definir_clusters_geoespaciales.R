library(easypackages)
my_packages <- c("tidyverse", "lsr")
libraries(my_packages)

# Descarga la base de datos de DENUE

dbdenue <- read_csv("/Users/pvelazquez/Documents/PROYECTOS/INDUSTRIAL/DATOS/02_0317/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv", guess_max = 100000)

db <- dbdenue %>%
  filter(municipio %in% c("Tijuana")) %>%
  mutate_all(funs(iconv(.,from = "UTF-8",to='ASCII//TRANSLIT'))) %>%
  mutate(nombre_act       = as_factor(nombre_act,
                                      levels(.$nombre_act),
                                      labels(.$nombre_act)),
         per_ocu          = as_factor(per_ocu,
                                      levels(.$per_ocu),
                                      labels(.$per_ocu)),
         codigo_act       = as_factor(as.character(codigo_act),
                                      levels(.$codigo_act),
                                      labels(.$codigo_act)),
         tipo_vial        = as_factor(tipo_vial,
                                      levels(.$tipo_vial),
                                      labels(.$tipo_vial)),
         tipo_v_e_1       = as_factor(tipo_v_e_1,
                                      levels(.$tipo_v_e_1),
                                      labels(.$tipo_v_e_1)),
         tipo_v_e_2       = as_factor(tipo_v_e_2,
                                      levels(.$tipo_v_e_2),
                                      labels(.$tipo_v_e_2)),
         tipo_v_e_3       = as_factor(tipo_v_e_3,
                                      levels(.$tipo_v_e_3),
                                      labels(.$tipo_v_e_3)),
         tipo_asent       = as_factor(tipo_asent,
                                      levels(.$tipo_asent),
                                      labels(.$tipo_asent)),
         tipoCenCom       = as_factor(tipoCenCom,
                                      levels(.$tipoCenCom),
                                      labels(.$tipoCenCom)),
         cve_ent          = as_factor(cve_ent,
                                      levels(.$cve_ent),
                                      labels(.$cve_ent)),
         cve_loc          = as_factor(cve_loc,
                                      levels(.$cve_loc),
                                      labels(.$cve_loc)),
         localidad        = as_factor(localidad,
                                      levels(.$cve_loc),
                                      labels(.$cve_loc)),
         ageb             = as_factor(ageb,
                                      levels(.$ageb),
                                      labels(.$ageb)),
         manzana          = as_factor(manzana,
                                      levels(.$manzana),
                                      labels(.$manzana)),
         tipoUniEco       = as_factor(tipoUniEco,
                                      levels(.$tipoUniEco),
                                      labels(.$tipoUniEco)),
         longitud         = as.numeric(longitud),
         latitud          = as.numeric(latitud),
         codigo_industria = str_sub(codigo_act, 1, 2)) %>% 
  separate(fecha_alta,into = c("fa_m", "fa_y")) %>%
  saveRDS("denue.rds")







ymax <- max(db$latitud)
ymin <- min(db$latitud)

xmax <- max(db$longitud)
xmin <- min(db$longitud)

w <- owin(xrange = c(xmin, xmax), yrange = c(ymin, ymax))
tijuana <- ppp(db$longitud, db$latitud, window = w)

plot(tijuana)


library(spatstat)

st_as_sf

dbspat <- st_as_sf(db, coords = c("x", "y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
  st_transform(crs = 4326)



