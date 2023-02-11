# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# Faenas ------------------------------------------------------------------


read_faenas = function(path, pattern = ".csv",...) {

  list_faenas = list.files(path = path,pattern = pattern, full.names = TRUE, ...)

  tablas_faenas = lapply(list_faenas, read.csv, skip = 5, fileEncoding = "latin1", stringsAsFactors = FALSE, header = FALSE)

  tablas_faenas = do.call("rbind",tablas_faenas)

  names(tablas_faenas) = c("id", "x1","armador","vessel","x2","x3","id_vessel","start_port","start_date","end_date","id_faena","n_calas","status","origin","record_date")

  tablas_faenas = tablas_faenas[,c("armador","vessel","id_vessel","start_port","start_date","end_date","id_faena","n_calas","status","origin","record_date")]

  tablas_faenas = tablas_faenas[!duplicated(tablas_faenas[,c("vessel","id_vessel","start_port","start_date", "id_faena","record_date")]),]

  return(tablas_faenas)

}


# Calas -------------------------------------------------------------------


read_calas = function(path, pattern = ".csv",...) {

  list_calas = list.files(path = path, pattern = pattern, full.names = TRUE, ...)

  tablas_calas = lapply(list_calas, read.csv, skip = 5, fileEncoding = "latin1", stringsAsFactors = FALSE, header = FALSE)

  tablas_calas = do.call("rbind", tablas_calas)

  names(tablas_calas) = c("id", "x1","id_faena","n_cala","start_date","x2","x3","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")

  tablas_calas$description =  str_replace_all(string = tablas_calas$description,pattern = "\\p{WHITE_SPACE}",replacement = "")

  tablas_calas = tablas_calas[,c("id_faena","n_cala","start_date","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")]

  tablas_calas = tablas_calas[!duplicated(tablas_calas[,c("id_faena","n_cala","start_date","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")]),]

  return(tablas_calas)

}


# Tallas Calas ------------------------------------------------------------

read_tallas_calas = function(path, pattern = ".csv", ...){

  list_calas_tallas = list.files(path = path, pattern = pattern, full.names = TRUE, ...)

  tablas_calas_tallas = lapply(list_calas_tallas, read.csv, skip = 5, fileEncoding = "latin1", stringsAsFactors = FALSE, header = FALSE)

  tablas_calas_tallas = do.call("rbind", tablas_calas_tallas)

  names(tablas_calas_tallas) = c("id", "x1","id_faena","n_cala","description","x2","x3","length","measurement_unit","freq")

  tablas_calas_tallas$description =  str_replace_all(string = tablas_calas_tallas$description,pattern = "\\p{WHITE_SPACE}",replacement = "")

  tablas_calas_tallas = tablas_calas_tallas[,c("id_faena","n_cala","description","length","measurement_unit","freq")]

  tablas_calas_tallas = tablas_calas_tallas[!duplicated(tablas_calas_tallas[,c("id_faena","n_cala","description","length","measurement_unit")]),]

  return(tablas_calas_tallas)


}



# Tratar Calas ------------------------------------------------------------


tratar_calas = function(calas){

  lat_end_pro = convert_lon_lat(calas$start_lat, vctr_nams = c("grado_lat","minutos_lat","segundos_lat"))
  names(lat_end_pro) = c("grado_lat", "minutes_lat", "seconds_lat")

  lon_end_pro = convert_lon_lat(calas$start_lon, vctr_nams = c("grado_lon","minutos_lon","segundos_lon"))
  names(lon_end_pro) = c("grado_lon", "minutes_lon", "seconds_lon")

  calas = cbind(calas, lat_end_pro)
  calas = cbind(calas, lon_end_pro)

  calas = calas %>% dplyr::filter(minutes_lat < 60, seconds_lat < 60, minutes_lon < 60, seconds_lon < 60)

  calas$lat_end_pro = calas$grado_lat + calas$minutes_lat/60 + calas$seconds_lat/3600

  calas$lon_end_pro = calas$grado_lon + calas$minutes_lon/60 + calas$seconds_lon/3600

  calas = calas %>% dplyr::filter(gear %in% c("RED DE CERCO")) %>% select(id_faena, n_cala, start_date, end_date, lon_end_pro, lat_end_pro, description , gear, catch_kg, status)

  return(calas)

}



# Tratar Tallas Calas -----------------------------------------------------


tratar_tallas_calas = function(tallas_calas){

  tallas_calas = tallas_calas %>% dplyr::filter(!is.na(n_cala))

  tallas_calas$n = 1:nrow(tallas_calas)

  tallas_calas = tallas_calas %>% select(n, id_faena, n_cala, description, length, freq)

  tallas_calas = tallas_calas %>% select(-"n") %>% spread("length","freq")

  return(tallas_calas)

}


# Uniendo calas a tallas calas --------------------------------------------


merge_callas_tallas = function(calas, tallas_calas){

  calas_limpias = tratar_calas(calas = calas)

  tallas_calas_limpias = tratar_tallas_calas(tallas_calas = tallas_calas)

  tallas_lance = merge(calas_limpias, tallas_calas_limpias, by = c("id_faena","n_cala","description"), all = TRUE)

  return(tallas_lance)

}


# Descarga ----------------------------------------------------------------


tratamiento_descargas = function(data_descarga, ports = c("Callao","Malabrigo","Chimbote","Coishco", "Chancay", "Tambo de Mora","Paracas (Pisco)", "Samanco","Supe", "Bayovar","Carqu¡n","Vegueta")){

  names(data_descarga) = c("region","puerto","id_planta","fabrica","localidad","armador","emb","matricula","acta_des","F_ini_desembarque","F_fin_desembarque","act_descarga","F_ini_descarga","F_fin_descarga","cb","tm_declarada","descarga","n_report","parte_muestreo","n_juv","por_juv","moda","por_moda","id_faena")


  data_descarga = data_descarga %>% dplyr::filter(puerto %in% ports) %>% mutate(id_matricula = removeSpace_Letter(matricula), puerto = tolower(puerto)) %>% select(puerto, emb, id_matricula, F_ini_desembarque, F_fin_desembarque, F_ini_descarga, F_fin_descarga, cb, tm_declarada, descarga, id_faena)

  return(data_descarga)

}


# Pre Tratamiento de tallas -----------------------------------------------

pre_trat_tallas_viaje = function(tallas_viaje, marcas  = seq(5,20,0.5)){

  names(tallas_viaje) = c("region","puerto","emb","matricula","fecha_descarga","descarga","id_faena",marcas,"moda","obs")

  tallas_viaje = tallas_viaje %>% mutate(id_matricula = removeSpace_Letter(matricula), puerto = tolower(puerto)) %>% select(region, id_matricula, descarga, id_faena, as.character(marcas))

  tallas_viaje = obtener_solo_muestra(tallas_viaje)

  return(tallas_viaje)

}






