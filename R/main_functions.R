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













