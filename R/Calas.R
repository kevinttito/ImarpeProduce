
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
  calas$lat_end_pro = calas$lat_end_pro * -1

  calas$lon_end_pro = calas$grado_lon + calas$minutes_lon/60 + calas$seconds_lon/3600
  calas$lon_end_pro = calas$lon_end_pro * -1

  calas = calas %>% dplyr::filter(gear %in% c("RED DE CERCO")) %>% select(id_faena, n_cala, start_date, end_date, lon_end_pro, lat_end_pro, description , gear, catch_kg, status)

  return(calas)

}
