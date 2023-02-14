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



# Uniendo calas a tallas calas --------------------------------------------


merge_callas_tallas = function(calas, tallas_calas){

  calas_limpias = tratar_calas(calas = calas)

  tallas_calas_limpias = tratar_tallas_calas(tallas_calas = tallas_calas)

  tallas_lance = merge(calas_limpias, tallas_calas_limpias, by = c("id_faena","n_cala","description"), all = TRUE)

  return(tallas_lance)

}


# Get Efforts -------------------------------------------------------------


filtrando_obteniendo_esfuerzo = function(calas_tallas_Total, descargas, min_dur_calas, max_dur_calas){

  tallas_lance = calas_tallas_Total %>% dplyr::filter(!is.na(start_date) & !is.na(end_date)) %>%
    mutate(fecha_start = lubridate::dmy_hm(substring(start_date,first = 1,last = 16)),
           fecha_fin = lubridate::dmy_hm(substring(end_date,first = 1,last = 16)),
           dur_calas = as.numeric(difftime(fecha_fin,fecha_start,units = "hours")),
           description = ifelse(gsub("[  ]*","",description) %in% "",NA,gsub("[  ]*","",description))) %>%
    ungroup()


  calas_sp = tallas_lance %>% dplyr::group_by(id_faena) %>%
    dplyr::summarise(calas = max(n_cala), n_sp = length(which(!is.na(unique(description))))) %>%
    ungroup()


  dur_calas_total = tallas_lance %>% dplyr::filter(!is.na(dur_calas), dur_calas > min_dur_calas, dur_calas <= max_dur_calas, !duplicated(.[,c("id_faena","n_cala")])) %>%
    dplyr::group_by(id_faena) %>%
    dplyr::summarise(dur_calas = mean(as.numeric(dur_calas))) %>%
    ungroup()


  position = tallas_lance %>% dplyr::group_by(id_faena) %>%
    dplyr::filter(n_cala %in% max(n_cala, na.rm = TRUE)) %>%
    dplyr::select(id_faena, lon_end_pro,lat_end_pro) %>% ungroup()


  esfuerzo = merge(merge(calas_sp, position, by = "id_faena", all = TRUE), dur_calas_total, by = "id_faena", all = TRUE)


  esfuerzo = esfuerzo[match(descargas$id_faena, esfuerzo$id_faena,nomatch = 0),]

  esfuerzo = Distancia_Costa(data = esfuerzo,colLon = match(x = "lon_end_pro",table = names(esfuerzo)),colLat = match(x = "lat_end_pro",table = names(esfuerzo)))

  return(esfuerzo)

}


# Get total efforts -------------------------------------------------------


obtener_esfuerzo_total = function(faenas, esfuerzo_calas, min_dv = 10, max_dv = 96){

  faenas_limpias = ordenar_faenas(faenas = faenas)

  data_esfuerzo = merge(esfuerzo_calas, faenas_limpias, by = "id_faena", all = TRUE)

  data_esfuerzo = data_esfuerzo[!duplicated(data_esfuerzo),]
  data_esfuerzo = data_esfuerzo %>% dplyr::filter(dV <= max_dv,  dV >= min_dv)

  return(data_esfuerzo)

}


# Uniendo descarga tallas -------------------------------------------------


uniendo_descarga_tallas = function(descargas, tallas_viaje){

  data_total = merge(descargas, tallas_viaje, by = c("id_faena","descarga","id_matricula"), all = TRUE)

  data_total = data_total %>% dplyr::filter(!is.na(F_fin_desembarque)) %>%
    dplyr::mutate(F_ini_desembarque = lubridate::dmy_hm(substring(F_ini_desembarque,first = 1,last = 16)),
                  F_fin_desembarque = lubridate::dmy_hm(substring(F_fin_desembarque,first = 1,last = 16)),
                  F_ini_descarga = lubridate::dmy_hm(substring(F_ini_descarga,first = 1,last = 16)),
                  F_fin_descarga = lubridate::dmy_hm(substring(F_fin_descarga,first = 1,last = 16)))

  return(data_total)


}


# Uniendo descarga esfuerzo -----------------------------------------------

uniendo_descarga_esfuerzo = function(descargas_viajes, data_esfuerzo, difftime = 7, marcas = seq(5,20,0.5)){

  descargas_viajes = merge(descargas_viajes, data_esfuerzo, by = "id_faena", all = TRUE) %>% dplyr::filter(!is.na(descarga))

  descargas_viajes = descargas_viajes %>%
    dplyr::mutate(fecha = lubridate::ymd(format((F_ini_descarga - lubridate::hours(difftime)), "%Y-%m-%d"))) %>%
    dplyr::mutate_at(., as.character(marcas),function(x)as.numeric(as.character(x)))

  descargas_viajes = addEsfuerzo(descargas_viajes)

  descargas_viajes$tipo.de.flota = lanchas[match(descargas_viajes$id_matricula,lanchas$id_matricula),"TIPO.DE.CASCO"]

  descargas_viajes = descargas_viajes %>% dplyr::mutate(tipo.de.flota = ifelse(tipo.de.flota %in% "MADERA","IND MAD","IND"))

  return(descargas_viajes)

}


# Variables espaciales ----------------------------------------------------

generando_var_spaciales = function(data_total, dc_max = 100){

  data_total = data_total[!duplicated(data_total[,c("id_faena")]),]

  data_total = data_total %>% mutate(dc_pro = ifelse( dc_pro > dc_max , NA, dc_pro))

  data_total = area_iso(data = data_total,colLon = match(x = "lon_end_pro",table = names(data_total)),colLat = match(x = "lat_end_pro",table = names(data_total)))

  names(data_total) = c(names(data_total),"area_pro","lat_pro_cat","dc_pro_cat")

  data_total = CorrigiendoPuertos(data = data_total)

  return(data_total)

}



