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

  library(fenix)

  tallas_lance = calas_tallas_Total %>% dplyr::filter(!is.na(Fstart) & !is.na(Fend)) %>% mutate(fecha_start = dmy_hm(substring(Fstart,first = 1,last = 16)), fecha_fin = dmy_hm(substring(Fend,first = 1,last = 16)), fecha_start = dmy_hm(substring(Fstart,first = 1,last = 16)), dur_calas = as.numeric(difftime(fecha_fin,fecha_start,units = "hours")),description = ifelse(gsub("[  ]*","",description) %in% "",NA,gsub("[  ]*","",description))) %>% ungroup()


  calas_sp = tallas_lance %>% dplyr::group_by(id_faena) %>% dplyr::summarise(calas = max(n_cala), n_sp = length(which(!is.na(unique(description))))) %>% ungroup()


  dur_calas_total = tallas_lance %>% dplyr::filter(!is.na(dur_calas), dur_calas > min_dur_calas, dur_calas <= max_dur_calas, !duplicated(.[,c("id_faena","n_cala")])) %>% dplyr::group_by(id_faena) %>% dplyr::summarise(dur_calas = mean(as.numeric(dur_calas))) %>% ungroup()


  position = tallas_lance %>% dplyr::group_by(id_faena) %>% dplyr::filter(n_cala %in% max(n_cala, na.rm = TRUE)) %>% select(id_faena, lon_end_pro,lat_end_pro) %>% ungroup()

  esfuerzo = merge(merge(calas_sp, position, by = "id_faena", all = TRUE), dur_calas_total, by = "id_faena", all = TRUE)


  esfuerzo = esfuerzo[match(descargas$id_faena, esfuerzo$id_faena,nomatch = 0),]

  esfuerzo$dc_pro = Distancia_Costa(data = esfuerzo,colLon = match(x = "lon_end_pro",table = names(esfuerzo)),colLat = match(x = "lat_end_pro",table = names(esfuerzo)))

  return(esfuerzo)

}





