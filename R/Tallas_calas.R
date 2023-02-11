
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


# Tratar Tallas Calas -----------------------------------------------------


tratar_tallas_calas = function(tallas_calas){

  tallas_calas = tallas_calas %>% dplyr::filter(!is.na(n_cala))

  tallas_calas$n = 1:nrow(tallas_calas)

  tallas_calas = tallas_calas %>% select(n, id_faena, n_cala, description, length, freq)

  tallas_calas = tallas_calas %>% select(-"n") %>% spread("length","freq")

  return(tallas_calas)

}

