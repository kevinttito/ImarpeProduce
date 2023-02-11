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

  list_calas = list.files(path = path,pattern = pattern, full.names = TRUE, ...)


  tablas_calas = lapply(list_calas, read.csv, skip = 5, fileEncoding = "latin1", stringsAsFactors = FALSE, header = FALSE)

  tablas_calas = do.call("rbind", tablas_calas)

  names(tablas_calas) = c("id", "x1","id_faena","n_cala","start_date","x2","x3","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")

  tablas_calas$description =  str_replace_all(string = tablas_calas$description,pattern = "\\p{WHITE_SPACE}",replacement = "")

  tablas_calas = tablas_calas[,c("id_faena","n_cala","start_date","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")]

  tablas_calas = tablas_calas[!duplicated(tablas_calas[,c("id_faena","n_cala","start_date","end_date","start_lat","start_lon","end_lat","end_lon","gear","description","catch_kg","status","origin","record_date")]),]

  return(tablas_calas)

}


faenas = read_calas(path = "D:/Pesca_anchoveta/Pesca_anchoveta_2022_II_nc/produce/data/calas/",pattern = ".csv")
