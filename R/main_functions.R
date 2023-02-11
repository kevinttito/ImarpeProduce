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


read_faenas = function(path,pattern = ".csv",...) {

  list_faenas = list.files(path = path,pattern = pattern, full.names = TRUE, ...)

  tablas_faenas = lapply(list_faenas, read.csv, skip = 4, fileEncoding = "latin1", stringsAsFactors = FALSE)

  xd = length(tablas_faenas)

  new_data_faenas = tablas_faenas[[xd]]

  xd = xd - 1
  while (xd > 1) {

    new_data_faenas = rbind(new_data_faenas,tablas_faenas[[xd]])
    # print(dim(new_data_faenas))
    new_data_faenas = new_data_faenas[!duplicated(new_data_faenas[,c("Embarcación","Matrícula","Punto.de.Zarpe","Fecha.Inicio", "Codigo.Faena","Fecha.Registro")]),]
    xd = xd - 1
  }

  new_data_faenas =  new_data_faenas[,c("Armador","Embarcación","Matrícula","Punto.de.Zarpe","Fecha.Inicio","Fecha.Fin","Codigo.Faena","N..Calas","Estado","Origen","Fecha.Registro")]

  new_data_faenas = new_data_faenas[!duplicated(new_data_faenas[,c("Armador","Embarcación","Matrícula","Punto.de.Zarpe","Fecha.Inicio","Fecha.Fin","Codigo.Faena","N..Calas","Estado","Origen","Fecha.Registro")]),]


  return(new_data_faenas)

}
