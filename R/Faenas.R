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
