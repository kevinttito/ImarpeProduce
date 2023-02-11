# Removiendo espacios y letras --------------------------------------------

removeSpace_Letter <- function(vector) {

  out <- gsub(pattern = "[a-zA-Z-]", replacement = "", x = vector)
  out <- str_replace_all(string = out, pattern = "\\p{WHITE_SPACE}", replacement = "")
  return(out)

}

# Convert Longitud y Latitud ----------------------------------------------

convert_lon_lat = function(x, vctr_nams = c("grado","minutos","segundos")){

  # grado = as.numeric(sub("Â°.*","",sub("\\ï¿½.*", "", gsub("-","", x))))
  grado = as.numeric(sub("°.*","",sub("[ø].*", "", gsub("-","", x))))
  grado = ifelse(grado > 0, grado, -1*grado)
  # minutes = as.numeric(sub(".*\\°","",sub("[0-99]*\\ï¿½.", "", sub("\\'.*", "", x))))
  minutes = as.numeric(sub(".*\\°","",sub("[0-999]*\\ø.", "", sub("\\'.*", "", x))))
  minutes = ifelse(minutes > 0, minutes, -1*minutes)
  seconds = as.numeric(sub("[']","",substring(x,(nchar(x)-4),(nchar(x)-2))))
  seconds = ifelse(seconds > 0, seconds, -1*seconds)
  lon_lat = data.frame(grado, minutes, seconds)
  return(lon_lat)

}


# Filtrar solo muestreado -------------------------------------------------

obtener_solo_muestra = function(x, marcas = seq(5,20,0.5)){

  total = apply(x[,as.character(marcas)],1, sum, na.rm = TRUE)
  x = x[total > 0,]

  return(x)

}


# MCD 2 -------------------------------------------------------------------

mcd2 = function (x, y) {
  mayor = max(x, y)
  menor = min(x, y)
  r = mayor %% menor
  if (!(x & y)) {
    menor = mayor
  } else {
    while (r != 0) {
      mayor = r
      r = menor %% r
      menor = mayor
    }
  }
  return(abs(menor))
}


# MCD ---------------------------------------------------------------------

mcd = function (...) {

  numeros = c(...)
  resultado = mcd2(numeros[1], numeros[2])
  if (length(numeros) > 2) {
    for(n in numeros[3:length(numeros)]) {
      resultado = mcd2(resultado, n)
    }
  }

  return(abs(resultado))

}