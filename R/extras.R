
# Removiendo espacios y letras --------------------------------------------

removeSpace_Letter <- function(vector) {

  out <- gsub(pattern = "[a-zA-Z-]", replacement = "", x = vector)
  out <- str_replace_all(string = out, pattern = "\\p{WHITE_SPACE}", replacement = "")
  return(out)

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


# Distancia Costa ---------------------------------------------------------

.shoreDistance = function(data) {

  grados2mn  = 60 * 180 / pi
  grados2rad = pi/180

  shore_rad = Shoreline_Peru * grados2rad
  x_rad = data[2] * grados2rad
  y_rad = data[1] * grados2rad

  xy_rad = sin(y_rad) * sin(shore_rad$Lat)
  yx_rad = cos(y_rad) * cos(shore_rad$Lat) * cos(shore_rad$Long - x_rad)
  dist = min(acos(xy_rad + yx_rad)) * grados2mn

  return(dist)

}


# Calcula Distancia Costa -------------------------------------------------


Distancia_Costa = function(data, colLon, colLat){

  new_data = data[,c(colLat, colLon)]
  data$dc = apply(new_data, 1, .shoreDistance)
  return(data)

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


# Determinar_areas --------------------------------------------------------


area_iso = function(data, colLat, colDC){

  new_data = data[,c(colLat,colDC)]
  names(new_data) = c("lat","dc")

  new_data[is.na(new_data)] = 0

  data$area = NA
  data$grad_cat = NA
  data$dc_cat = NA

  for(i in 1:nrow(new_data)){

    fg = areas_isoparalitorales[new_data$lat[i] >= -1*areas_isoparalitorales$grad & new_data$dc[i] < areas_isoparalitorales$dc,]

    areas = subset(fg, subset = fg$grad %in% min(fg$grad) & fg$dc %in% min(fg$dc),select = c("area","grad","dc"))[1,]

    data[i,c("area","grad_cat","dc_cat")] = areas

  }

  return(data)


}



# Corrigiendo Puertos -----------------------------------------------------

CorrigiendoPuertos = function(data){
  data$puerto[data$puerto == "carqu??n"] = "huacho"
  data$puerto[data$puerto == "carquin"] = "huacho"
  data$puerto[data$puerto == "Carquin"] = "huacho"
  data$puerto[data$puerto == "Carquín"] = "huacho"
  data$puerto[data$puerto == "carquín"] = "huacho"

  data$puerto[data$puerto == "carquï¿½n"] = "huacho"
  data$puerto[data$puerto == "Carquï¿½n"] = "huacho"
  data$puerto[data$puerto == "carqu¡n"] = "huacho"
  data$puerto[data$puerto == "Carqu¡n"] = "huacho"
  data$puerto[data$puerto == "coishco"] = "chimbote"
  data$puerto[data$puerto == "malabrigo"] = "chicama"
  data$puerto[data$puerto == "paracas (pisco)"] = "pisco"
  data$puerto[data$puerto == "bayovar"] = "parachique"
  data$puerto[data$puerto == "pacocha"] = "ilo"
  data$puerto[data$puerto == "matarani"] = "mollendo"
  data$puerto[data$puerto == "Chimbote "] = "chimbote"
  data$puerto[data$puerto == "chimbote "] = "chimbote"
  return(data)
}


# Peso Juvenil Adulto -----------------------------------------------------

add_peso_juv_adulto = function(data, tallas){

  data$juv_peso = apply(data[,as.character(tallas[tallas<12])],1,sum)
  data$adu_peso = apply(data[,as.character(tallas[tallas>=12])],1,sum)
  return(data)

}


# add Viaje ---------------------------------------------------------------

addEsfuerzo = function(data, tallas){

  data$n = 1
  data$total = apply(data[as.character(tallas)],1,sum, na.rm = TRUE)
  data$n_m = ifelse(data$total == 0, 0, 1)
  return(data)

}



# Remover Colas -----------------------------------------------------------


.removeTails = function(x) {
  y = sign(abs(x))
  r = rle(y)
  if(r$value[1]==0 & r$length[1]>1) x = tail(x, -r$length[1]+1)
  if(tail(r$value, 1)==0 & tail(r$length, 1)>1) x = head(x, -tail(r$length, 1)+1)
  return(x)
}





