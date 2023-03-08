
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

.shoreDistance_un = function(data) {

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


Distancia_Costa = function(lat, lon) {

  grados2mn  = 60 * 180 / pi
  grados2rad = pi/180

  shore_rad = Shoreline_Peru * grados2rad
  x_rad = lon * grados2rad
  y_rad = lat * grados2rad

  xy_rad = lapply(y_rad, function(x) sin(x) * sin(shore_rad$Lat))

  yx_rad = Map(function(x, y) cos(x) * cos(shore_rad$Lat) * cos(shore_rad$Long - y), y_rad, x_rad)

  dist = unlist(Map(function(x, y) min(acos(x + y)) * grados2mn, xy_rad, yx_rad))

  return(dist)

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

area_iso = function(lat, dc){

  dc_cat = sapply(dc, function(x) min(subset(areas_grados_dc, dc > x, select = dc )))

  lat_cat = sapply(lat, function(x) max(subset(areas_grados_dc, grad < -1*x, select = grad )))

  lat_dc = data.frame(area = NA, dc_cat, lat_cat)

  for(i in 1:nrow(lat_dc)){

    lat_dc$area[i] = subset(areas_grados_dc, dc %in% lat_dc$dc_cat[i] & grad %in% lat_dc$lat_cat[i], select = area)

  }

  names(lat_dc) = c("area","lat_area","dc_area")

 return(lat_dc)

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





