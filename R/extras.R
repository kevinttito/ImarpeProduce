
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



area_iso = function(data, colLon, colLat){


  xys = st_as_sf(areas_isoparalitorales, coords=c("lon","lat"), crs = st_crs(4326))

  polys = xys %>%
    dplyr::group_by(area, grad, dc) %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    st_convex_hull()

  new_data = data[,c(colLon,colLat)]
  names(new_data) = c("lon","lat")

  new_data[is.na(new_data)] = 0

  pt_S = st_as_sf(new_data, coords = c("lon","lat"), crs = st_crs(4326))

  xg = apply(st_intersects(polys, pt_S, sparse = FALSE), 2,
             function(col){as.numeric(polys[which(col),c("area","grad","dc")])[1:3]})

  xg= as.data.frame(t(xg))
  names(xg) = c("area_iso","grad_cat","dc_cat")

  return(cbind(data,xg))


}










