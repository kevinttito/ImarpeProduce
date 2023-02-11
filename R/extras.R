

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



