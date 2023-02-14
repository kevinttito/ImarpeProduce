# Función Longitud Peso ---------------------------------------------------


Length_weight <- function(Length ,a , b) {
  w = a * (Length^b)
  return(w)
}


# Función de Ponderación --------------------------------------------------


Ponderacion <- function(frecuencia, captura, tallas, a, b) {

  if (sum(frecuencia, na.rm = TRUE) != 0) {

    peso <- Length_weight(Length = tallas,a = a,b = b) * frecuencia
    talla_ponderada <- (captura / sum(peso, na.rm = TRUE)) * frecuencia

  } else {

    talla_ponderada <- rep(0, length(tallas))

  }

  return(talla_ponderada)

}


# Convirtiendo de número a peso -------------------------------------------


number_to_weight <- function(data, tallas, a, b, fileEncoding = "latin1"){

  peso <- as.data.frame(t(apply(data[, as.character(tallas)], 1 , function(x)Length_weight(Length = tallas,a = a,b = b)*x)))
  id <- setdiff(names(data), as.character(tallas))
  peso <- cbind(data[,id], peso)
  return(peso)

}


# Estimar porcentaje de juveniles -----------------------------------------


Porc_Juveniles <- function(data, tallas, juvLim = 12) {

  juv <- 100*(sum(data[tallas < juvLim], na.rm = TRUE)/sum(data, na.rm = TRUE))

  return(juv)

}


# Rangos de las tallas ----------------------------------------------------


min_range <- function(data) {

  data[data==0] <- NA
  min_v <- min(as.numeric(names(data)[!is.na(data)]))

  return(min_v)

}


max_range <- function(data) {

  data[data==0] <- NA
  max_v <- max(as.numeric(names(data)[!is.na(data)]))

  return(max_v)

}


# Talla Media -------------------------------------------------------------


Talla_Media <- function (freq, talla) {

  if(is.numeric(freq)){

    freq <- freq

  } else {

    freq <- as.numeric(freq)

  }

  freq[is.na(freq)] <- 0
  tallaMedia <- sum(talla*freq)/sum(freq)

  return(tallaMedia)

}


# Agrega estadísticos pesqueros -------------------------------------------


estadisticos <- function(data, tallas = seq(5,20,0.5)) {

  ### Juveniles
  data$juv = apply(X = data[as.character(tallas)], MARGIN = 1, FUN = Porc_Juveniles, tallas = tallas)

  ### Rangos
  data$max = apply(X = data[as.character(tallas)], MARGIN = 1, FUN = max_range)
  data$min = apply(X = data[as.character(tallas)], MARGIN = 1, FUN = min_range)

  ### Talla Media
  data$Tmedia = apply(X = data[as.character(tallas)], MARGIN = 1, FUN = Talla_Media, talla = tallas)

  return(data)

}



# Hallar Modas ------------------------------------------------------------

getModes = function(x, tallas = seq(5,20,0.5),thr = 0.03) {
  names(x) = tallas
  x = .removeTails(x)
  y = rle(x)
  yValue  = y$value
  yLength = which(y$length!=1)
  values = as.numeric(names(yValue))
  newValues = rowMeans(cbind(values[yLength-1], values[yLength+1]))
  values[yLength] = newValues
  names(yValue) = values
  xdif  = rle(sign(diff(yValue)))$value
  modes = as.numeric(names(xdif[xdif==1]))
  x = x/sum(x, na.rm=TRUE)
  modes = c(na.omit(modes[x[as.character(modes)]>thr]))
  return(modes)
}

