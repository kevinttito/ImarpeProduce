

# Indicadores -------------------------------------------------------------

obtieneIndicadores = function(tallas2, marcas = seq(5,20,0.5)){


  indicadores <- data.frame(id_faena = tallas2$id_faena,
                            stringsAsFactors = FALSE)


  tallas2[tallas2 == 0] = NA

  # min
  fx = min
  indicadores$min <- apply(tallas2[, as.character(marcas)], 1, fx, na.rm = TRUE)

  # max
  fx = max
  indicadores$max <- apply(tallas2[, as.character(marcas)], 1, fx, na.rm = TRUE)

  # primera marca
  fx = function(x){as.numeric(names(x)[!is.na(x)])[1]}
  indicadores$first <- apply(tallas2[, as.character(marcas)], 1, fx)

  # ultima marca
  fx = function(x){rev(as.numeric(names(x)[!is.na(x)]))[1]}
  indicadores$last <- apply(tallas2[, as.character(marcas)], 1, fx)

  # num. ind. primera marca
  fx = function(x){as.numeric(x[!is.na(x)])[1]}
  indicadores$nfirst <- apply(tallas2[, as.character(marcas)], 1, fx)

  # num. ind. ultima marca
  fx = function(x){rev(as.numeric(x[!is.na(x)]))[1]}
  indicadores$nlast <- apply(tallas2[, as.character(marcas)], 1, fx)

  # amplitud (numero de marcas desde la minima hasta la maxima)
  indicadores$amplitud <- 2 * (indicadores$max - indicadores$min) + 1

  # suma
  fx = sum
  indicadores$suma <- apply(tallas2[, as.character(marcas)], 1, fx, na.rm = TRUE)

  # NA 1.0 cm
  fx = function(x){length(x[is.na(x)]) == length(seq(marcas[1], marcas[length(marcas)], 1))}
  indicadores$NA1.0 <- apply(tallas2[, as.character(seq(marcas[1], marcas[length(marcas)], 1))], 1, fx)

  # NA 0.5 cm
  fx = function(x){length(x[is.na(x)]) == length(seq(marcas[2], marcas[length(marcas)-1], 1))}
  indicadores$NA0.5 <- apply(tallas2[, as.character(seq(marcas[2], marcas[length(marcas)-1], 1))], 1, fx)

  # mcd
  fx = function(x){y <- x[!is.na(x)]; if(length(y)>1) mcd(y) else NA}
  indicadores$mcd <- apply(tallas2[, as.character(seq(marcas[1], marcas[length(marcas)], 0.5))], 1, fx)
  indicadores$mcd[is.na(indicadores$mcd)] <- 1

  return(indicadores)

}



# Filtro de tallas --------------------------------------------------------



filtro_tallas_viaje = function(tallas_viaje, marcas = seq(5,20,0.5), nIndividuos = 500, tallaMin = 5, tallaMax = 18, nIndExtremo = 10){

  # Define el número de individuos de los extremos de la frecuencia a partir del cual se considera "muy alto"

  tabla_indicadores_filtrar = obtieneIndicadores(tallas_viaje,marcas = marcas)

  ### Seccion 4. Identificación de caracter??sticas de cada muestreo
  muestreos <- data.frame(id_faena = tabla_indicadores_filtrar$id_faena,
                          stringsAsFactors = FALSE)

  ### Seccion 4.1. Identificación de muestreos con un número de individuos medidos excesivamente alto
  muestreos$obs6 <- tabla_indicadores_filtrar$suma > nIndividuos

  ### Seccion 4.2. Identificación de muestreos con individuos de una sola talla
  muestreos$obs3 <- tabla_indicadores_filtrar$amplitud == 1

  ### Seccion 4.3. Identificación de muestreos que posiblemente fueron multiplicados porun valor constante
  muestreos$obs4 <- tabla_indicadores_filtrar$mcd > 1

  ### Seccion 4.4. Identificación de muestreos con individuos de tallas muy pequeñas o muy grandes
  muestreos$obs1 <- tabla_indicadores_filtrar$first <= tallaMin | tabla_indicadores_filtrar$last >= tallaMax

  ### Seccion 4.5. Identificación de muestreos que inician y terminan con un valor alto
  muestreos$obs2 <- tabla_indicadores_filtrar$nfirst >= nIndExtremo & tabla_indicadores_filtrar$nlast >= nIndExtremo

  ### Seccion 4.6. Identificación de muestreos con individuos de tallas únicamente enteras
  muestreos$obs5 <- tabla_indicadores_filtrar$NA0.5 & !tabla_indicadores_filtrar$NA1.0

  ### Seccion 5. Resultados

  muestreos = subset(muestreos, !obs1)
  muestreos = subset(muestreos, !obs2)
  muestreos = subset(muestreos, !obs3)
  muestreos = subset(muestreos, !obs4)
  muestreos = subset(muestreos, !obs5)
  muestreos = subset(muestreos, !obs6)

  tallas_viaje = tallas_viaje[match(muestreos$id_faena,tallas_viaje$id_faena),]

  return(tallas_viaje)

}



