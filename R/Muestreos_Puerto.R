# Pre Tratamiento de tallas -----------------------------------------------

pre_trata_tallas_viaje = function(tallas_viaje, marcas  = seq(5,20,0.5)){

  names(tallas_viaje) = c("region","puerto","emb","matricula","fecha_descarga","descarga","id_faena",marcas,"moda","obs")

  tallas_viaje = tallas_viaje %>% mutate(id_matricula = removeSpace_Letter(matricula), puerto = tolower(puerto)) %>% select(region, id_matricula, descarga, id_faena, as.character(marcas))

  tallas_viaje = obtener_solo_muestra(tallas_viaje)

  return(tallas_viaje)

}


# Tallas viaje ------------------------------------------------------------


tratamiento_tallas_viaje = function(tallas_viaje, marcas = seq(5,20,0.5), nIndividuos = 500, tallaMin = 5, tallaMax = 18, nIndExtremo = 10){

  tallas_viaje = pre_trat_tallas_viaje(tallas_viaje = tallas_viaje)
  tallas_viaje = filtro_tallas_viaje(tallas_viaje = tallas_viaje, marcas = marcas, nIndividuos = nIndividuos, tallaMin = tallaMin, tallaMax = tallaMax, nIndExtremo = nIndExtremo)

  return(tallas_viaje)


}

