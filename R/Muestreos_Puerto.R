# Pre Tratamiento de tallas -----------------------------------------------

pre_trata_tallas_viaje = function(tallas_viaje, marcas  = seq(5,20,0.5)){

  names(tallas_viaje) = c("region","puerto","emb","matricula","fecha_descarga","descarga","id_faena",marcas,"moda","obs")

  tallas_viaje = tallas_viaje %>% mutate(id_matricula = removeSpace_Letter(matricula), puerto = tolower(puerto)) %>% select(region, id_matricula, descarga, id_faena, as.character(marcas))

  tallas_viaje = obtener_solo_muestra(tallas_viaje)

  return(tallas_viaje)

}


