# Descarga ----------------------------------------------------------------


tratamiento_descargas = function(data_descarga, ports = c("Callao","Malabrigo","Chimbote","Coishco", "Chancay", "Tambo de Mora","Paracas (Pisco)", "Samanco","Supe", "Bayovar","Carqu¡n","Vegueta")){

  names(data_descarga) = c("region","puerto","id_planta","fabrica","localidad","armador","emb","matricula","acta_des","F_ini_desembarque","F_fin_desembarque","act_descarga","F_ini_descarga","F_fin_descarga","cb","tm_declarada","descarga","n_report","parte_muestreo","n_juv","por_juv","moda","por_moda","id_faena")


  data_descarga = data_descarga %>% dplyr::filter(puerto %in% ports) %>% mutate(id_matricula = removeSpace_Letter(matricula), puerto = tolower(puerto)) %>% select(puerto, emb, id_matricula, F_ini_desembarque, F_fin_desembarque, F_ini_descarga, F_fin_descarga, cb, tm_declarada, descarga, id_faena)

  return(data_descarga)

}




