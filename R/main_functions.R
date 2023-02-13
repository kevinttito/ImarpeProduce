# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



# Uniendo calas a tallas calas --------------------------------------------


merge_callas_tallas = function(calas, tallas_calas){

  calas_limpias = tratar_calas(calas = calas)

  tallas_calas_limpias = tratar_tallas_calas(tallas_calas = tallas_calas)

  tallas_lance = merge(calas_limpias, tallas_calas_limpias, by = c("id_faena","n_cala","description"), all = TRUE)

  return(tallas_lance)

}








