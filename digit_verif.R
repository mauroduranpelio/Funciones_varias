# determina digito verificador RUT:
## basado en algoritmo explicado aqui: https://es.wikipedia.org/wiki/Rol_Único_Tributario



digit_verif <- function(RUT){
  
  # valido que input RUT sea numerico y con formato: 1000000<=RUT<=99999999
  if( (!is.numeric(RUT)) | (RUT < 1000000) | (RUT > 99999999)) {
    stop("Rut ingresado debe ser un número entre 1.000.000 y 99.999.999")}
  
  # agregar comentario explicativo:
  largo_rut <- nchar(RUT)
  new_rut <- strsplit(as.character(RUT),"")[[1]] # separo digitos del input RUT en un char_vector
  new_rut <- as.numeric(new_rut[largo_rut:1]) # revierto orden de los digitos y convierto en num_vector
  
  suma_prod <- suppressWarnings(sum(new_rut*c(2:7))) # realizo suma_producto según algoritmo y suprimo mensaje warning
  digit <- 11 - suma_prod%%11
  
  if(digit == 11) {return(0)}
  else if(digit == 10) {return("k")}
  else{return(digit)}
}


