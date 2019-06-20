# determina digito verificador RUT:
## basado en algoritmo explicado aqui: https://es.wikipedia.org/wiki/Rol_Único_Tributario



digit_verif <- function(RUT){
  
  # falta validar que input RUT sea numerico según formato rut (1000000<=RUT<=99999999)
  #     if(regla RUT no se cumple) {stop("Rut ingresado no válido")} 
  if( (!is.numeric(RUT)) | (RUT < 1000000) | (RUT > 99999999)) {
    stop("Rut ingresado debe ser un número entre 1.000.000 y 99.999.999")}
  
  # agregar comentario explicativo:
  largo_rut <- nchar(RUT)
  new_rut <- strsplit(as.character(RUT),"")[[1]]
  new_rut <- as.numeric(new_rut[largo_rut:1])
  
  suma_prod <- sum(new_rut*c(2:7)) # ver cómo evitar mensaje warning
  digit <- 11 - suma_prod%%11
  
  if(digit == 11) {return(0)}
  else if(digit == 10) {return("k")}
  else{return(digit)}
}


