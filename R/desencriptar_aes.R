#' @title Desencripta en AES CBC
#'
#' @description Desencripta en AES CBC un dato encriptado y en formato base64. La clave y el vi deben de ir en formato string sin espacios
#'
#' @param dato, modo, vi, clave
#'
#' @return json
#'
#' @examples  desencriptar_aes("ANOZ8RrW8zGvcqKEXJEe8A==", "CBC", "47,148,14,86,131,53,108,190,252,83,71,46,69,201,12,119" , "01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10")
#'
#' @import digest
#' caTools
#' dplyr
#'
#' @export

desencriptar_aes <- function(dato, modo = "CBC", vi, clave){

  # Declaración variables
  dato_encriptado <- base64decode(z = dato, what = "raw")
  modo <- c(modo)
  vi <- vi %>% strsplit(.,",") %>% unlist() %>% as.integer()
  clave <- clave %>% strsplit(.,",") %>% unlist() %>% as.hexmode() %>% as.raw()

  # Creación de objeto
  aes <- AES(clave, mode = modo, IV=vi)
  dato_desencriptado <- aes$decrypt(dato_encriptado, raw = TRUE)

  return(dato_desencriptado)
}
