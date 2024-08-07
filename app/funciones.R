porcentaje <- function(x, decimales = 1) {
  numero <- round(x*100, decimales)
  numero_format <- format(numero, big.mark = ".", decimal.mark = ",")
  paste0(numero_format, "%")
}