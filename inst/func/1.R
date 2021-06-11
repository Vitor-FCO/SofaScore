# ---------------------------------------------
#' Close the page .
#'
#' @aliases Close_Sofa.
#' @export
#' @return Fecha a página do sofa score.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }



# Criando função para fechar a pagina do sofa score
Close_Sofa <- function(remDr = NULL){
  if(is.null(remDr)) stop("Please before running any function is necessary to create a connection with the SofaScore website by the Sofa_score() function!")
  if(class(remDr) != "remoteDriver") stop("The connection package should be of the class remoteDriver")
  remDr$close()
}
