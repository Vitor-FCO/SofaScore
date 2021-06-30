# ---------------------------------------------
#' Close the page .
#'
#' @aliases Close_Sofa.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @param ... argumentos adicionais passados para outros métodos.
#' @return Close the sofa score web page.
#'
#' @example
#' \ donttest{
#' X <- Sofa_score()
#' Close_Sofa(remDr = X)
#' }

Close_Sofa <- function(server = NULL, ...){
  if(class(server) != "SofaScore") stop("Only Sofa_score objects are acceptable. To create
                                         a Sofa_score object see the Sofa_score() function!")
  remDr <- server$remDr
  remDr$close()
}
