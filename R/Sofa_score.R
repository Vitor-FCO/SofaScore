# ---------------------------------------------
#' Open the page .
#'
#' @aliases Sofa_score.
#' @export
#' @param port esse objeto precisa ser um código da porta que você utiliza pra abrir o navegador.
#' @param chromever esse objeto é o código atualizado do crome que utiliza.
#' @param ... argumentos adicionais passados para outros métodos.
#' @return Abre a página do sofa score.
#'
#' @example
#' \ donttest{
#' Sofa_score(port = 4444L ,chromever = "87.0.4280.88")
#' }

Sofa_score <- function(port = NULL ,chromever = NULL, ...){
  url <- 'https://www.sofascore.com/tournament/football/brazil/brasileiro-serie-a/325'
  rD <- suppressWarnings(try(RSelenium::rsDriver(port = 4444L ,chromever = "87.0.4280.88")))
  remDr <-  try(RSelenium::remoteDriver$new())
  remDr$open()
  remDr$navigate(url)
  return(remDr)
}

