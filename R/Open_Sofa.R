# ---------------------------------------------
#' Open the page .
#'
#' @aliases Open_Sofa.
#' @export
#' @param browser esse objeto é o nome do browser que o R deverá utilizar.
#' @param ... argumentos adicionais passados da função "remoteDriver".
#' @return Abre a página do sofa score.
#'
#' @example
#' \ donttest{
#' Open_Sofa(port = 4444L ,browser = "firefox")
#' }

Open_Sofa <- function(browser = "firefox", ...){
  url <- 'https://www.sofascore.com/tournament/football/brazil/brasileiro-serie-a/325'
  rd <- RSelenium::rsDriver(browser = browser, ...)
  remDr <- rd[["client"]]
  remDr$navigate(url)
  out <- list(remDr = remDr, rD = rd)
  class(out) <- "SofaScore"
  return(out)
}

