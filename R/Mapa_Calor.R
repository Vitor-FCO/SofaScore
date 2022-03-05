# ---------------------------------------------
#' Função que retorna o mapa de calor do jogador.
#'
#' @aliases Mapa_Calor.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class remoteDriver
#' @param player esse objeto descreve o jogador que deseja saber o mapa de calor em campo.
#' @param team esse objeto descreve o time que o jogador pertence atualmente.
#' @return retorna o mapa de calor do jogador.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }


# Função que retorna o elenco do time
## Entrar com o "team" e "position"
Mapa_Calor <- function(server = NULL,player = NULL, team=NULL){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  Tab <- TabLinkPlayers(server = server, team= team)
  url <- Tab[Tab[,1]==player,2]

  remDr$navigate(url)
  Sys.sleep(3)
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*2/6);")
  #url <- 'https://www.sofascore.com/pt/jogador/hulk/34705'
  # webElemB <- remDr$findElement("css","#onetrust-accept-btn-handler")
  # webElemB$clickElement()

  WE <- suppressMessages(try(remDr$findElement("css","#onetrust-accept-btn-handler"),silent = TRUE))
  if(suppressWarnings(class(WE) != "try-error")){
    webElemB <- remDr$findElement("css","#onetrust-accept-btn-handler")
    webElemB$clickElement()
  }


  webElem_img1 <- remDr$findElements(using = 'class',"ikigwV")
  webElem_img1_1 <- webElem_img1[[1]]
  inic <- webElem_img1_1$getElementLocationInView() # Coloca  elemento na tela e fala a localização dele
  Local <- webElem_img1_1$getElementLocation() # Fala a localização do elemento na páagina inteira
  remDr$screenshot(file = tf <- tempfile(fileext = ".png"))
  fig <- png::readPNG(tf)
  fig1 = fig[inic$y:(inic$y + Local$height),Local$x:(Local$x + Local$width),1:4]
  OpenImageR::imageShow(fig1)
  return(fig1)
}

