# ---------------------------------------------
#' Function for data of Campeonato Brassileiro .
#'
#' @aliases Brasileiro_A.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @param type esse objeto descreve se a tabla é geral, para os jogos em casa ou jogos fora de casa.
#' @param year esse objeto informa o ano que se deseja a tabela do campeonato.
#' @param ... argumentos adicionais passados para outros métodos.
#' @return Abre a página do sofa score.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

Brasileiro_A <- function(server = NULL,type=NULL,year=NULL, ...){
  if(class(server) != "Sofa_score") stop("Only Sofa_score objects are acceptable. To create
                                         a Sofa_score object see the Sofa_score() function!")
  url <- 'https://www.sofascore.com/tournament/football/brazil/brasileiro-serie-a/325'
  remDr <- server$remDr
  remDr$navigate(url)
  if(is.null(year)){year=lubridate::year(Sys.Date())}
  if(is.null(type)){type="OVERALL"}
  p <- lubridate::year(Sys.Date())-as.numeric(year)+1
  caminho <- paste("/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[1]/
                   div[2]/div/div[2]/div/div/ul/div/div[1]/li[",p,"]",sep="")
  # Selecionando o botão para selecionar o ano
  webElemB <- remDr$findElements(using = 'class',"iDNquT")
  webElemB <- webElemB[[1]]
  webElemB$clickElement()
  Sys.sleep(1)
  # Selecionando o ano de análise
  option <- remDr$findElement(using = 'xpath', caminho)
  option$clickElement()
  type<- toupper(type)
  Bot <- switch(type,
                OVERALL = remDr$findElements(using = 'class',"bfqsCw")[[1]],
                HOME = remDr$findElements(using = 'class',"bfqsCw")[[2]],
                AWAY = remDr$findElements(using = 'class',"bfqsCw")[[3]],
                { cat("option not recognised\n")})
  Bot$clickElement()
  Sys.sleep(1) # comando para esperar 1 segundo aqui
  webElem <- remDr$findElements(using = 'class',"hRIdgz")
  webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))[[1]]
  BD <- unlist(strsplit(webElem, split = '\n'))
  BD = BD[11:170]
  BD <- matrix(BD,ncol = 8,byrow =TRUE)
  Tabela <- as.data.frame(BD)
  names(Tabela) <- c("Classification","Team",unlist(strsplit(webElem, split = '\n'))[4:8],"Pts")
  return(Tabela)
}

