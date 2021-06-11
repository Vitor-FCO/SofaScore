








# Função que retorna os melhores jogadores do Brasileirão
## Entrar com o "year"; "statistic"; "team"
Top_Players <- function(year=NULL,stat){
  url <- 'https://www.sofascore.com/tournament/football/brazil/brasileiro-serie-a/325'
  remDr$navigate(url)
  # Criando o caminho do botão do ano
  if(is.null(year)){year=year(Sys.Date())}
  p <- year(Sys.Date())-as.numeric(year)+1
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
  # Selecionar o segundo em uma lista de seleção
  Sys.sleep(1)
  webElem <- remDr$findElements(using = 'class',"gocrVi")
  webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))
  p <- which(substr(webElem, start = 1, stop = 11) %in% "TOP PLAYERS")
  webElem <- webElem[p]
  BD <- unlist(strsplit(webElem, split = '\n'))
  BD <- BD[-(length(BD))]
  BD <- data.frame(matrix(BD,ncol = 4,byrow = T))
  BD <- BD[-1,]
  names(BD) <- c("Classification","Top Players","Position","Rating")
  return(BD)
}
