# ---------------------------------------------
#' Função para trazer os ultimos jogos do time.
#'
#' @aliases Last_Games.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @return retorna últimos jogos.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

Last_Games <- function(server = NULL, team=NULL){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  url <- TabLink[TabLink[,1]== team,2]
  remDr$navigate(url)
  webElem1 <- NULL
  Cont <- 0
  repeat{
    ## Quando precisar atualizar é proximo comando, atualizar nome da class
    webElem <- remDr$findElements(using = 'class',"dQuvSG") # selecionando tabela da web com os dados dos jogos
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()})) # selecionando texto na tabela
    #if(length(which(as.Date(webElem,format = "%d/%m/%y")>Sys.Date()))>0){webElem <- webElem[1:which(as.Date(webElem,format = "%d/%m/%y")>Sys.Date())[1]]}
    # Preciso colocar tres espaços depois da data maior que hoje
    webElem1 <- c(webElem1,webElem) # juntando texto de cada página em um único objeto
    webElemB <- remDr$findElements(using = 'xpath', "/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[3]/div/div/div/div/div[1]/div[1]/div") # selecionando botão "next"
    Cont <- Cont+1
    if(length(webElemB) == 0| Cont == 23){
      break
    }
    else{
      webElemB <- webElemB[[1]]
      webElemB$clickElement()
    }
  }
  BD <- unlist(strsplit(webElem1, split = '\n'))
  BD <- BD[!BD=="SofaScore ratings"]
  Can <- c(which(BD %in% c("Canceled","Postponed")), which(as.Date(BD,format = "%d/%m/%y")>Sys.Date()))
  Can <- Can[order(Can)]
  t<-length(BD)+length(Can)*2
  # Colocando o numero certo de colunas em todos jogos (cancelados e proximosjogos)
  if (length(Can)!= 0) {
    for (i in 1:length(Can)) {
      BD <- c(BD[1:(Can[i]+i*2)],"","",BD[(Can[i]+i*2+1):length(BD)])
    }
  }
  j<-0
  i <- 0
  # acrescentar
  while ( i <= length(BD)) {
    j<- j+1
    i <- 7*j+1
    if (is.na(BD[i])) {break()}
    if (stringr::str_length(BD[i]) == 8) { # testa se é data a posição multipla de 8
      BD <- c(BD[1:(i-1)],BD[i-7],BD[i:length(BD)])
    }
  }
  BD <- matrix(BD,ncol = 7,byrow =TRUE)
  BD <- BD[order(as.Date(BD[,2],format = "%d/%m/%y"),decreasing = TRUE),]
  BD <- as.data.frame(BD[as.Date(BD[,2],format = "%d/%m/%y")<=Sys.Date(),]) # Tirar os dados dos proximos jogos
  names(BD) <- c("Championship", "Date","Game Situation", "Home Team","Away Team","Home Team Goals","Away Team Goals")
  return(BD)
}
