# ---------------------------------------------
#' Função para trazer os proximos jogos do time.
#'
#' @aliases Upcoming_Games.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @return retorna próximos jogos.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

## Entrar com o "team" (colocar)
Upcoming_Games <- function(server = NULL, team=NULL){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  url <- TabLink[TabLink[,1]== team,2]
  remDr$navigate(url)
  webElem1<- NULL
  repeat{
    webElem <- remDr$findElements(using = 'class',"dQuvSG") # selecionando tabela da web com os dados dos jogos
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()})) # selecionando texto na tabela
    webElem1 <- c(webElem1,webElem) # juntando texto de cada página em um único objeto
    webElemB <- remDr$findElements(using = 'xpath', "/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[3]/div/div/div/div/div[1]/div[2]/div") # selecionando botão "next"
    if(length(webElemB) == 0){
      break
    }
    else{
      webElemB <- webElemB[[1]]
      webElemB$clickElement()
    }
  }
  BD <- unlist(strsplit(webElem1, split = '\n'))
  BD <- BD[!BD=="SofaScore ratings"]
  v <- as.Date(BD,format = "%d/%m/%y")>Sys.Date()
  Prox <-  which(v %in% "TRUE")
  if ((length(Prox)-1)!= 0) {
    for (i in 1:(length(Prox)-1)) {
      BD <- c(BD[1:(Prox[i]+3+(i-1)*2)],"","",BD[(Prox[i]+4+(i-1)*2):length(BD)])
    }
  }
  Can <- which(BD %in% c("Canceled","Postponed"))
  Can <- Can[Can < Prox[1]]
  if (length(Can)!= 0) {
    for (i in 1:length(Can)) {
      BD <- c(BD[1:(Can[i]+i*2)],"","",BD[(Can[i]+i*2+1):length(BD)])
    }
  }
  BD <- c(BD,"","")
  j<-0
  i <- 0
  repeat {
    j<- j+1
    i <- 7*j+1
    if (i>length(BD)) {
      break
    }
    if (stringr::str_length(BD[i]) == 8) {
      BD <- c(BD[1:(i-1)],BD[i-7],BD[i:length(BD)])
    }
  }
  BD <- matrix(BD,ncol = 7,byrow =TRUE)
  BD <- data.frame(BD[order(as.Date(BD[,2],format = "%d/%m/%y"),decreasing = FALSE),])
  v <- as.Date(BD[,2],format = "%d/%m/%y")>Sys.Date()
  Prox <-  which(v %in% "TRUE")
  names(BD) <- c("Championship", "Date","Hora", "Home Team","Away Team","Home Team Goals","Away Team Goals")
  BD <- BD[Prox,c(-6,-7)]
  rownames(BD) <-  NULL # tirar a numeração que estava errada das linhas
  return(BD)
}

