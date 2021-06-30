# ---------------------------------------------
#' Função para trazer os proximos jogos do time.
#'
#' @aliases Upcoming_Games.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @return retorna próximos jogos.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

## Entrar com o "team" (colocar)
Upcoming_Games <- function(server = NULL){
  if(class(server) != "Sofa_score") stop("Only Sofa_score objects are acceptable. To create
                                         a Sofa_score object see the Sofa_score() function!")
  remDr <- server$remDr
  url <- 'https://www.sofascore.com/team/football/palmeiras/1963'
  remDr$navigate(url)
  webElem1<- NULL
  repeat{
    webElem <- remDr$findElements(using = 'class',"dotAOs")
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))
    webElem1 <- c(webElem1,webElem)
    webElemB <- remDr$findElements(using = 'class',"epVTwK")
    if(length(webElemB) == 1){
      break
    }
    else{
      webElemB <- webElemB[[2]]
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
  BD <- data.frame(matrix(BD,ncol = 7,byrow =TRUE))
  v <- as.Date(BD[,2],format = "%d/%m/%y")>Sys.Date()
  Prox <-  which(v %in% "TRUE")
  names(BD) <- c("Championship", "Date","Hora", "Home Team","Away Team","Home Team Goals","Away Team Goals")
  BD <- BD[Prox,c(-6,-7)]
  return(BD)
}

