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
  webElem <- remDr$findElements(using = 'class',"dotAOs")
  webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))
  BD <- unlist(strsplit(webElem, split = '\n'))
  BD <- BD[!BD=="SofaScore ratings"]
  Can <- which(BD %in% c("Canceled","Postponed"))
  t<-length(BD)+length(Can)*2
  if (length(Can)!= 0) {
    for (i in 1:length(Can)) {
      BD <- c(BD[1:(Can[i]+i*2)],"","",BD[(Can[i]+i*2+1):length(BD)])
    }
  }
  j<-0
  i <- 0
  while ( i <= length(BD)) {
    j<- j+1
    i <- 7*j+1
    if (stringr::str_length(BD[i]) == 8) {
      BD <- c(BD[1:(i-1)],BD[i-7],BD[i:length(BD)])
    }
    if (stringr::str_length(BD[i+1]) != 8|as.Date(BD[i+1],format = "%d/%m/%y")>Sys.Date()) {
      BD <- BD[1:(i-1)]
      break
    }
  }
  BD <- data.frame(matrix(BD,ncol = 7,byrow =TRUE))
  names(BD) <- c("Championship", "Date","Game Situation", "Home Team","Away Team","Home Team Goals","Away Team Goals")
  return(BD)
}
