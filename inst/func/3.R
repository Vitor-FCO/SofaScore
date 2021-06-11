








# Função para trazer os ultimos jogos do time
## Entrar com o "team" (colocar)
#remDr$open()
Last_Games <- function(){
  url <- 'https://www.sofascore.com/team/football/palmeiras/1963'
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
    if (str_length(BD[i]) == 8) {
      BD <- c(BD[1:(i-1)],BD[i-7],BD[i:length(BD)])
    }
    if (str_length(BD[i+1]) != 8|as.Date(BD[i+1],format = "%d/%m/%y")>Sys.Date()) {
      BD <- BD[1:(i-1)]
      break
    }
  }
  BD <- data.frame(matrix(BD,ncol = 7,byrow =TRUE))
  names(BD) <- c("Championship", "Date","Game Situation", "Home Team","Away Team","Home Team Goals","Away Team Goals")
  return(BD)
}
