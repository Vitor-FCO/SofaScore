# ---------------------------------------------
#' Função que retorna o elenco do time.
#'
#' @aliases Players.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class remoteDriver
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @param position esse objeto informa a posição dos jogadores que deseja extrair informações.
#' @return retorna elenco o time.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }


# Função que retorna o elenco do time
## Entrar com o "team" e "position"
Players <- function(server = NULL,team=NULL,position=NULL){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  BD1 <- NULL
  for (j in 1:length(TabLink[,2])){
    url <- TabLink[j,2]
    remDr$navigate(url)
    Sys.sleep(5)
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    remDr$navigate(url)
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*2/6);")
    Sys.sleep(2)
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(1) # comando para esperar 1 segundo aqui
    #Clicando no botão para os jogadores ficarm na lista
    webElemB <- remDr$findElements(using = 'class',"bfqsCw")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    Sys.sleep(1) # comando para esperar 1 segundo aqui
    webElem <- remDr$findElements(using = 'class',"dtirhQ")
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))[[length(webElem)]]
    webElem <- stringr::str_replace_all(webElem, "\n- ", "\n-\n")
    BD <- unlist(strsplit(webElem, split = '\n'))
    BD <- BD[-1]
    BD <- data.frame(matrix(BD,ncol = 4,byrow = T))
    v <- NULL
    for (i in 1:nrow(BD)) {
      if (is.na(suppressWarnings(as.numeric(stringr::str_sub(BD[i,1], end = 2))))) {
        if (is.na(suppressWarnings(as.numeric(stringr::str_sub(BD[i,1], end = 1))))) {v[i]<-0} else {v[i]<-1}
      }else{v[i]<-2}
    }
    V0 <- as.numeric(stringr::str_sub(BD$X1, end = v))
    V1 <- stringr::str_sub(BD$X1, start =  v+1)
    BD <- data.frame(V0, V1,BD[,c(2,3,4)])
    BD <- data.frame(TabLink[j,1],BD)
    BD1 <- rbind(BD1,as.matrix(BD))
  }
  BD1 <- as.data.frame(BD1)
  names(BD1) <- c("Team","Number","Player","Position","Age","Nationality")
  if(!is.null(team)){BD1 <- BD1[BD1$Team==team,]}
  if(!is.null(position)){BD1 <- BD1[BD1$Position==position,]}
  return(BD1)
}
