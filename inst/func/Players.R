# ---------------------------------------------
#' Função que retorna o elenco do time.
#'
#' @aliases Upcoming_Games.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class remoteDriver
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @param position esse objeto informa a posição dos jogadores que deseja extrair informações.
#' @param ... argumentos adicionais passados para outros métodos.
#' @return retorna elenco o time.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

Tab_Link <- readRDS("data/Tab_Link.rds")
# Função que retorna o elenco do time
## Entrar com o "team" e "position"
Players <- function(server = NULL,team=NULL,position=NULL){
  if(class(server) != "Sofa_score") stop("Only Sofa_score objects are acceptable. To create
                                         a Sofa_score object see the Sofa_score() function!")
  remDr <- server$remDr
  BD1 <- NULL
  for (j in 1:length(Tab_Link[,2])){
    url <- Tab_Link[j,2]
    remDr$navigate(url)
    Sys.sleep(5)
    webElemB <- remDr$findElements(using = 'class',"iDNquT")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()



    webElemB <- remDr$findElements(using = 'class',"bfqsCw")
    webElemB <- remDr$findElements(using = 'class',"iFonsX")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    Sys.sleep(2) # comando para esperar 1 segundo aqui
    # webElemA <- remDr$findElement(using = 'xpath',"/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[5]/div[2]")
    # webElemA$clickElement()
    webElemB <- remDr$findElement(using = 'xpath',"/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[5]/div[2]/div[1]/a[2]")
    #webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement() # clicar no List View do site
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
    V1 <- str_sub(BD$X1, start =  v+1)
    BD <- data.frame(V0, V1,BD[,c(2,3,4)])
    BD <- data.frame(Tab_Link[j,1],BD)
    BD1 <- rbind(BD1,as.matrix(BD))
  }
  BD1 <- as.data.frame(BD1)
  names(BD1) <- c("Team","Number","Player","Position","Age","Nationality")
  if(!is.null(team)){BD1 <- BD1[BD1$Team==team,]}
  if(!is.null(position)){BD1 <- BD1[BD1$Position==position,]}
  return(BD1)
}
