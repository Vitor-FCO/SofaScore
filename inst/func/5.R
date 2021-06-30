







# Função que retorna o elenco do time
## Entrar com o "team" e "position"
Players <- function(team=NULL,position=NULL){
  BD1 <- NULL
  for (j in 1:length(Tab_Link[,2])){
    url <- Tab_Link[j,2]
    remDr$navigate(url)
    Sys.sleep(1.7)
    webElemB <- remDr$findElements(using = 'class',"bfqsCw")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    Sys.sleep(1) # comando para esperar 1 segundo aqui
    webElem <- remDr$findElements(using = 'class',"dtirhQ")
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))[3]
    webElem <- str_replace_all(webElem, "\n- ", "\n-\n")
    BD <- unlist(strsplit(webElem, split = '\n'))
    BD <- BD[-1]
    BD <- data.frame(matrix(BD,ncol = 4,byrow = T))
    v <- NULL
    for (i in 1:nrow(BD)) {
      if (is.na(suppressWarnings(as.numeric(str_sub(BD[i,1], end = 2))))) {
        if (is.na(suppressWarnings(as.numeric(str_sub(BD[i,1], end = 1))))) {v[i]<-0} else {v[i]<-1}
      }else{v[i]<-2}
    }
    V0 <- as.numeric(str_sub(BD$X1, end = v))
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