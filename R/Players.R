# ---------------------------------------------
#' Função que retorna o elenco do time.
#'
#' @aliases Players.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class remoteDriver
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @param position esse objeto informa a posição dos jogadores que deseja extrair informações.
#' @param time tempo necessário para o sistema carregar páginas na web em segundos.
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
Players <- function(server = NULL,team=NULL,position=NULL, time = 1){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  BD1 <- NULL
  for (j in 1:length(TabLink[,2])){
    url <- TabLink[j,2]
    remDr$navigate(url)
    cont<- 0
    #repeat{
    # webElem_img1 <- remDr$findElements(using = 'class',"hCmVGz")
    # webElem_img1_1 <- webElem_img1[[1]]
    # inic <- webElem_img1_1$getElementLocationInView()
    # remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    # remDr$navigate(url)
     remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*2/6);")
    # remDr$navigate(url)
    Sys.sleep(2*time)
    # Mexendo na pagina para ela carregar inteira
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*2/6);")
    Sys.sleep(2*time) # comando para esperar
    # Comandos para clicarna tabela
    webElemB <- remDr$findElements(using = 'class',"kJURRM")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    inic <- webElemB$getElementLocationInView()
    webElemB1 <- remDr$findElements(using = 'class',"dCSxyt") #Encontra onde está o "Outros", só pq está do lado da tabela que queremos.
    webElemB1 <- webElemB1[[length(webElemB1)]]
    #inic <- webElemB1$getElementLocationInView() # coloca ele na pagina
    #Local <- webElemB$getElementLocation() # salva a localização do elemento
    Sys.sleep(3*time) # comando para esperar
    #Clicando no botão para os jogadores ficarm na lista
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*4/12);")
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*3/12);")
    webElemB <- remDr$findElements(using = 'xpath', "/html/body/div[1]/main/div/div[2]/div[1]/div[2]/div/div[3]/div[2]/div[3]/div")[[1]] # selecionando botão "mostre mais"
    webElemB$clickElement()
    Sys.sleep(2*time)



      webElemB <- remDr$findElements(using = 'class',"iCdnqS")
      webElemB <- webElemB[[length(webElemB)]]
      webElemB$clickElement()
      Sys.sleep(1.5*time) # comando para esperar
      webElemB <- remDr$findElements(using = 'class',"iCdnqS")
      webElemB <- webElemB[[length(webElemB)]]
      webElemB$clickElement()
      # print(inic) # Printa a localização do "outros"
      #cont <- cont + 1
      # if (cont==2) { #Faz oo repeat inteiro por duas vezes
      #   break
      # }
    #}
    # Pegando a lista de jogadores "Tab__Wrapper"
    webElem <- remDr$findElements(using = 'class',"hIyudd")
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
