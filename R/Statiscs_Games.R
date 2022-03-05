# ---------------------------------------------
#' Função para trazer as estatísticas dos ultimos jogos do time.
#'
#' @aliases Statiscs_Games.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class "SofaScore".
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @param time tempo necessário para o sistema carregar páginas na web em segundos.
#' @return retorna últimos jogos.
#'
#' @example
#' \ donttest{
#'
#'
#'
#' }

Statiscs_Games<- function(server = NULL, team=NULL, time= 1){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  url <- TabLink[TabLink[,1]== team,2]
  remDr$navigate(url)
  Sys.sleep(time)
  BD1 <- NULL
  BD <- NULL
  for (i in 1:10) {

    # Nessa parte devo fazer um for para apertar cada jogo e ir para as estatísticas dos jogos
    #Jogo 1 a 8, 1 a 10 trocar no a[i]
    jogo <- paste("/html/body/div[1]/main/div/div[2]/div[1]/div[1]/
              div[3]/div/div/div[1]/div/div[2]/a[",i,"]/div/div",sep="")
    #jogo <- "/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[3]/div/div/div[1]/div/div[2]/a[1]/div/div"
    bot <- remDr$findElement(using = 'xpath', jogo)
    bot$clickElement()
    Sys.sleep(time*2.5)

    #esse botão seleciona as estatísticas
    estat <- "/html/body/div[1]/main/div/div[2]/div[1]/div[1]/div[3]/div/div/div[2]/div/div[1]/div/div[2]/div[1]/div/div/a[3]"
    bot_est <- remDr$findElement(using = 'xpath', estat)
    bot_est$clickElement()
    Sys.sleep(time)

    #aqui seleciona a tabela
    webElem <- remDr$findElements(using = 'class',"jdyPRF")
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))[[2]] #no 2 estão as estatisticas e no 1 estão os jogos
    BD <- unlist(strsplit(webElem, split = '\n'))
    teste <- sum(BD %in% "STATISTICS")
    if (teste != 0) {
    Camp <- BD[2]
    Jogo <- BD[3]
    BD <- BD[(which(BD %in% "Ball possession")-1) : length(BD)]

    #descendo  scrow todo
    webElem <- remDr$findElement("css", "#__next > main > div > div.Content__PageContainer-sc-g01dez-0.eyhylD > div.Grid-sc-1mmtejb-0.kJKput > div.Col-sc-ffl2sp-0.jWLzlJ > div:nth-child(3) > div > div > div.Col-sc-ffl2sp-0.jdyPRF.widget-wrapper > div > div.ps__rail-y > div")
    webElem$sendKeysToElement(list(key = "end"))
    webElem <- remDr$findElements(using = 'class',"jdyPRF")
    webElem <- unlist(lapply(webElem, function(x){x$getElementText()}))[[2]] #no 2 estão as estatisticas e no 1 estão os jogos
    BDFinal <- unlist(strsplit(webElem, split = '\n'))
    BDFinal <- BDFinal[(which(BDFinal %in% "Shots inside box")+2) : length(BDFinal)]
    #juntando o banco antes de descer o scroll com o depois
    BD <- c(BD,BDFinal)
    BD <- data.frame(matrix(BD,ncol = 3,byrow = T))
    BD <- cbind(Camp,Jogo,BD)
    Sys.sleep(0.5*time)

    BD1 <- rbind(BD1,as.matrix(BD))
    }
  }
  BD1 <- as.data.frame(BD1[,c(1:2,4,3,5)])
  names(BD1) <- c("Championship", "Game", "Statiscs", "Home Team","Away Team")
  return(BD1)
}


