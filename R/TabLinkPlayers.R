# ---------------------------------------------
#' Função que retorna o elenco do time.
#'
#' @aliases TabLinkPlayers.
#' @export
#' @param server connection with the SofaScore website by the Sofa_score() function, this object should be of the class remoteDriver
#' @param team esse objeto descreve o time que deseja saber o elenco.
#' @return retorna elenco o time.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @example
#' \ donttest{
#'  team<-"Flamengo"
#'
#'
#' }


# Função que retorna o elenco do time
## Entrar com o "team" e "position"
TabLinkPlayers <- function(server = NULL,team=NULL){
  if(class(server) != "SofaScore") stop("Only SofaScore objects are acceptable. To create
                                         a SofaScore object see the Sofa_score() function!")
  remDr <- server$remDr
  url <- TabLink[TabLink[,1]==team,2]
  remDr$navigate(url)
  cont<- 0
  repeat{
    Sys.sleep(1)
    Sys.sleep(2)
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight*2/6);")
    Sys.sleep(2) # comando para esperar 1 segundo aqui
    webElemB <- remDr$findElements(using = 'class',"kJURRM")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    inic <- webElemB$getElementLocationInView()
    webElemB1 <- remDr$findElements(using = 'class',"dCSxyt")
    webElemB1 <- webElemB1[[length(webElemB1)]]
    inic <- webElemB1$getElementLocationInView()
    Local <- webElemB$getElementLocation()
    Sys.sleep(3) # comando para esperar 1 segundo aqui
    #Clicando no botão para os jogadores ficarm na lista
    webElemB <- remDr$findElements(using = 'class',"iCdnqS")
    webElemB <- webElemB[[length(webElemB)]]
    webElemB$clickElement()
    Sys.sleep(1) # comando para esperar 1 segundo aqui
    cont <- cont + 1
    if (cont==2) {
      break
    }
  }

  # Clicar no show more, tabela de jogadores
  webElemB <- remDr$findElements(using = 'class',"aeyKT")
  webElemB <- webElemB[[length(webElemB)]]
  webElemB$clickElement()

  # The method findElements() returns a list of Web Elements.
  # This code finds all links in page 1 by using the tag name
  all_links_page_1 <- remDr$findElements("tag name", "a")

  # The function below, when used in a webElement with the "a" tag name,
  # returns the link inner text and its url.
  # Both info are also printed on the console if print_output = TRUE

  show_links_info <- function(link_element, print_output = TRUE) {
    text <- as.character(link_element$getElementText())
    url <- as.character(link_element$getElementAttribute("href"))
    c(text, url)
  }



  # Use lapply to apply the show_links_info function to the list with
  # all the links in the first page
  saved_list <- lapply(all_links_page_1, show_links_info)

  # The code below saves all links from page 1 in a dataframe
  df <- suppressWarnings(as.character(matrix(unlist(saved_list), nrow=length(saved_list), byrow=TRUE),stringsAsFactors=FALSE))

  x<-matrix(Reduce(rbind, df),ncol = 2, byrow = FALSE)

  x<-x[stringr::str_which(x[,2], "player"),]
  x <- as.data.frame(x)
  #x <- as.data.frame(x) %>% dplyr::distinct(V2, .keep_all = TRUE)
  x <- dplyr::distinct(x,x$V2, .keep_all = TRUE)

  file.split <- strsplit(x[,1], "\n")
  n.obs <- sapply(file.split, length)
  seq.max <- seq_len(max(n.obs))

  file_split = data.frame(x[,c(2,1)], reshape2::colsplit(x[,1], pattern="\n", names = seq.max))

  BD_Link <- file_split[,c(4,1)]
  names(BD_Link) <- c("Player","Link")

  return(BD_Link)

  # http://estatidados.com.br/using-rselenium-for-task-automation-and-web-scraping/

}

