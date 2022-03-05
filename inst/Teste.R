### Favor baixar esses pacotes para reproduzir os resultados.
require(readxl) # Baixar Dados
require(tidyverse) # Manusear Banco de Dados(Tiblle e pipe)
require(ggplot2) # Criar Gráficos
require(ggthemes) # Temas de Gráficos
require(hms) # Mexer com horas
require(lubridate) # Mexer com datas
require(reshape2) # Mudar formato dos dados, longo para largo
require(EnvStats) # Calcular Desvio padrão geométrico
require(gtable) # Trabalhar com tabelas
require(gt) # Trabalhar com tabelas

library(magrittr) # usar o pipe
library(magick) #abrir imagem no R

library(SofaScore)

server <- Open_Sofa(port=4545L, browser = "firefox")
Close_Sofa(server= server)
# Para fechaar tudo
rd$
remDr$open()
Tab <- Brasileiro_A(server = server,type="Overall",year=2021)
Tab <- Last_Games(server = server, team="Atl. Mineiro")
Tab <- Upcoming_Games(server = server, team="Atl. Mineiro")
Tab <- Players(server = server,team="Atl. Mineiro",position="Forward", time=2)
Tab <- TabLinkPlayers(server = server, team="Fluminense")
M <- Mapa_Calor(server = server, player = "Manoel" ,team="Fluminense")
Tab <- Statiscs_Games(server = server, team="Flamengo", time=1)

server$rD

Team <- c("Atlético MG", "Palmeiras", "Fortaleza", "RB Bragantino", "Flamengo", "Corinthians", "Atlético GO", "Ceará", "Athletico PR",  "Internacional", "Santos", "São Paulo", "Juventude","Cuiabá", "Bahia", "Fluminense", "Grêmio", "Sport Recife", "América MG", "Chapecoense")
Link <- c("atletico-mineiro/1977", "palmeiras/1963", "fortaleza/2020", "red-bull-bragantino/1999", "flamengo/5981", "corinthians/1957",
          "atletico-goianiense/7314", "ceara-sc/2001", "athletico-paranaense/1967",  "internacional/1966", "santos/1968", "sao-paulo/1981",
          "juventude/1980","cuiaba/49202", "bahia/1955", "fluminense/1961", "gremio/5926", "sport-recife/1959", "america-mineiro/1973", "chapecoense/21845")
Link_Comp <- paste("https://www.sofascore.com/team/football/",Link,sep = "")
Tab_Link <- data.frame(Team,Link_Comp)
names(Tab_Link) <- c("Team","Link")

Tab %>%
gt() %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(table.width = px(400)) %>%
  # Trabalhando no título da tabela
  tab_header(title = "Tabela: Estatística descritiva",
             subtitle = "") %>%
  tab_style(
    style = list(
      cell_text(align = "right")
    ),
    locations = cells_stub(rows = FALSE)
  ) %>%
  tab_options(#Alinhamento
    heading.align = "center",
    #Cor
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    heading.background.color = "#4472C4",
    column_labels.background.color = "#4472C4",
    row_group.background.color = "#8EA9DB",
    table.background.color =  "#D9E1F2",
    #tamanho
    heading.subtitle.font.size = 14,
    column_labels.font.size = 14,
    table.font.size = 13,
  ) %>%
  cols_align(align = "center") %>%
  tab_spanner_delim(delim="_")

Tab %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  tab_options(table.width = px(400)) %>%
  # Trabalhando no título da tabela
  tab_header(title = "Tabela: Estatística descritiva",
             subtitle = "") %>%
  tab_style(
    style = list(
      cell_text(align = "right")
    ),
    locations = cells_stub(rows = FALSE)
  ) %>%
  tab_options(#Alinhamento
    heading.align = "center",
    #Cor
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    heading.background.color = "#4472C4",
    column_labels.background.color = "#4472C4",
    row_group.background.color = "#8EA9DB",
    table.background.color =  "#D9E1F2",
    #tamanho
    heading.subtitle.font.size = 14,
    column_labels.font.size = 14,
    table.font.size = 13,
  ) %>%
  cols_align(align = "center") %>%
  fmt_number(columns=c(2:15),decimals = 2,
             sep_mark = ".",dec_mark = ",") %>%
  tab_spanner_delim(delim="_")
