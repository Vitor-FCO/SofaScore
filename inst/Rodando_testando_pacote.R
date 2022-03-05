#install.packages(SofaScore)
devtools::document()
devtools::install()
devtools::load_all()
#library(SofaScore)
require("SofaScore")

TabLink
server <- Open_Sofa(port= 4445L, browser= "firefox")
Brasileiro_A(server= server, type= "Overall", year= 2020)
Last_Games(server= server, team= "Atl. Mineiro")
Upcoming_Games(server= server, team= " Atlético MG")
Players(server= server, team= "Atl. Mineiro", position= "Forward")
Close_Sofa(server= server)
