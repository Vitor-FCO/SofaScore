

usethis::use_build_ignore("inst/script_development.R")
usethis::use_build_ignore("inst/script_testes_template.R")
usethis::use_build_ignore("inst/script_testes_classes.R")
usethis::use_build_ignore("inst/Leisch-CreatingPackages.pdf")


usethis::use_git_ignore("inst/script_development.R")
usethis::use_git_ignore("inst/script_testes_template.R")
usethis::use_git_ignore("inst/script_testes_classes.R")
usethis::use_git_ignore("inst/Leisch-CreatingPackages.pdf")


#usethis::use_travis()

#-------------------------------------------------------------------------------
# rodar essa parte só uma vez...
# criando um banco de dados para testes:

# set.seed(1234567890)
# n <- 50
# sigma <- 2
# beta0 <- 50
# beta1 <- -5.5
# x <- seq(-1, 1, length.out = n)
# y <- beta0 + beta1*x + rnorm(n, sd = sigma)
# dados <- data.frame(x = x, y = y)
#
# # adicionando ao pacote o banco de dados criado:
# usethis::use_data(dados)

#-------------------------------------------------------------------------------
# construindo o pacote:

devtools::document()
devtools::install()
devtools::load_all()


# devtools::build_manual()
# devtools::build()


#-------------------------------------------------------------------------------
# verificando o pacote:
require("RSelenium")
require("stringr")
require("lubridate")
require("SofaScore")


#rm(list=ls(all=T))
devtools::check()
devtools::test()

devtools::check_win_devel()
devtools::check_win_release()
devtools::check_rhub(email = "meuemail")
devtools::release_checks()
devtools::spell_check()

devtools::submit_cran()

# depois de subir para o CRAN:
usethis::use_github_release()

# # …or push an existing repository from the command line
# git remote add origin https://github.com/Vitor-FCO/R_avancado_trab_final.git
# git branch -M master
# git push -u origin master
#
# git config --global user.email "vitor.fariaco@gmail.com"
# git config --global user.name "Vitor-FCO"
#
#
# # para adicionar tudo
# git add .
#
# # para comitar
# git commit -m "submeter ao github"
#
# # para subir:
# git push



______________________
Teste
Brasileiro_A(type = "home")

V <-Last_Games()
View(V)

V<-Upcoming_Games()
View(V)

x <- Players(team = "Atl. Mineiro",position = "Midfielder")

Top_Players(2020)

____________
Last_Games <- funciont(connec = NULL){
   if(is.null(connec)) stop("Please before running any function is necessary to create a connection with the SofaScore website by the Sofa_score() function!")
   if(class(connec) != "remoteDriver") stop("The connection package should be of the class remoteDriver")
}

