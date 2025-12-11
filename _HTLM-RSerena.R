#________________________________
#RSERENA
#lancement des  scripts Rmd
#version 2.6.1
#________________________________
#comptage classique Reserve----
library(rmarkdown)
library(rstudioapi)
current_path<- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
#pages html
  render("accueil.Rmd", "all", encoding="UTF-8") 
  render("ornitho-bilan.Rmd", "all", encoding="UTF-8") 
  render("ornitho-limicoles.Rmd", "all", encoding="UTF-8") 
  render("ornitho-anatides.Rmd", "all", encoding="UTF-8")
  render("ornitho-autres.Rmd", "all", encoding="UTF-8")
  render("ornitho-SitesObs.Rmd", "all", encoding="UTF-8")
  render("ornitho-tableau&data.Rmd", "all", encoding="UTF-8")
#fiche pdf 
  render("ornitho-BilanComptagePDF.Rmd", "all", encoding="UTF-8")
#fiche bilan comptage pour observatoire et MB
  source("ornitho-VisuelBilanComptage.R", encoding="UTF-8")
#tableau excel limicoles cotiers
  source("ornitho-opnl-limicole.R", encoding="UTF-8")
#________________________________ OK
  
  
#________________________________
## ajout pour comptage wetland----
  render("ornitho-Wetlands.Rmd", "all", encoding="UTF-8")
#________________________________
## ajout pour comptage larides----
  render("ornitho-larides.Rmd", "all", encoding="UTF-8") 
#________________________________
##si modif de la page doc----
  render("ornitho-doc.Rmd", "all", encoding="UTF-8") 
#________________________________
##bagues----
  render("LectureBagues.Rmd", "all", encoding="UTF-8")
#________________________________
  
  
#________________________________
# Température ----
  library(rmarkdown)
  library(rstudioapi)
  current_path<- rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
#temperature
  render("tempSuface.Rmd", "all", encoding="UTF-8")
#________________________________
  
  
#________________________________
# inventaire ----
  library(rmarkdown)
  library(rstudioapi)
  current_path<- rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
#MAJ des inventaires
  render("accueil.Rmd", "all", encoding="UTF-8") 
  render("inventaire.Rmd", "all", encoding="UTF-8")
  render("inventaireLC.Rmd", "all", encoding="UTF-8")
#si modif benthos
  render("Benthos-inventaires.Rmd", "all", encoding="UTF-8") 
#________________________________

