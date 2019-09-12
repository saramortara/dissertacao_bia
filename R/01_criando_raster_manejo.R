#### Script 01. Lendo os dados do mapbiomas e dos pontos ####

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)

# lendo os dados
camtrap <- read.csv("data/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv")

head(camtrap)
