#### Script 02. Criando a variaval manejo  ####

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)

#### 1. lendo os dados ####
## mapbiomas no entorno dos pontos
mb_buf <- raster("data/raster/mapbiomas_buffer100km")  # lendo apeas a ultima banda, a mais recente

## pontos mamiferos
pt_cam <- readOGR("data/shapefile/pontos_camtrap.shp")

## atlas IBGE
atlas <- readOGR("data/shapefile/AtlasIBGE/AND2018_ct_municipio2013_var130.shp")

head(atlas)

# checando os dados de agrotoxicos
table(atlas$var130_e) # 5 niveis

# tranformando em classes de 1 a 5
levels(atlas$var130_e) <- 1:5

table(atlas$var130_e) # 5 niveis

pt_atlas <- atlas[pt_cam,]

#### 2. extraindo os valores



