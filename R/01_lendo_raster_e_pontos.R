#### Script 01. Criando os buffers com os valores do mapbiomas

# carrega os pacotes necessarios
## pacotes que a gente usa mesmo!
library(raster) 
library(rgdal)
## so para plotar
library(maps) 
# para %>% 
library(dplyr)

#### 1. Lendo os dados ####

## mapbiomas download aqui: https://mapbiomas.org/downloads_colecoes -> clicar em MATA ATLANTICA
mb <- stack("data/raster/MATAATLANTICA.tif")[[33]] # lendo apenas a ultima banda, a mais recente
dados_mb <- read.csv("data/classes_mapbiomas.csv", sep=";") 
## dados bioticos
camtrap <- read.csv("data/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv")
# selecionando apenas os camtrap com presenca e ausencia
camtrap <- camtrap[camtrap$PA=="y",]
dim(camtrap)

# plotando os pontos no mapa
map('world','Brazil')
points(Y ~ X, data=camtrap)

head(camtrap)

# transformando os pontos em objeto espacial
pt_cam <- SpatialPointsDataFrame(coords = camtrap[,c("X","Y")], 
                                 data = camtrap,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

# exportando os dados em formato especial
#dir.create("data/shapefile/pontos_camtrap")
# writeOGR(pt_cam, ".", "./data/shapefile/pontos_camtrap/pontos_camtrap", 
#          driver= "ESRI Shapefile", 
#          overwrite_layer = TRUE)

#### 2. Criando o buffer #### 

# criando buffer no entorno dos pontos
# criando objetos com os valores de area
## valor maximo de area para grandes mamiferos  853.7
a1 <- 900 
r1 <- sqrt(a1/pi)*1000

buf_dist <- r1

# vamos criar um arquivo espacial para cada comunidade
buf_cam <- buffer(pt_cam, width = buf_dist, dissolve=FALSE)

buf_cam

plot(buf_cam)

# changing class to export as shapefile
buf_polydf <- as(buf_cam, "SpatialPolygonsDataFrame")
class(buf_polydf)

#dir.create("data/shapefile/buffer_camtrap")
#writeOGR(buf_polydf, ".", "./data/shapefile/buffer_camtrap/buffer_camtrap", 
#         driver= "ESRI Shapefile", 
#         overwrite_layer = TRUE)

#### 3. Extraindo valores do mapbiomas para os buffer

mb_cro <- crop(mb, buf_cam)
mb_crop <- mask(mb_cro, buf_cam)

# exporta o novo raster com o buffer de 900 km2
writeRaster(mb_crop, filename = "data/raster/mapbiomas_buffer900km2_raw",
            format = "GTiff", overwrite = T)

#### 4. Reduzindo as informações do mapbiomas ####
head(dados_mb)
mb_rcl <- as.matrix(dados_mb[,c("is", "becomes")])
mb_new <- reclassify(mb_crop, mb_rcl)

# exporta o novo raster mapbiomas com as classes simplificadas
writeRaster(mb_new, filename = "data/raster/mapbiomas_buffer900km2",
             format = "GTiff", overwrite = T)

