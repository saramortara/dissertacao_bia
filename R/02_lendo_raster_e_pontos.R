#### Script 01. Lendo os dados do mapbiomas e dos pontos ####

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)

# lendo os dados
camtrap <- read.csv("data/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv")
mb <- stack("data/raster/MATAATLANTICA.tif")[[33]] # lendo apeas a ultima banda, a mais recente

# selecionando apenas os camtrap com presenca e ausencia
camtrap <- camtrap[camtrap$PA=="y",]

# plotando os pontos no mapa
map('world','Brazil')
points(Y ~ X, data=camtrap)

summary(camtrap)

# transformando os pontos em objeto espacial
cam_pt <- SpatialPointsDataFrame(coords = camtrap[,c("X","Y")], 
                                 data = camtrap,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))


# criando buffer no entorno dos pontos
buf_dist <- 20000
cam_buffer <- buffer(cam_pt, width = buf_dist, dissolve = TRUE)

plot(cam_buffer)

# cortando o mapbiomas para o buffer


