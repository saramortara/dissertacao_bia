#### Script 01. Criando os buffers com os valores do mapbiomas

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)

#### 1. Lendo os dados ####

## mapbiomas
mb <- stack("data/raster/MATAATLANTICA.tif")[[33]] # lendo apenas a ultima banda, a mais recente
dados_mb <- read.csv("data/classes_mapbiomas.csv", sep=";") 
## atlas
atlas <- readOGR("data/shapefile/AtlasIBGE/AND2018_ct_municipio2013_var130.shp")
## dados bioticos
camtrap <- read.csv("data/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv")
# selecionando apenas os camtrap com presenca e ausencia
camtrap <- camtrap[camtrap$PA=="y",]

# plotando os pontos no mapa
map('world','Brazil')
points(Y ~ X, data=camtrap)

# transformando os pontos em objeto espacial
pt_cam <- SpatialPointsDataFrame(coords = camtrap[,c("X","Y")], 
                                 data = camtrap,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

# exportando os dados em formato especial
writeOGR(pt_cam, ".", "./data/shapefile/pontos_camtrap/pontos_camtrap", 
         driver= "ESRI Shapefile", 
         overwrite_layer = TRUE)

#### 2. Criando o buffer #### 

# criando buffer no entorno dos pontos
buf_dist <- 100000
buf_cam <- buffer(pt_cam, width = buf_dist, dissolve = TRUE)

plot(Y ~ X, data=camtrap, pch=".", cex=1.5, las=1)
map('world','Brazil', add=TRUE)
plot(buf_cam, add=TRUE)

head(pt_cam)


#### 3. Extraindo valores do mapbiomas para os buffer
mb_cro <- crop(mb, buf_cam)
mb_crop <- mask(mb_cro, buf_cam)

# exporta o novo raster com o buffer de 100 km
writeRaster(mb_crop, filename = "data/raster/mapbiomas_buffer100km_raw",
            format = "GTiff", overwrite = T)

#### 4. Reduzindo as informações do mapbiomas ####
head(dados_mb)
mb_rcl <- as.matrix(dados_mb[,c("is", "becomes")])
mb_new <- reclassify(mb_crop, mb_rcl)

# exporta o novo raster mapbiomas
writeRaster(mb_new, filename = "data/raster/mapbiomas_buffer100km",
             format = "GTiff", overwrite = T)


