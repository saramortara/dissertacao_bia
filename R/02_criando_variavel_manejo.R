#### Script 02. Criando a variaval manejo  ####
# extrai o valor de manejo para cara localidade, extrai % mata, % agricultura do mapbiomas

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)

#### 1. lendo os dados ####
## mapbiomas no entorno dos pontos - area 900 km2
mb_buf <- raster("data/raster/mapbiomas_buffer900km2")

## pontos mamiferos
pt_cam <- readOGR("data/shapefile/pontos_camtrap/pontos_camtrap.shp")

## atlas IBGE
atlas <- readOGR("data/shapefile/AtlasIBGE/AND2018_ct_municipio2013_var130.shp", 
                 #use_iconv = TRUE, 
                 encoding = "UTF-8")

head(atlas)

# checando os dados de agrotoxicos
table(atlas$var130_e) # 5 niveis

# tranformando em classes de 1 a 5
levels(atlas$var130_e) <- 1:5

table(atlas$var130_e) # 5 niveis

atlas

head(atlas)

proj4string(atlas)
proj4string(pt_cam)

atlas <- spTransform(atlas, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# agora com a mesma projecao
proj4string(atlas)
proj4string(pt_cam)

cam_over <- over(pt_cam, atlas)

cam_over$loctn_d <- pt_cam$loctn_d

cam_over

#### 2. fazendo o merge das localidads com a info de agrotoxico ####

cam_atlas <- merge(pt_cam, cam_over, by="loctn_d")
dim(cam_atlas)

head(cam_atlas)

cam_atlas_df <- cam_atlas@data

areas_excluir <- data.frame(loctn_d=cam_atlas_df$loctn_d[is.na(cam_atlas_df$var130_e)])

write.table(areas_excluir, "results/02_areas_excluir.csv", 
            col.names = TRUE, row.names = FALSE, sep=",")

write.table(cam_atlas_df, "results/02_dados_agrotoxicos.csv", 
            col.names = TRUE, row.names = FALSE, sep=",")

##### 3. extraindo os valores do mapbiomas ####
# % mata
# % agricultura
# % outros


