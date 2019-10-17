#### Script 02. Criando a variaval manejo  ####
# extrai o valor de manejo para cara localidade, extrai % mata, % agricultura do mapbiomas

# carrega os pacotes necessarios
library(raster)
library(rgdal)
library(maps)
library(dplyr)

#### 1. lendo os dados ####
## mapbiomas no entorno dos pontos - area 900 km2
mb_buf <- raster("data/raster/mapbiomas_buffer900km2.tif")

mb_buf

plot(mb_buf)

## pontos mamiferos
pt_cam <- readOGR("data/shapefile/pontos_camtrap/pontos_camtrap.shp")

## buffer pontos
pt_buf <- readOGR("data/shapefile/buffer_camtrap/buffer_camtrap.shp")

# identificando as sobreposicoes
lista <- list()
for(i in 1:nrow(pt_buf)){
lista[[i]] <- over(pt_buf[i,],pt_buf[-i,])  
}

# vou adicionar uma coluna indicando qual area sobrepoe com qual area
sapply(lista, function(x) x[,1])
pt_cam$sobrepoe_com <- sapply(lista, function(x) x[,1])

head(pt_cam)

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

atlas <- spTransform(atlas, 
                     CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

pt_cam <- spTransform(pt_cam, 
                     CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# agora com a mesma projecao
proj4string(atlas)
proj4string(pt_cam)

cam_over <- over(pt_cam, atlas)

cam_over$location_id <- pt_cam$location_id

#### 2. fazendo o merge das localidads com a info de agrotoxico ####
cam_atlas <- merge(pt_cam, cam_over, by="location_id")
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

head(cam_atlas_df)

nrow(pt_buf)

# extraindo os valores para cara localidade
mb_list <- list() 
for(i in 1:nrow(pt_buf)){
  mb_list[[i]] <- raster::extract(mb_buf, pt_buf[i,])
}

# criando um data frame com os valores de cada localidade
mb_table <- lapply(mb_list, function(x) table(x))
mb_df <- lapply(mb_table, function(x) data.frame(t(as.matrix(x)))) %>%
  bind_rows()

# criando coluna com area total
mb_df$total_area_pixel <- rowSums(mb_df, na.rm=TRUE)
# criando coluna com id para juntar os dados
mb_df$location_id <- pt_cam$location_id
# nomeando as colunas segundo as classes do mapbiomas
names(mb_df)[1:6] <- c("forest", "non_forest_natural", "farming",
                  "non_vegetated_area", "water_bodies", "none")

# criando coluna de prop de mata
mb_df$prop_forest<- mb_df$forest/mb_df$total_area_pixel

# criando coluna de prop de area agricola
mb_df$prop_farming<- mb_df$farming/mb_df$total_area_pixel

#### 4. finalmente criando variavel de manejo ####

# primeiro juntamos os dados do mapbiomas com os demais
pred_df <- merge(cam_atlas_df, mb_df, by="location_id") 

dim(pred_df)

head(pred_df)

pred_df$farming_intensity <- pred_df$prop_farming*as.numeric(pred_df$var130_e)

# exportando os dados
write.table(pred_df, "results/02_dados_preditoras.csv", 
            col.names = TRUE, row.names = FALSE, sep=",")

