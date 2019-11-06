#### Script 03. Calculando a beta diversidade ####

# carregando os pacotes
## para calculo da beta
library(betapart) 
## para manipalacao dos dados
library(reshape) 
library(dplyr) 

#### 1. lendo os dados ####
## dados do data paper CAMTRAP
cam_files <- list.files(path = "data", pattern = "ATLANTIC_CAMTRAPS",
                        full.names = TRUE)

head(cam_files)

cam <- lapply(cam_files, read.csv, as.is = TRUE)
names(cam) <- c('location', 'records', 'species', 'study', 'survey') 

str(cam)

lapply(cam, head)

survey <- cam$survey

head(survey)

ids <- survey[,c('location_id', 'survey_id', 'effort', 'design')]

# checando os tipos de desenho
table(ids$design)
# corrigindo os valores de desenho
## points vira ponto
ids$design[ids$design == 'points'] #
ids$design[ids$design == 'points'] <- 'point'
## vazio vira NA
ids$design[ids$design == ''] #
ids$design[ids$design == ''] <- NA
## checando novamente
table(ids$design)

head(ids)

## id das areas para excluir
dados_pred <- read.csv("results/02_preditoras_editada.csv", header = TRUE)
#areas_excluir <- read.csv("results/02_areas_excluir.csv", header = TRUE)

head(dados_pred) # location_id
areas_ficam <- data.frame(location_id = dados_pred$location_id[!is.na(dados_pred$farming_intensity)])

areas_df <- merge(areas_ficam, ids, by = 'location_id', all.x = TRUE, all.y = FALSE)
dim(areas_ficam)
dim(areas_df)

head(areas_df)

table(areas_df$location_id)
length(unique(areas_ficam$location_id))

#### 2. criando matriz area x especie para calculo da betadiversidade ####
### especies nas colunas e areas nas linhas
data <- cam$records #survey_id

head(data)

data_cast <- cast(data, survey_id ~ species_id, value = "presence_absence")
rownames(data_cast) <- data_cast$survey_id

head(data_cast)

dim(areas_df)
dim(data_cast)

# excluindo a primeira coluna com o survey_id e as areas que nao temos dados
area_sp <- data_cast[data_cast$survey_id %in% areas_df$survey_id,-1]

dim(area_sp)
dim(areas_df)

rownames(area_sp)

#### 3. calculo da beta diversidade #### 
head(area_sp)

### calculando a beta diversidade geral
beta_total <- beta.multi(area_sp)

beta_total

##  criando uma tabela com os valores 
beta_tab <- as.data.frame(unlist(beta_total))
names(beta_tab) <- c("value")

beta_tab

beta_tab$prop <- beta_tab$value/beta_tab$value[3]

### calculando a beta diversidade par a par
beta_par <- beta.pair(area_sp)

str(beta_par)
# beta.sim = turnover
# beta.sne = nestedness
# beta.sor = total (sorensen)

## exportando os dados para quatro planilhas diferentes, arquivos csv separados por virgula
#dir.create("results/")
write.table(as.matrix(beta_par$beta.sim), "results/03_beta_par_turnover.csv", 
            sep = ",", col.names = NA)
write.table(as.matrix(beta_par$beta.sne), "results/03_beta_par_nestedness.csv", 
            sep = ",", col.names = NA)
write.table(as.matrix(beta_par$beta.sor), "results/03_beta_par_sorensen_total.csv", 
            sep = ",", col.names = NA)
write.table(beta_tab, "results/03_beta_total.csv", 
            sep = ",", col.names = NA)

#### 4. calculando beta media e gerando tabela alfa, beta por area ####

colMeans(as.matrix(beta_par$beta.sim))

beta_mean <- lapply(beta_par, function(x) colMeans(as.matrix(x)))

# criando data.frame com valores de beta medias
div_df <- as.data.frame(matrix(unlist(beta_mean), 
                               ncol = 3, nrow = nrow(area_sp), byrow = FALSE))
  
div_df$survey_id <- rownames(area_sp)
names(div_df)[1:3] <- c("beta.sim", "beta.sne", "beta.sor")

head(div_df)

div_df$alfa <- rowSums(area_sp)

head(div_df)

write.table(div_df, "results/03_diversity.csv", 
            col.names = TRUE, 
            row.names = FALSE)

#### 5. juntando todos os dados ####
head(ids)

## primeiro junta com o id
div_ids <- merge(div_df, ids, by = 'survey_id')

dim(div_ids)

head(div_ids)

## agora juntamos com as variaveis preditoras
head(dados_pred)
dim(div_ids)

dados_geral <- merge(div_ids, dados_pred[,-1], by = 'location_id')

head(dados_geral)

write.table(dados_geral, 
            "data/dados_geral.csv", 
            col.names = TRUE, row.names = FALSE, sep = ",")


apply(dados_geral, 2, function(x) sum(is.na(x)))
