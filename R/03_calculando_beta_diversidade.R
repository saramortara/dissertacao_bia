#### Script 03. Calculando a beta diversidade ####

# carregando os pacotes
## para calculo da beta
library(betapart) 
## para manipalacao dos dados
library(reshape) 
library(dplyr) 

#### 1. lendo os dados ####
## dados do data paper CAMTRAP
cam_files <- list.files(path="data", pattern="ATLANTIC_CAMTRAPS", 
                        full.names = TRUE)

head(cam_files)

cam <- lapply(cam_files, read.csv)
names(cam) <- c('location', 'records', 'species', 'study', 'survey') 

str(cam)

## id das areas para excluir
areas_excluir <- read.csv("results/02_areas_excluir.csv", header = TRUE)

#### 2. criando matriz area x especie para calculo da betadiversidade ####
### especies nas colunas e areas nas linhas
data <- cam$records

head(data)

data_cast <- cast(data, survey_id ~ species_id, value="presence_absence")
rownames(data_cast) <- data_cast$survey_id

head(data_cast)

dim(areas_excluir)

# excluindo a primeira coluna com o survey_id e as areas que nao temos dados
area_sp <- data_cast[!data_cast$survey_id%in%areas_excluir$loctn_d,-1]

dim(area_sp)

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
dir.create("results/")
write.table(as.matrix(beta_par$beta.sim), "results/03_beta_par_turnover.csv", sep=",", col.names=NA)
write.table(as.matrix(beta_par$beta.sne), "results/03_beta_par_nestedness.csv", sep=",", col.names=NA)
write.table(as.matrix(beta_par$beta.sor), "results/03_beta_par_sorensen_total.csv",  sep=",", col.names=NA)
write.table(beta_tab, "results/03_beta_total.csv", sep=",", col.names=NA)

#### 4. calculando beta media e gerando tabela alfa, beta por area ####

colMeans(as.matrix(beta_par$beta.sim))

beta_mean <- lapply(beta_par, function(x) colMeans(as.matrix(x)))

# criando data.frame com valores de beta medias
div_df <- as.data.frame(matrix(unlist(beta_mean), ncol=3, nrow=162, byrow = FALSE))

div_df$survey_id <- rownames(area_sp)

head(div_df)

div_df$alfa <- rowSums(area_sp)

head(div_df)

names(div_df)[1:3] <- c("beta.sim", "beta.sne", "beta.sor")

write.table(div_df, "results/03_diversity.csv", 
            col.names = TRUE, 
            row.names=FALSE)
