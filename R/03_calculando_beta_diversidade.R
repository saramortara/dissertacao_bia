#### Script 03. Calculando a beta diversidade ####

# carregando os pacotes
library(betapart)
library(reshape)

#### 1. lendo os dados ####
cam_files <- list.files(path="data", pattern="ATLANTIC_CAMTRAPS", 
                        full.names = TRUE)

head(cam_files)

cam <- lapply(cam_files, read.csv)
names(cam) <- c('location', 'records', 'species', 'study', 'survey') 


#### 2. criando matriz area x especie para calculo da betadiversidade ####
### especies nas colunas e areas nas linhas
data <- cam$records

data_cast <- cast(data, survey_id ~ species_id, value="presence_absence")
rownames(data_cast) <- data_cast$survey_id

# excluindo a primeira coluna com o survey_id
area_sp <- data_cast[,-1]

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

### calculando a beta diversidade par a par
beta_par <- beta.pair(area_sp)

str(beta_par)
# beta.sim = turnover
# beta.sne = nestedness
# beta.sor = total (sorensen)

### exportando os dados para quatro planilhas diferentes, arquivos csv separados por virgula
dir.create("results/")
write.table(as.matrix(beta_par$beta.sim), "results/beta_par_turnover.csv", sep=",", col.names=NA)
write.table(as.matrix(beta_par$beta.sne), "results/beta_par_nestedness.csv", sep=",", col.names=NA)
write.table(as.matrix(beta_par$beta.sor), "results/beta_par_sorensen_total.csv",  sep=",", col.names=NA)
write.table(beta_tab, "results/beta_total.csv", sep=",", col.names=NA)

