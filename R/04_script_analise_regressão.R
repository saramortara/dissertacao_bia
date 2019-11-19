##### An?lise de regress?o linear #####

# carregando os pacotes
library(lme4) # modelos mistos
library(bbmle) # AICtab
library(MuMIn) # para R2 de modelos mistos
library(scales) # para rescalonar variavel effort
library(ggplot2) # para graficos
library(ggpubr)

####criando o diretorio#### BAIXAR NOVOS DOCUMENTOS
#setwd("C:\\Users\\beatr\\Desktop\\dissertacao_bia-master\\results")
getwd()

###USAR dados FORMADOS EM "BETADIVERSIDADE E MANEJO"###
#lendo os dados - transformar planilhas em objetos
dados <- read.csv("data/dados_geral.csv", header = TRUE, sep = ",")

head(dados)


##########################################
### DISTRIBUICAO DAS VARIAVEIS  ##########
##########################################

##histograma
hist(dados$farming_intensity)

##adicionar linha de media
abline(v = mean(dados$farming_intensity, na.rm = TRUE), col = "green", lwd = 3)

##histograma
### beta total
hist(dados$beta.sor)
abline(v = mean(dados$beta.sor, na.rm = TRUE), col = "green", lwd = 3)

### beta substituicao
hist(dados$beta.sim)
abline(v = mean(dados$beta.sim, na.rm = TRUE), col = "green", lwd = 3)

### beta aninhamento
hist(dados$beta.sne)
abline(v = mean(dados$beta.sne, na.rm = TRUE), col = "green", lwd = 3)

###rela??o entre as variaveis
#par(mfrow = c(1, 3))
plot(beta.sor ~ farming_intensity, data = dados, las = 1 , bty = "l", main = "Total")
plot(beta.sim ~ farming_intensity, data = dados, las = 1 , bty = "l", main = "Turnover")
plot(beta.sne ~ farming_intensity, data = dados, las = 1 , bty = "l", main = "Aninhamento")
plot(alfa ~ farming_intensity, data = dados, las = 1 , bty = "l", main = "Riqueza")

names(dados)
###rela??o entre as variaveis
#par(mfrow = c(1, 3))
plot(beta.sor ~ prop_farming, data = dados, las = 1 , bty = "l", main = "Total")
plot(beta.sim ~ prop_farming, data = dados, las = 1 , bty = "l", main = "Turnover")
plot(beta.sne ~ prop_farming, data = dados, las = 1 , bty = "l", main = "Aninhamento")
plot(alfa ~ prop_farming, data = dados, las = 1 , bty = "l", main = "Riqueza")
#par(mfrow = c(1, 1))

names(dados)
## correlacao entre as variaveis preditoras
cor(dados[,c('prop_farming', 'prop_forest', 'farming_intensity')])

###############################
#### CONSRTU??O DE MODELOS ####
###############################

##criar modelo linear e guardar como objeto - usa as duas v?riaveis

##criar objetos para representar as hipoteses

head(dados)

# rescalonando a variavel de manejo
sort(dados$effort)
dados$effort2 <- rescale(dados$effort)

# removendo os valores na de intensidade de manejo
dados2 <- dados[!is.na(dados$farming_intensity),]
dim(dados2)

dim(dados)

# checando se os NA foram embora
apply(dados, 2, function(x) sum(is.na(x)))

# Modelos para beta total
h1.1 <- lmer(beta.sor ~ farming_intensity + (1|location_id), 
             # weights = dados$effort2, 
             data = dados)

h0.1 <- lmer(beta.sor ~ 1 + (1|location_id), 
             #weights = effort2, 
             data = dados)

AICctab(h1.1, h0.1)

# Modelos para beta turnover
h1.2 <- lmer(beta.sim ~ farming_intensity + (1|location_id), 
             #weights = effort2, 
             data = dados)

h0.2 <- lmer(beta.sim ~ 1 + (1|location_id), 
             #weights = effort2, 
             data = dados)

AICctab(h1.2, h0.2)

# Modelos para alfa diversidade
h1.3 <- glmer(alfa ~ farming_intensity + (1|location_id), 
              #weights = effort2, 
              family = 'poisson',
              data = dados)

h0.3 <- glmer(alfa ~ 1 + (1|location_id), 
              #weights = effort2, 
              family = 'poisson',
              data = dados)

AICctab(h1.3, h0.3)

#### Calculando o R2 para os modelos mistos ####
# primeiro entre em ?r.squaredGLMM
r.squaredGLMM(h1.1)

#### Criando os graficos ####

### Exemplo de grafico para a riqueza

# criando objetos para auxiliar grafico
## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

# grafico riqueza ~ intensidade de manejo
fig <- ggplot(dados, aes(x = farming_intensity, y = alfa)) +
  geom_smooth(method = lm, fill = "grey75", se = TRUE) +
  geom_point(shape = 19, size = tamp, alpha = 0.5) + # size=3
  labs(x = "Intensidade do manejo", y = "Riqueza") +
  #scale_color_manual(values = cor) +
  theme_classic()

# criando figura com 4 paineis para intensidade de manejo
r1 <- ggarrange(
  fig %+%  aes(y = beta.sor) + labs(x = "", y = "Beta total") + ggtitle("A"),
  fig %+%  aes(y = beta.sim) + labs(x = "", y = "Beta substituição") + ggtitle("B"),
  fig %+%  aes(y = beta.sne) + labs(x = "", y = "Beta aninhamento") + ggtitle("C"),
  fig + labs(x = "") + ggtitle("D"), 
  ncol = 4
)

annotate_figure(r1, bottom = "Intensidade de manejo")

# criando figura com 4 paineis para proporção de mata
r2 <- ggarrange(
  fig %+%  aes(x = prop_farming, y = beta.sor) + 
    labs(x = "", y = "Beta total") + ggtitle("A"),
  fig %+%  aes(x = prop_farming, y = beta.sim) + 
    labs(x = "", y = "Beta substituição") + ggtitle("B"),
  fig %+%  aes(x = prop_farming, y = beta.sne) + 
    labs(x = "", y = "Beta aninhamento") + ggtitle("C"),
  fig +  aes(x = prop_farming) +
    labs(x = "") + ggtitle("D"), 
  ncol = 4
)

annotate_figure(r2, bottom = "Proporção de área agrícola")

# exportando as figuras
png("figs/figure01_manejo.png", res = 300, height = 1000, width = 2800)
annotate_figure(r1, bottom = "Intensidade de manejo")
dev.off()

png("figs/figure02_prop_agricola.png", res = 300, height = 1000, width = 2800)
annotate_figure(r2, bottom = "Proporção de área agrícola")
dev.off()

