##### An?lise de regress?o linear #####

# carregando os pacotes
library(lme4) # modelos mistos
library(bbmle) # AICtab
library(scales) # para rescalonar variavel effort
library(ggplot2) # para graficos

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

sort(dados$effort)
dados$effort2 <- rescale(dados$effort)

dados2 <- dados[!is.na(dados$farming_intensity),]
dim(dados2)

dim(dados)

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

### Exemplo de grafico para a riqueza

# criando objetos para auxiliar grafico
## tamanho do texto e numeros
tamt <- 18
## tamanho dos pontos
tamp <- 3

r <- ggplot(dados, aes(x = farming_intensity, y = alfa)) +
  geom_smooth(method = lm, fill = "grey80", se = TRUE) +
  geom_point(shape = 19, size = tamp, alpha = 0.5) + # size=3
  labs(x = "Intensidade do manejo", y = "Riqueza)") +
  #scale_color_manual(values = cor) +
  theme_classic(base_size = tamt)

r



###### a partir daqui não roda ##########################
#Hipotese 2 - o tamanho do fragmento, junto com o tipo de matriz, interfere na riqueza de esp?cies
h2<- glm(riqueza~log(Size)+Vegetation.type, data = floresta, family = 'poisson')

#Hipotese 3 - nula
h3<- glm(riqueza~1, data = floresta, family= 'poisson')

##sumario dos modelos

#sumario hipotese 1
summary(h1)

#sumario hipotese 2
summary(h2)

#sumario hipotese 3
summary(h3)

##############################
#### INSPE??O DE RESIDUOS ####
##############################

##mudar a conforma??o
par(mfrow=c(2,2))

#inspes?o de cada modelo - gerando o plot do modelo

#residuos hipotese 1
plot(h1)

#residuos hipotese 2
plot(h2)

#residuos hipotese 3
plot(h3)

##voltando a conforma??o
par(mfrow=c(1,1))

#######################################
#### COMPARANDO OS MODELOS COM AIC ####
#######################################

library(bbmle)

AICtab(h1, h2, h3, weights=TRUE, base=TRUE)

summary(h1)

###############################################
#### COMPARAR AJUSTE DO MODELO COM GRAFICO ####
###############################################

##para consiguir identificar onde inicia a sequencia/termina
range(log(floresta$Size),na.rm = TRUE)

##criar a sequencia para eixo x
xv<- seq(-2,14, length= 294)

##predict - necessario para ver como seria o modelo e como com os dados reais

##fun??o range - serve para verificar o tamanho da do primeiro numero do eixo x e ultimo
range(log(floresta$Size),na.rm=TRUE)

##criar um novo data para ser o objeto Size
data.new <- data.frame(Size=xv)

##fazer a predi??o
predh1<- predict(h1, newdata=data.new, type="response")

#fun??o length serve para ver variaveis tem ao redor
length(predh1)

#sumario de predict
summary(predh1)

#criar um objeto com o expoente de coef de h1, para poder plotar os dados e fazer a curva do modelo
coefh1<- exp(coef(h1))

##plot dos dados riqueza e tamanho frag (log)

plot(riqueza ~ log(Size), data=floresta, col=floresta$cor,las=1, 
     xlab="Size(log)", ylab="Riqueza", pch=1, bty= 'l')

##curva do modelo. A fun??o 'curve' ? fun??o, ent?o precisar ter dados
curve(coefh1[1]+ coefh1[2]*x, add = TRUE)

###incluindo legenda
legend("topright", c("Ombr?fila", "Semidecidua", "Decidua"), 
       col=c("darkgreen", "green", "orange"), pch=1)

#retirando pontos de camptrap
camtrap.novo <- camtrap[c(-2,-3,-6,-9,-10,-11,-17,-19,-24,-25,-27,-29,-42,-43,-58,-62,-80,-81,-83,-86,-92,-94,-105,-107,-108,-139,-141,-144),]

head(camtrap.novo)
tail(camtrap.novo)
