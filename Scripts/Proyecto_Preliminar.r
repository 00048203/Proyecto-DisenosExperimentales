#######################################
# PROYECTO DISEÑOS EXPERIMENTALES     #
# SARMIENTO LOPEZ, ALEJANDRA MARÍA    #
# MENJIVAR SURIANO, MARINA ELIZABETH  #
# CARDONA RODRÍGUEZ, JOSE ALBERTO     #
# RAMÍREZ SOLÍS, LUIS EDMUNDO         #
#######################################

#####################
# Carga de paquetes #
#####################

library(openxlsx) # Para abrir documentos de Excel
library(tidyverse) # ggplot2, para gráficos
library(moments) # Estadísticos varios
library(nortest) # Test de normalidad no paramétricos
library(lmtest) # Test de normalidad no paramétricos
library(parameters) # Análisis de parámetros ANOVA
library(effectsize)
library(lsr) # Efectos
library(agricolae) # Test Multimedias
library(DescTools) # Test Multimedias
library(pwr2) #Potencia
library(car)
library(gridExtra)
library(effects) # Cierres por grupo
library(asbio) # Test de aditividad

##################
# Carga de datos #
##################

data.df <- read.xlsx(xlsxFile ="Data/Proyecto.xlsx",sheet = "Datos")
View(data.df)

colnames(data.df)
colnames(data.df) <- c("Temperatura","Salinidad","TipoAgua","NivelPH")

str(data.df)
data.df$Temperatura <- as.factor(data.df$Temperatura)
data.df$Salinidad <- as.factor(data.df$Salinidad)
data.df$TipoAgua <- as.factor(data.df$TipoAgua)

########################
# Exploración de datos #
########################

table(data.df$Temperatura)
table(data.df$Salinidad)
aggregate(NivelPH ~ Temperatura, data = data.df, FUN = mean)
aggregate(NivelPH ~ Temperatura, data = data.df, FUN = sd)
aggregate(NivelPH ~ Salinidad, data = data.df, FUN = mean)
aggregate(NivelPH ~ Salinidad, data = data.df, FUN = sd)
aggregate(NivelPH ~ TipoAgua, data = data.df, FUN = mean)
aggregate(NivelPH ~ TipoAgua, data = data.df, FUN = sd)

#NivelPH vs Temperatura
ggplot(data = data.df, aes(x = Temperatura, y = NivelPH, color = Temperatura)) +
  geom_boxplot() +   theme_bw()


ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = Salinidad)) +
  geom_boxplot() +   theme_bw()

ggplot(data = data.df, aes(x = TipoAgua, y = NivelPH, color = TipoAgua)) +
  geom_boxplot() +   theme_bw()

ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = Temperatura)) +
  geom_boxplot() +   theme_bw()

ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = TipoAgua)) +
  geom_boxplot() +   theme_bw()

ggplot(data = data.df, aes(x = Temperatura, y = NivelPH, color = TipoAgua)) +
  geom_boxplot() +  theme_bw()

##########################
# Gráficos de interacción#
##########################
interaction.plot(data.df$Salinidad, data.df$Temperatura, data.df$NivelPH,col = 1:3, type = "b")
interaction.plot(data.df$Salinidad, data.df$TipoAgua, data.df$NivelPH,col = 1:3, type = "b")
interaction.plot(data.df$TipoAgua, data.df$Temperatura, data.df$NivelPH,col = 1:3, type = "b")


##########################
# Gráficos usando ggplot #
##########################

ggplot(data = data.df, aes(x = Temperatura, y = NivelPH, color = Salinidad,
                           group = Salinidad))  +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  theme_bw()

ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = Temperatura,
                           group = Temperatura))  +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  theme_bw()

ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = TipoAgua,
                           group = TipoAgua))  +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  theme_bw()

ggplot(data = data.df, aes(x = Temperatura, y = NivelPH, color = TipoAgua,
                           group = TipoAgua))  +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  theme_bw()


#########
# ANOVA #
#########

anova1 <- aov(NivelPH ~ Temperatura + Salinidad + TipoAgua, data = data.df)
anova2 <- aov(NivelPH ~ Temperatura*Salinidad*TipoAgua, data = data.df)

summary(anova1)
model_parameters(anova1)
summary(anova2)
model_parameters(anova2)


###########
# Efectos #
###########

eta_squared(anova1, partial = FALSE)
etaSquared(anova1)
eta_squared(anova2, partial = FALSE)
eta_squared(anova2, partial = TRUE)

etaSquared(anova2)

####################
# SUPUESTOS ########
####################

################
# 1. Normalidad #
################

#Anderson-Darling
ad.test(anova2$residuals)

#######################
# 2. Homocedasticidad #
#######################
plot(anova2, which=1)
bartlett.test(NivelPH ~ interaction(Temperatura,Salinidad,TipoAgua), data = data.df)


####################
# 3. Independencia #
####################
#(Si y solo si nos dan el orden en que se recopilan los datos)
#No tiene sentido ya que no nos dan el orden que se tomaron los datos
plot(anova2$residuals)
#Durbin Watson
dwtest(anova2)

#acf(anova2$residuals, ylim=c(-1,1))

###################
# 4. Aditividad   #
###################
  #Cuando hay dos o más factores

tukey.add.test(data.df$NivelPH, data.df$Temperatura, data.df$Salinidad)
tukey.add.test(data.df$NivelPH, data.df$Temperatura, data.df$TipoAgua)
tukey.add.test(data.df$NivelPH, data.df$TipoAgua, data.df$Salinidad)

#p-value > 0.05 No rechazo H0
#p-value= 0.8728263>0.05
#No se rechaza H0
#Solo se realiza cuando hay más de dos factores
#Todos los efectos principales tienen un comportamiento aditivo.


###############################
# Comparación de tratamientos #
###############################
#Solamente a los que resultaron ser significativos en el ANOVA
#A menos que la interacción resulte significativa
#Se tendría que realizar para los factores

par(mfrow=c(1,1))

###########
# Fisher  #
###########

LSD.test(anova2, "Temperatura",console=T) 
LSD.test(anova2, "Salinidad",console=T) 
LSD.test(anova2, "TipoAgua",console=T) 
LSD.test(anova2, c("Temperatura","Salinidad"),console=T) 
LSD.test(anova2, c("Temperatura","TipoAgua"),console=T) 
LSD.test(anova2, c("Salinidad","TipoAgua"),console=T) 
LSD.test(anova2, c("Salinidad","TipoAgua","Temperatura"),console=T) 

###########
# Tukey   #
###########

TukeyHSD(anova2) 
plot(TukeyHSD(anova2))

HSD.test(anova2, "Temperatura",console=T)
HSD.test(anova2, "Salinidad",console=T)
HSD.test(anova2, "TipoAgua",console=T)

###########
# Duncan  #
###########

duncan.test(anova2, "Temperatura",console=T)
duncan.test(anova2, "Salinidad",console=T)
duncan.test(anova2, "TipoAgua",console=T)

###########
# Newman  #
###########

SNK.test(anova2, "Temperatura",console=T)
SNK.test(anova2, "Salinidad",console=T)
SNK.test(anova2, "TipoAgua",console=T)


###########
# Dunnet  # (Un grupò de control)
###########

DunnettTest(x=data.df$NivelPH, data.df$Temperatura)
DunnettTest(x=data.df$NivelPH, data.df$Salinidad)
DunnettTest(x=data.df$NivelPH, data.df$TipoAgua)
DunnettTest(x=data.df$NivelPH, interaction(data.df$Temperatura,data.df$Salinidad, data.df$TipoAgua))



