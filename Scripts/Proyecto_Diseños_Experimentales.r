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
colnames(data.df) <- c("Temperatura","Salinidad","TipoAgua","NivelPH","Orden")


str(data.df)

data.df$Temperatura <- as.factor(data.df$Temperatura)
data.df$Salinidad <- as.factor(data.df$Salinidad)
data.df$TipoAgua <- as.factor(data.df$TipoAgua)
data.df <- data.df[order(data.df$Orden),] #(Por orden)

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

#NivelPH vs Temperatura, color by Temperatura
ggplot(data = data.df, aes(x = Temperatura, y = NivelPH, color = Temperatura)) +
  geom_boxplot() +   theme_bw()

#Salinidad vs NivelPH, color by Salinidad
ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = Salinidad)) +
  geom_boxplot() +   theme_bw()

#TipoAgua vs NivelPH, color by TipoAgua
ggplot(data = data.df, aes(x = TipoAgua, y = NivelPH, color = TipoAgua)) +
  geom_boxplot() +   theme_bw()

#Salinidad vs NivelPH, color by Temperatura
ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = Temperatura)) +
  geom_boxplot() +   theme_bw()

#Salinidad vs NivelPH, color by TipoAgua
ggplot(data = data.df, aes(x = Salinidad, y = NivelPH, color = TipoAgua)) +
  geom_boxplot() +   theme_bw()

#Temperatura vs NivelPH, color by TipoAgua
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
# ANOVA # TABLA ANOVA
#########

#anova1 <- aov(NivelPH ~ Temperatura + Salinidad + TipoAgua, data = data.df)
anova2 <- aov(NivelPH ~ Temperatura*Salinidad*TipoAgua, data = data.df)

#summary(anova1)
#model_parameters(anova1)
summary(anova2)
model_parameters(anova2)

##########
#Factores# Analizados a nivel de significancia 5%
##########
#Temperatura  -> resulta ser significativa p-value = 3.24e-06 < 0.05
#Salinidad    -> resulta ser significativa p-value = 1.34e-11 < 0.05
#TipoAgua     -> resulta ser significativa p-value = 2.95e-05 < 0.05


##########################
#Interacciones de orden 2# Analizados a nivel de significancia 5%
##########################
#Temperatura:Salinidad  -> resulta ser NO significativa p-value = 0.7448 > 0.05
#Temperatura:TipoAgua   -> resulta ser significativa p-value = 1.21e-06 < 0.05
#Salinidad:TipoAgua     -> resulta ser NO significativa p-value = 0.0694 < 0.05


##########################
#Interacciones de orden 3# Analizados a nivel de significancia 5%
##########################
#Temperatura:Salinidad:TipoAgua -> resulta ser NO significativa p-value = 0.1833 < 0.05


###########
# Efectos # EVALUACIÓN DE EFECTOS
###########

eta_squared(anova1, partial = FALSE)
etaSquared(anova1)
eta_squared(anova2, partial = FALSE)
eta_squared(anova2, partial = TRUE)

etaSquared(anova2)

####################
# SUPUESTOS ######## COMPROBACIÓN DE SUPUESTOS
####################

################
# 1. Normalidad #
################

###################
# Métodos gráficos#
###################
par(mfrow=c(1,1))

hist(anova2$residuals)
hist(anova2$residuals, breaks = 10) 

plot(anova2, which = 2)
qqnorm(anova2$residuals)
qqline(anova2$residuals)

##################
#Anderson-Darling#
##################
ad.test(anova2$residuals)

#Anderson-Darling normality test

#data:  anova2$residuals
#A = 0.21557, p-value = 0.8267

#######################
# 2. Homocedasticidad #
#######################
plot(anova2, which=1)
bartlett.test(NivelPH ~ interaction(Temperatura,Salinidad,TipoAgua), data = data.df)
# p-value = 0.6394 > 0.05 
# H0: Las varianzas entre todos los grupos son iguales entre si
# No puedo rechazar H0
# Por ende existe homocedasticidad


####################
# 3. Independencia #
####################
plot(anova2$residuals)

#Durbin Watson
dwtest(anova2)

#DW = 2.3385, p-value = 0.7418 > 0.05
# No se rechaza H0
# H1 Existe una autocorrelación positiva y es más grande que 0
# H0: No hay una autocorrelación
# Los datos no están autocorrelacionados
# No hay independencia entre los datos


###################
# 4. Aditividad   #
###################
#Cuando hay dos o más factores
#Dentro del rango en que evaluó los factores debe de haber una tendencia lineal
#H0:Los efectos principales son aditivos
#H1: Los efectos principales no son aditivos

tukey.add.test(data.df$NivelPH, data.df$Temperatura, data.df$Salinidad)
#p-value = 0.9646107 > 0.05
#No puedo rechazar H0
#Por ende todos mis efectos principales tienen un comportamiento aditivo.
#No hay efectos que sean multiplicativos

tukey.add.test(data.df$NivelPH, data.df$Temperatura, data.df$TipoAgua)
#p-value = 0.411978 > 0.05
#No puedo rechazar H0
#Por ende todos mis efectos principales tienen un comportamiento aditivo.
#No hay efectos que sean multiplicativos

tukey.add.test(data.df$NivelPH, data.df$TipoAgua, data.df$Salinidad)
#p-value = 0.8886523 > 0.05
#No puedo rechazar H0
#Por ende todos mis efectos principales tienen un comportamiento aditivo.
#No hay efectos que sean multiplicativos


###############################
# Comparación de tratamientos # PRUEBAS MULTIMEDIAS
###############################
#***Significativos en el ANOVA
#Temperatura***
#Salinidad***
#TipoAgua***
#Temperatura:TipoAgua***
par(mfrow=c(1,1))

###########
# Fisher  #
###########

LSD.test(anova2, "Temperatura",console=T)
plot(LSD.test(anova2,"Temperatura",console = T))

LSD.test(anova2, "Salinidad",console=T)
plot(LSD.test(anova2,"Salinidad",console = T))

LSD.test(anova2, "TipoAgua",console=T) 
plot(LSD.test(anova2,"TipoAgua",console = T))

LSD.test(anova2, c("Temperatura","TipoAgua"),console=T) 

#LSD.test(anova2, c("Temperatura","Salinidad"),console=T) 
#LSD.test(anova2, c("Salinidad","TipoAgua"),console=T) 
#LSD.test(anova2, c("Salinidad","TipoAgua","Temperatura"),console=T) 

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
# Dunnet  # (Un grupo de control)
###########

DunnettTest(x=data.df$NivelPH, data.df$Temperatura)

DunnettTest(x=data.df$NivelPH, data.df$Salinidad)

DunnettTest(x=data.df$NivelPH, data.df$TipoAgua)

DunnettTest(x=data.df$NivelPH, interaction(data.df$Temperatura,data.df$Salinidad, data.df$TipoAgua))



