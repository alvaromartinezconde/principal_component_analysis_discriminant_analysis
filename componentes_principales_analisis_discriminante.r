library(descr)
library(MASS)
library(heplots)
library(mvnormtest)

# Modelo Análisis Discriminante

modelo_discrimin <- lda(data=futbol1, COVID_19_NUM ~ goles_local  + primera_visitante + remates_local + remates_visitante + 
                          faltas_local + faltas_visitante + corners_local + 
                          amarillas_local+ amarillas_visitante)

predict(modelo_discrimin)$class


discrim = futbol1[ ,c(8,13,17,18,21,22,23,25,26,35)]

##### scatterplotMatrix(discrim)

modelo_discrimin$scaling

modelo_discrimin$means

plot(modelo_discrimin)

summary(modelo_discrimin)

# Análisis Componentes Principales

comprincip <- prcomp(reduccionCorr)
plot(prop_varianza_acum, type="l")
comprincip$rotation[, 1:4]

comprincip$rotation[, 1:4]

Fisher<-comprincip$x[, 1:4]


predicciones <- table(predict(modelo_discrimin)$class, futbol1$COVID_19_NUM) 

table(predict(modelo_discrimin)$class, futbol1$COVID_19_NUM) 

tabla<-data.frame(predicciones) 


fit.p <- predict(modelo_discrimin)$class

Discriminante <- data.frame(futbol1,fit.p)

CrossTable(futbol1[ ,34],modelo_discrimin$fit.p, digits=2, format= "SPSS",
           prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE, dnn= c("Grupo real","Grupo pronosicado"))

plot(comprincip)
biplot(comprincip)

FisherUnion <- cbind(Fisher,Discriminante$COVID_19_NUM)

FisherUnion <- data.frame(FisherUnion)

FisherUnion <- rename(FisherUnion, COVID=V5)

names(FisherUnion)

# Análisis Discriminante con componentes principales

modelo_discrimin <- lda(data=FisherUnion, COVID ~ .)

predict(modelo_discrimin)$class

table(predict(modelo_discrimin)$class, futbol1$COVID_19_NUM) 


shapiro.test(FisherUnion$PC1)
shapiro.test(FisherUnion$PC2)
shapiro.test(FisherUnion$PC3)
shapiro.test(FisherUnion$PC4)
shapiro.test(FisherUnion$COVID)

prop_varianza <- comprincip$sdev^2 / sum(comprincip$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum
