install.packages("multcompView")
install.packages("bear")


library(lattice)
library(lsmeans)
library(nlme)
library(ggplot2)
library(multcomp)
library(bear)
library(car)
library(gplots)
#######################################################################################################
forrajes <- read.csv("forrajes repetidos.csv")
summary(forrajes)
names(forrajes)
#[1] "bloque"      "tratamiento" "parcela"     "tiempo"      "cobertura"  
forrajes$tratamiento<-as.factor(forrajes$tratamiento)
forrajes$tiempo<-as.factor(forrajes$tiempo)
####graficos
(medias <- aggregate(cobertura~tiempo + tratamiento, forrajes, mean))
windows()
xyplot(cobertura~tiempo|tratamiento, forrajes, cex=1, pch=16, type=c("p","r"),layout=c(5,1))
xyplot(cobertura~tiempo|tratamiento,   type = c("p", "l") ,  medias, cex=1, lty=2, pch=16,layout=c(5,1))

barchart(cobertura ~ tratamiento | tiempo,medias,layout=c(5,1),xlab="tratamiento")
barchart(cobertura ~ tiempo |tratamiento,medias,layout=c(5,1),xlab="tiempo")

#Modelo 1: Efecto aleatorio de parcela anidada en bloque, errores independientes y homoscedásticos#########################
modelo.01<-lme(cobertura ~tratamiento + tiempo + tratamiento:tiempo,random = ~1|bloque/parcela, forrajes)
anova(modelo.01)
pseudo.R2 <- c(round(cor(getResponse(modelo.01),predict(modelo.01))^2,3))
criterios<-cbind(pseudo.R2,AIC(modelo.01),BIC(modelo.01))
colnames(criterios)<-c("pseudo.R","AIC","BIC")
criterios   
plot(predict(modelo.01), residuals(modelo.01), col="blue", pch=16)


# Modelo 2 agregamos estructura de correlacion entre los tiempos
modelo.02<-update(modelo.01, correlation=corAR1(form=~as.integer(as.character(tiempo))|bloque/parcela ))
anova(modelo.02)
pseudo.R2 <- c(round(cor(getResponse(modelo.02),predict(modelo.02))^2,3))
criterios<-cbind(pseudo.R2,AIC(modelo.02),BIC(modelo.02))
colnames(criterios)<-c("pseudo.R","AIC","BIC")
criterios  
plot(predict(modelo.02), residuals(modelo.02), col="blue", pch=16)
anova(modelo.01, modelo.02)                     
# 
# Modelo 3: Agregamos estructura de error heterogenea en el tiempo
modelo.03<-update(modelo.02,weights=varIdent(form= ~ 1|tiempo ))
anova(modelo.03)
pseudo.R2 <- c(round(cor(getResponse(modelo.03),predict(modelo.03))^2,3))
criterios<-cbind(pseudo.R2,AIC(modelo.03),BIC(modelo.03))
colnames(criterios)<-c("pseudo.R","AIC","BIC")
criterios  
plot(predict(modelo.03), residuals(modelo.03), col="blue", pch=16)
anova(modelo.02, modelo.03)


forrajes$TRAT<-as.factor(paste(forrajes$tratamiento, forrajes$tiempo))
leveneTest(residuals(modelo.02)~forrajes$TRAT)
qqnorm(residuals(modelo.02))
qqline(residuals(modelo.02))
plot(density(residuals(modelo.02)))
shapiro.test(residuals(modelo.02))
#########################pruebas de comparciones multiples#########################

# Grafico de barras tratamiento
(desvio <- aggregate(cobertura~tratamiento , forrajes, sd))
(medias <- aggregate(cobertura~tratamiento , forrajes, mean))
# Use 95% confidence interval instead of SEM
dodge <- position_dodge(width=0.9)
(limits <- aes(ymax = medias$cobertura + max(desvio$cobertura), ymin=medias$cobertura - min(desvio$cobertura)))
p<-ggplot(medias, aes(x=tratamiento, y=cobertura, fill=tratamiento)) 
p + geom_bar(position="dodge", stat="identity") + geom_errorbar(limits, position=dodge, width=0.25)

# Grafico de barras tiempo
(desvio <- aggregate(cobertura~tiempo , forrajes, sd))
(medias <- aggregate(cobertura~tiempo , forrajes, mean))
# Use 95% confidence interval instead of SEM
(limits <- aes(ymax = medias$cobertura + max(desvio$cobertura), ymin=medias$cobertura - min(desvio$cobertura)))
dodge <- position_dodge(width=0.9)
p<-ggplot(medias, aes(x=tiempo, y=cobertura, fill=tiempo)) 
p + geom_bar(position="dodge", stat="identity") + geom_errorbar(limits, position=dodge, width=0.50)


####pruebas de comparaciones multiples
vignette("generalsiminf", package = "multcomp")

comparaciones.mult.tratamiento<- glht(modelo.02, linfct=mcp(tratamiento="Tukey"))
summary(comparaciones.mult.tratamiento)
plot(comparaciones.mult.tratamiento)

#uso otra paquete
pruebas.trat<-lsmeans(modelo.02,~ tratamiento, adjust="tukey")
cld(pruebas.trat)

pruebas.tiempo<-lsmeans(modelo.02,~ tiempo, adjust="tukey")
cld(pruebas.tiempo)


