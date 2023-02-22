datos<-read.table("https://gente.itam.mx/lnieto/index_archivos/salarios.txt",header=TRUE)
library(R2jags)
datos
n=nrow(datos)
n
x=datos[,2:4]
m=3
xf=rbind(c(5.4,17,6.0), c(6.2,12,5.8), c(6.4,21,6.1))
xf
data<-list("n"=n,'m'=m,"y"=datos[,1],"x"=x, 'xf'=xf)
inits=function(){list(alpha=0, beta=rep(0,3),tau=1,yf1=rep(0,n), yf2=rep(0,m))}
#-Selecting parameters to monitor-
parameters<-c('alpha',"beta","tau","yf1", 'yf2')
set.seed(1234)
ej4.sim<-jags(data,inits,parameters,model.file="Ej4.txt",
              n.iter=10000,n.chains=1,n.burnin=1000,n.thin=1)
traceplot(ej4.sim)
out<-ej4.sim$BUGSoutput$sims.list
z<-out$beta
pairs(z)
cor(z)
out.sum<-ej4.sim$BUGSoutput$summary
out.sum
out.sum[1,]
#vamos a comparar esto con las beta gorro
X=cbind(rep(1,n), x)
X
X=as.matrix(X)
X
dim(X)
beta_gorro=solve(t(X)%*%X)%*%t(X)%*%as.matrix(datos[,1])
beta_gorro #:) Bien:)))))))
#son muy cercanos a las medias que encontramos en la cadena :)))))
#las cuales pueden verse en el summary, por ejemplo.
#vamos a ver si podemos ver como se calculo yf1[1]   32.4362941 
beta_gorro
x
x[1,]
t(t(x[1,]))
beta_gorro[1]+t(t(x[1,]))%*%beta_gorro[2:4]
#Nice!!! Muy cercano a 32.43629
out.sum[1,1]+t(t(x[1,]))%*%out.sum[2:4,1]
#ok cercana tambien. Quien sabe que pei.

xf[1,]
out.sum[2:4,1]

#a ver la prediccion para la primera...
#muy cercanaaa yf2[1]   34.082215
out.sum[1,1]+xf[1,]%*%out.sum[2:4,1]

out<-ej4.sim$BUGSoutput$sims.list
out$beta#la primera columna es la cadena de beta 1 la segunda es la cadena de beta2

out$yf1
hist(out$yf1)
plot(density(out$yf1))


#para usar notacion de LaTeX
library(latex2exp)

#JAGS
out<-ej4.sim$BUGSoutput$sims.list
out
z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l", main=TeX(r"(\textbf{Traza de la cadena para $\alpha$}.)"), col='red',
     xlab='Iteración', ylab=TeX(r"($\alpha$)"), lwd=2)
plot(cumsum(z)/(1:length(z)),type="l", main='Promedios ergódicos', xlab='Iteración',
     ylab=TeX(r"($\bar{\alpha})"), lwd=2) 
abline(h=mean(z), col='red', lwd=1)
hist(z,freq=FALSE, main='Histograma de las realizaciones', xlab=TeX(r"($\alpha$)"))
acf(z, main="Autocorrelación")

#replicamos con cada parametro
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l", main=TeX(r"(\textbf{Traza de la cadena para $\beta_1$}.)"), col='orange',
     xlab='Iteración', ylab=TeX(r"($\beta_1$)"), lwd=2)
plot(cumsum(z)/(1:length(z)),type="l", main='Promedios ergódicos', xlab='Iteración',
     ylab=TeX(r"($\bar{\beta_1})"), lwd=2) 
abline(h=mean(z), col='orange', lwd=1)
hist(z,freq=FALSE, main='Histograma de las realizaciones', xlab=TeX(r"($\beta_1$)"))
acf(z, main="Autocorrelación")
out$beta

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l", main=TeX(r"(\textbf{Traza de la cadena para $\beta_2$}.)"), col='green4',
     xlab='Iteración', ylab=TeX(r"($\beta_2$)"), lwd=2)
plot(cumsum(z)/(1:length(z)),type="l", main='Promedios ergódicos', xlab='Iteración',
     ylab=TeX(r"($\bar{\beta_2})"), lwd=2) 
abline(h=mean(z), col='green4', lwd=1)
hist(z,freq=FALSE, main='Histograma de las realizaciones', xlab=TeX(r"($\beta_2$)"))
acf(z, main="Autocorrelación")



z<-out$beta[,3]
par(mfrow=c(2,2))
plot(z,type="l", main=TeX(r"(\textbf{Traza de la cadena para $\beta_3$}.)"), col='blue',
     xlab='Iteración', ylab=TeX(r"($\beta_3$)"), lwd=2)
plot(cumsum(z)/(1:length(z)),type="l", main='Promedios ergódicos', xlab='Iteración',
     ylab=TeX(r"($\bar{\beta_3})"), lwd=2) 
abline(h=mean(z), col='blue', lwd=1)
hist(z,freq=FALSE, main='Histograma de las realizaciones', xlab=TeX(r"($\beta_3$)"))
acf(z, main="Autocorrelación")


z<-out$tau
par(mfrow=c(2,2))
plot(z,type="l", main=TeX(r"(\textbf{Traza de la cadena para $\tau$}.)"), col='goldenrod',
     xlab='Iteración', ylab=TeX(r"($\tau$)"), lwd=2)
plot(cumsum(z)/(1:length(z)),type="l", main='Promedios ergódicos', xlab='Iteración',
     ylab=TeX(r"($\bar{\tau})"), lwd=2) 
abline(h=mean(z), col='goldenrod', lwd=1)
hist(z,freq=FALSE, main='Histograma de las realizaciones', xlab=TeX(r"($\tau$)"))
acf(z, main="Autocorrelación")




z=out$beta
pairs(data.frame(alpha=out$alpha, beta_1=z[,1],beta_2=z[,2], beta_3=z[,3]), col='orange', cex=2)
cor(cbind(out$alpha, z))





#podemos estudiar esta distribucion y compararla con una normal 
library(ggplot2)
z
z=as.data.frame(x=z)
z
names(z)
names(z)='z'
z
names(z)
ggplot(z, aes(x=z))+
  geom_histogram()
ej4.sim$BUGSoutput$summary
ej2.sim$BUGSoutput$summary
#nos reporta estimaciones maximo verosimiles de una normal suponiendo que 
#la posterior marginal de la media converge a una normal

ggplot(z, aes(x=z))+
  geom_density(col='blue')+
  stat_function(fun = dnorm, args = c(200, sqrt(40)), col='red')+
  stat_function(fun = dnorm, args = c(205.39, 2.94), col='green')
#azul es la posterior
#roja la previa
#verde una aproximacion normal a la posterior basada en los
#estimadores de maxima verosimilitud :) suponiendo que la distribucion limite es normal



z=out$beta[,1]
z=as.data.frame(z)

p1=ggplot(z, aes(x=z))+
  geom_histogram(fill='blue')+
  labs(title = TeX(r"(\textbf{Histograma de $\beta_1$}.)"),
       y = "Frecuencia", x = TeX(r"(\textbf{$\beta_1$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))

p1
z=out$beta[,2]
z=as.data.frame(z)
p2=ggplot(z, aes(x=z))+
  geom_histogram(fill='green4')+
  labs(title = TeX(r"(\textbf{Histograma de $\beta_2$}.)"),
       y = "Frecuencia", x = TeX(r"(\textbf{$\beta_2$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
p2

z=out$beta[,3]
p3=ggplot(z, aes(x=z))+
  geom_histogram(fill='orange')+
  labs(title = TeX(r"(\textbf{Histograma de $\beta_3$}.)"),
       y = "Frecuencia", x = TeX(r"(\textbf{$\beta_3$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
p3
z=out$alpha
#z=as.data.frame(z)
p4=ggplot(z, aes(x=z))+
  geom_histogram(fill='red')+
  labs(title = TeX(r"(\textbf{Histograma de $\alpha$}.)"),
       y = "Frecuencia", x = TeX(r"(\textbf{$\alpha$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
library(patchwork) #to interact with ggplot objects

p4
p4+p1+p2+p3
#Bien!!!



z=out$tau
#z=as.data.frame(z)
pp=ggplot(data.frame(z), aes(x=z))+
  geom_histogram(fill='goldenrod')+
  labs(title = TeX(r"(\textbf{Histograma de $\tau|X,\underline{y}$}.)"),
       y = "Frecuencia", x = TeX(r"(\textbf{$\tau$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
pp



#Predicciones


out$yf2[,1]
out$yf2
pred1=out$yf2[,1]
pred2=out$yf2[,2]
pred3=out$yf2[,3]
pr1=ggplot(as.data.frame(pred1), aes(x=pred1))+
  geom_density(size=2)+
  labs(title = TeX(r"(\textbf{Distribución predictiva final para $Y_{F_1}|\underline{x}_{F_1},X,\underline{y}$}.)"),
       y = "Density", x = TeX(r"(\textbf{$Y_{F_1}$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
pr1
pr2=ggplot(as.data.frame(pred2), aes(x=pred2))+
  geom_density(size=2, col='red')+
  labs(title = TeX(r"(\textbf{Distribución predictiva final para $Y_{F_2}|\underline{x}_{F_2}, X,\underline{y}$}.)"),
       y = "Density", x = TeX(r"(\textbf{$Y_{F_2}$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
pr3=ggplot(as.data.frame(pred3), aes(x=pred3))+
  geom_density(size=2, col='green4')+
  labs(title = TeX(r"(\textbf{Distribución predictiva final para $Y_{F_3}|\underline{x}_{F_3},X,\underline{y}$}.)"),
       y = "Density", x = TeX(r"(\textbf{$Y_{F_3}$}.)"))+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(hjust = 0.5))
pr3
pr1+pr2+pr3







#Pseudo-Rcuadrada
datos[,1]

out$yf1


out.sum<-ej4.sim$BUGSoutput$summary
out.sum
out.sum[7,]
out.sum[7+23,]
out.sum[1,]
#vamos a comparar esto con las beta gorro

xf[1,]
out.sum[2:4,1]

#a ver la prediccion para la primera...
#muy cercanaaa yf2[1]   34.082215
out.sum[1,1]+xf[1,]%*%out.sum[2:4,1]

out<-ej4.sim$BUGSoutput$sims.list
out$beta#la primera columna es la cadena de beta 1 la segunda es la cadena de beta2

out$yf1
hist(out$yf1)
plot(density(out$yf1))

#JAGS
out<-ej4.sim$BUGSoutput$sims.list
out
out$yf1[,23]

out.sum[7:(7+23),1]
datos[,1]
R2<-(cor(datos[,1],out.sum[7:(7+23),1]))^2
R2


cor(out$alpha, out$beta[,1])
cor(out$alpha, out$beta[,3])