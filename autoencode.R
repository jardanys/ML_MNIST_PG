
# Keras

library(autoencoder)

load("gatosperros.RData")

plotcd <- function(v){
  x <- matrix(v,64,64)
  image(1:65,1:65,t(apply(x,2,rev)),asp=1,xaxt="n",yaxt="n",
        col=grey((0:255)/255),ann=FALSE,bty="n")
}

dim(dm)

auto_encode <- autoencode(dm, unit.type = "logistic", N.hidden = c(5,4,3,2,3,4,5), nl = 9, 
           epsilon = 0.001, lambda = 0.0002, beta = 3, rho = 0.01)

?autoencode

auto_encode_9 <- autoencode(dm, unit.type = "logistic", N.hidden = c(4,3,2,3,4), nl = 7, 
                          epsilon = 0.001, lambda = 0.0002, beta = 6, rho = 0.01)

predicion_autoencode <- predict.autoencoder(auto_encode, dm)
predicion_autoencode_9 <- predict.autoencoder(auto_encode_9, dm)

dm
predicion_autoencode$X.output
predicion_autoencode$hidden.output
predicion_autoencode$mean.error

plotcd(dm[3,])
plotcd(predicion_autoencode_9$X.output[3,])
dim(predicion_autoencode$X.output)
dim(dm)

plotcd(dm[3,])
plotcd(predicion_autoencode$X.output[3,])