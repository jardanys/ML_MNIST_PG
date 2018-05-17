

# deeplearning, reducción de dimensionalidad

# keras

# Autoencoder

# Tarea 
# Utilice una red neuronal para discriminar sus dos digitos con error similar a SVM
# Reduzca dim gatos y perros a 2 dim usando auto encoder

train_3 <- readRDS("./RDS/Train_3.rds")
train_8 <- readRDS("./RDS/Train_8.rds")

test_3 <- readRDS("./RDS/Test_3.rds")
test_8 <- readRDS("./RDS/Test_8.rds")

train_3_8 <- rbind(train_3, train_8)
test_3_8 <- rbind(test_3, test_8)

pc <- prcomp(train_3_8[,c(1:784)])
pc_test <- predict(pc, test_3_8[,c(1:784)])
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")
x <- pc$x
x_test <- pc_test
y <- as.numeric(as.character(train_3_8[,785]))
y <- ifelse(y==3,1,0)
table(y)
y_test <- as.numeric(as.character(test_3_8[,785]))
y_test <- ifelse(y_test==3,1,0)
table(y_test)
componentes <- c(2:100)
neuronas <- c(1:10)
z <- 0
res <- data.frame(redes=0, componentes=1, neuronas=1, err=1)
library(progress)
pb <- progress_bar$new(total = 100)
for(i in componentes){
  for(j in neuronas){
    x <- pc$x[,c(1:i)]
    n_net <- nnet(y=y, x=x, size=j, trace=F, MaxNWts = 100000)
    predict.nnet <- round(predict(n_net, x_test[, c(1:i)]))
    error <- ifelse(predict.nnet==y_test, 0, 1)
    resid <- sum(error) / length(error)
    z <- z + 1
    res <- rbind(res, c(z, i, j, resid))
  }
  pb$tick()
}

res <- res[-1,]
res$err <- as.numeric(res$err)

min(res$err)

res[which.min(res$err),]

library(ggplot2)
ggplot(res, aes(x=redes, y=err, colour="resid test")) + geom_line() +
  ylab("Resid Train & Test") + labs(title = "Total Redes Neuronales", 
                                    subtitle = "Resid Test")



library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot each model
compo <- res[which.min(res$err),2]
neu <- res[which.min(res$err),3]
x <- pc$x[,c(1:compo)]
n_net <- nnet(y=y, x=x, size=neu, trace=F, MaxNWts = 100000)
plot.nnet(n_net)
wts.in<-n_net$wts
struct<-n_net$n
plot.nnet(wts.in,struct=struct)

