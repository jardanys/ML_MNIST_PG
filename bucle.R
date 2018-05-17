library(nnet)


load("gatosperros.RData")

plotcd <- function(v){
  x <- matrix(v,64,64)
  image(1:65,1:65,t(apply(x,2,rev)),asp=1,xaxt="n",yaxt="n",
        col=grey((0:255)/255),ann=FALSE,bty="n")
}

# Entradas Train & Test

set.seed(7931)
ind.gatostest <- sample(1:99, 9, replace = FALSE)
gatos.test <- dm[ind.gatostest, ]
gatos.train <- dm[1:99, ][-ind.gatostest, ]

ind.perrostest <- sample(1:99, 9, replace = FALSE)
perros.test <- dm[100:198, ][ind.perrostest, ]
perros.train <- dm[100:198, ][-ind.perrostest, ]


#plotcd(round(centro.gatos))
train <- rbind(gatos.train, perros.train)
test <- rbind(gatos.test, perros.test)

centro.train <- colMeans(train)

train <- train - matrix(centro.train,nrow(train),ncol(train),byrow=TRUE)
test <- test - matrix(centro.train,nrow(test),ncol(test),byrow=TRUE)

### Generación de PCA

set.seed(7931)
pc <- prcomp(train, center = F)
matriz.proyec <- pc$rotation
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")
y <- c(rep(1, 90), rep(0, 90))
y_test <- c(rep(1,9), rep(0,9))
x_test <- predict(pc, test)
componentes <- c(2:180)
neuronas <- c(1:10)
z <- 1
res <- data.frame(iteraciones=0, componentes=1, neuronas=1, err=1)
library(progress)
pb <- progress_bar$new(total = 50)
for(i in componentes){
  for(j in neuronas){
    x <- pc$x[,c(1:i)]
    n_net <- nnet(y=y, x=x, size=j, trace=F)
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
ggplot(res, aes(x=iteraciones, y=err, colour="resid test")) + geom_line() +
  ylab("Resid Train & Test") + labs(title = "Total Redes Neuronales", 
                                    subtitle = "Resid Test")



