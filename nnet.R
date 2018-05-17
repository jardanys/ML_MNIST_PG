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
pc <- prcomp(train, scale= T)
matriz.proyec <- pc$rotation
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")

y <- c(rep(1, 90), rep(0, 90))
x <- pc$x[,c(1:10)]

x_test <- predict(pc, test)
y_test <- c(rep(1,9), rep(0,9))

n_net <- nnet(y=y, x=x, size=4)

predict.nnet <- predict(n_net, x_test[, c(1:10)])

resultado <- data.frame(y_test=y_test, y_predict=predict.nnet)

resultado$predict <- round(resultado$y_predict)

resultado$resid <- ifelse(resultado$predict==resultado$y_test, 0, 1)

sum(resultado$resid) / length(resultado$resid)


