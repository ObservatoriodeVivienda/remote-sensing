## Para instalar paquetes de BioConductor
# source("https://bioconductor.org/biocLite.R")
# biocLite("biOps")

## Loading libraries
library(jpeg)
library(png)
library(EBImage)
source('lib.R')

# img <- readPNG('test.png')
img <- readJPEG('ortophoto_tizilingo.jpg')
display(img, title = 'Sample')

hist(img)
range(img)

## Negativo de la imagen
display(max(img) - img)

## Para mostrar las diferentes capas
display(img[, , 1])
display(img[, , 2])
display(img[, , 3])
# display(img[, , 4])   ## Esta la agrega Mac cuando capturo pantalla

## Para quedarnos con los colores de cierto umbral
img_crop = img[, , 1]
img_thresh = img_crop < .5
display(img_thresh)

## Prueba fallida para reducir el tamaño de la imagen con Componentes Principales
# library(lattice)
# library(reshape2)
# pc.load <- cbind(pc3$loadings[, 1:3])
# colnames(pc.load) <- c("PC1", "PC2", "PC3")
# pc.df <- melt(pc.load)
# xyplot(value ~ Var1, data = pc.df, group = Var2, type = "l",
#        ylab = "PC Loadings", xlab = "Spectral Bands", 
#        auto.key = list(corner = c(0.98, 0.98), points = FALSE, lines = TRUE),
#        panel = function (x, y, ...) {
#          panel.grid(h = -1, v = -1)
#          panel.xyplot(x, y, ...)
#          panel.abline(h = 0, lty = "dashed")
#        })

m <- 3
n <- 4

## Probando las dimensiones
data.table(
  layer1 = as.numeric(matrix(1:(m*n), ncol = m)),
  layer2 = as.numeric(matrix((1:(m*n))*2, ncol = 3)),
  layer3 = as.numeric(matrix((1:(m*n))*3, ncol = 3)),
  row = rep(1:n, m),
  col = rep(1:m, n)) %>%
  mutate(
    dist.tl = sqrt((1 - row)^2 + (1 - col)^2),
    dist.tr = sqrt((1 - row)^2 + (m - col)^2),
    dist.bl = sqrt((n - row)^2 + (1 - col)^2),
    dist.br = sqrt((n - row)^2 + (m - col)^2),
    dist.c = sqrt((round(n/2) - row)^2 + (round(m/2) - col)^2)) %>%
  data.table

m <- dim(img)[1]
n <- dim(img)[2]

## Probando con la imagen
dataset <- data.table(
    layer1 = as.numeric(img[, , 1]),
    layer2 = as.numeric(img[, , 2]),
    layer3 = as.numeric(img[, , 3]),
    row = rep(1:n, m),
    col = rep(1:m, n)) %>%
  mutate(
    dist.tl = sqrt((1 - row)^2 + (1 - col)^2),
    dist.tr = sqrt((1 - row)^2 + (m - col)^2),
    dist.bl = sqrt((n - row)^2 + (1 - col)^2),
    dist.br = sqrt((n - row)^2 + (m - col)^2),
    dist.c = sqrt((round(n/2) - row)^2 + (round(m/2) - col)^2)) %>%
  select(-row, -col)

dataset.sc <- scale(dataset)

# dataset %>% is.na %>% colSums
# clust <- hclust(dist(dataset))
# plot(clust)

## Clustering
clust <- kmeans(dataset.sc, 20, iter.max = 1000, algorithm = 'MacQueen')

## Des-escalando los centros del cluster
centers <- data.table(t(apply(clust$centers, 1,
    function(r) r * attr(dataset.sc, 'scaled:scale') +
      attr(dataset.sc, 'scaled:center')))) %>%
  setnames(paste0('adj.', names(dataset))) %>%
  mutate(cluster = 1:nrow(.))

## Unimos los centros desescalados con el conjunto de datos original
result <- dataset %>%
  mutate(cluster = clust$cluster) %>%
  join(centers)

## Probando algunos segmentos para ver qué capturan de la imagen
display(matrix(clust$cluster / 20, ncol = m, byrow = T))
display(matrix(clust$cluster == 15, ncol = m, byrow = T))
display(matrix(clust$cluster == 13, ncol = m, byrow = T))
display(matrix(clust$cluster == 9, ncol = m, byrow = T))

## Falta probar como convertir los 3 canales en uno
display((img[, , 1] + img[, , 2] + img[, , 3]) / 3)
