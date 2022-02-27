library(kohonen)

set.seed(6040)
sample.size <- 10000

sample.rgb <- as.data.frame(matrix(nrow = sample.size, ncol = 3))
colnames(sample.rgb) <- c('R', 'G', 'B')

sample.rgb$R <- sample(0:255, sample.size, replace = T)
sample.rgb$G <- sample(0:255, sample.size, replace = T)
sample.rgb$B <- sample(0:255, sample.size, replace = T)


grid.size <- ceiling(sample.size ^ (1/2.5))
# topo - topology of the map, rectangular, hexagonal - the mapping preserves the relative distance between the points, hexagonal has 6 neighbours so preserves the topology better
# toroidal - the ends of the map are connected
som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', toroidal = F)

som.model <- som(data.matrix(sample.rgb), grid = som.grid, rlen = 500)
som.events <- som.model$codes[[1]]
som.events.colors <- rgb(som.events[,1], som.events[,2], som.events[,3], maxColorValue = 255)


init_codes <- replicate(3, runif(grid.size * grid.size))
som.model.init <- som(data.matrix(sample.rgb), grid = som.grid, init=init_codes, rlen = 500, alpha=c(0.05,0.01))


plot(som.model,
     type = 'mapping',
     bg = som.events.colors[sample.int(length(som.events.colors), size = length(som.events.colors))],
     col = NA,
     main = '')

# plot(som.model, type='codes', palette.name = rainbow, main='Fan Map')

plot(som.model, type='changes')

plot(som.model,
     type = 'mapping',
     bg = som.events.colors,
     keepMargins = F,
     col = NA)



plotSOM <- function(clusters){
        plot(som.model,
             type = 'mapping',
             bg = som.events.colors,
             keepMargins = F,
             col = NA)
        
        add.cluster.boundaries(som.model, clusters)
}

k <- 20

som.cluster.k <- kmeans(som.events, centers = k, iter.max = 100)$cluster

som.dist <- dist(som.events)
som.cluster.h <- cutree(hclust(som.dist), k = k)

plotSOM(som.cluster.k)
plotSOM(som.cluster.h)