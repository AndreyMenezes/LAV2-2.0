# Script  de analise do agrupamento
# Iury Gregory Melo Ferreira - versao 2.0 (Fevereiro 2013)

library(fpc)
library(FNN)


dados <- read.csv("dados/TabelaParaPerfisNormal.csv",header=T)

distancias <- as.data.frame(knn.dist(dados[,-1],k=5,algorithm=c("kd_tree")))

media.distancias <- as.data.frame(rowMeans(distancias))


dados <- cbind(dados,media.distancias)
colnames(dados) <- c("matricula","mediana.sessao","nota.final.pratica","atividade","distancias")
dados <- dados[order(dados$distancia,decreasing = F),]

png("grafico das distancias.png",bg="transparent")
plot(dados$distancia)
dev.off()

agrupamento1 <- kmeans(d,3)
agrupamento2 <- kmeans(d,4)
agrupamento3 <- kmeans(d,5)
agrupamento4 <- kmeans(d,6)


png("AgrupamentoK3-2.png",bg="transparent",width = 800, height = 600)
plot(d$nota.final.pratica,d$atividade, col = agrupamento1$cluster)
points(agrupamento1$centers, col = 1:2, pch = 8, cex=2)
dev.off()

#png("AgrupamentoK4-2.png",bg="transparent",width = 800, height = 600)
#plot(d$nota.final.pratica,d$atividade, col = agrupamento2$cluster)
#points(agrupamento2$centers, col = 1:2, pch = 8, cex=2)
#dev.off()

#png("AgrupamentoK5-2.png",bg="transparent",width = 800, height = 600)
#plot(d$nota.final.pratica,d$ativiade, col = agrupamento3$cluster)
#points(agrupamento3$centers, col = 1:2, pch = 8, cex=2)
#dev.off()

#png("AgrupamentoK6-2.png",bg="transparent",width = 800, height = 600)
#plot(d$nota.final.pratica,d$atividade, col = agrupamento4$cluster)
#points(agrupamento4$centers, col = 1:2, pch = 8, cex=2)
#dev.off()

d2 <- dados[,c(-1,-5)]
db <- dbscan(d2,0.2359355,MinPts=5,method=c("hybrid"))

png("AgrupamentoDBSCAN.png",bg="transparent",width = 800, height = 600)
plot(db,d2)
dev.off()