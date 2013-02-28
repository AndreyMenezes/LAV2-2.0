# Script de agrupamento
# Iury Gregory Melo Ferreira - versao 2.0 (Fevereiro 2013)
#k = 3,4,5 e 6

dados <- read.csv("dados/TabelaParaPerfis.csv",header=T)

agrupamento1 <- kmeans(dados,3)
agrupamento2 <- kmeans(dados,4)
agrupamento3 <- kmeans(dados,5)
agrupamento4 <- kmeans(dados,6)


png("AgrupamentoK3.png",bg="transparent",width = 800, height = 600)
plot(dados$nota.final.pratica,dados$tempo.total.estudo, col = agrupamento1$cluster)
points(agrupamento1$centers, col = 1:2, pch = 8, cex=2)
dev.off()

png("AgrupamentoK4.png",bg="transparent",width = 800, height = 600)
plot(dados$nota.final.pratica,dados$tempo.total.estudo, col = agrupamento2$cluster)
points(agrupamento2$centers, col = 1:2, pch = 8, cex=2)
dev.off()

png("AgrupamentoK5.png",bg="transparent",width = 800, height = 600)
plot(dados$nota.final.pratica,dados$tempo.total.estudo, col = agrupamento3$cluster)
points(agrupamento3$centers, col = 1:2, pch = 8, cex=2)
dev.off()

png("AgrupamentoK6.png",bg="transparent",width = 800, height = 600)
plot(dados$nota.final.pratica,dados$tempo.total.estudo, col = agrupamento4$cluster)
points(agrupamento4$centers, col = 1:2, pch = 8, cex=2)
dev.off()

