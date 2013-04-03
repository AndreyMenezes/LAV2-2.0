# US7 - Criacao de Tabela para analisar o tamanho das sessões por dia.
# Iara Ribeiro - versao 2.0 (Fevereiro 2013)

data <- read.csv("dados/TableSessionLength.csv", header = T)
data <- data[data$timeSession != 0, ]

#Datas das provas da disciplina
data.prova1 <- "2011-09-17"
data.prova2 <- "2011-10-29"
data.prova3 <- "2011-11-26"

for(i in seq(1:nrow(data))){
  data[i,6] <- toString(data.frame(strsplit(toString(data[i,4]), " "))[1,])
}

tam.sessoes <- cbind(data[,3],data[,6], data[,6])
tam.sessoes[,2] <- as.Date(tam.sessoes[,2])

time.stamp.prova1 <- tam.sessoes[tam.sessoes[,3] == data.prova1, ][1,2]
time.stamp.prova2 <- tam.sessoes[tam.sessoes[,3] == data.prova2, ][1,2]
time.stamp.prova3 <- tam.sessoes[tam.sessoes[,3] == data.prova3, ][1,2]

colnames(tam.sessoes) <- c("tam.sessao","time.stamp","date")
write.csv(tam.sessoes, "dados/tam_sessoes_tempo.csv")
