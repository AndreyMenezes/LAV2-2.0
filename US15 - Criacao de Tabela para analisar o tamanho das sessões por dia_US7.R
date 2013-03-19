#US15
# US7 - Criacao de Tabela para analisar o tamanho das sessões por dia.
# Iara Ribeiro - versao 2.0 (Fevereiro 2013)

data.aula <- read.csv("dados/TableSessionLengthEmAula.csv", header = T)
data.aula <- data.aula[data.aula$timeSession != 0, ]

#data.aulas das provas da disciplina
data.aula.prova1 <- "2011-09-17"
data.aula.prova2 <- "2011-10-29"
data.aula.prova3 <- "2011-11-26"

for(i in seq(1:nrow(data.aula))){
  data.aula[i,"data"] <- toString(data.frame(strsplit(toString(data.aula[i,"data.hora"]), " "))[1,])
}

tam.sessoes <- cbind(data.aula[,"timeSession"],data.aula[,"data"], strptime(data.aula[,"data"],"%m/%d/%Y"))
tam.sessoes[,2] <- as.Date(strptime(tam.sessoes[,2],"%m/%d/%Y"))
tam.sessoes[,3] <- strptime(tam.sessoes[,3],"%m/%d/%Y")
time.stamp.prova1 <- tam.sessoes[tam.sessoes[,3] == data.aula.prova1, ][1,2]
time.stamp.prova2 <- tam.sessoes[tam.sessoes[,3] == data.aula.prova2, ][1,2]
time.stamp.prova3 <- tam.sessoes[tam.sessoes[,3] == data.aula.prova3, ][1,2]

colnames(tam.sessoes) <- c("tam.sessao","time.stamp","date")
write.csv(tam.sessoes, "dados/tam_sessoes_tempo_em_aula.csv")


data.fora <- read.csv("dados/TableSessionLengthForadeAula.csv", header = T)
data.fora <- data.fora[data.fora$timeSession != 0, ]

#data.foras das provas da disciplina
data.fora.prova1 <- "2011-09-17"
data.fora.prova2 <- "2011-10-29"
data.fora.prova3 <- "2011-11-26"

for(i in seq(1:nrow(data.fora))){
  data.fora[i,"data.fora"] <- toString(data.frame(strsplit(toString(data.fora[i,"data.fora.hora"]), " "))[1,])
}

tam.sessoes.fora <- cbind(data.fora[,"timeSession"],data.fora[,"data.fora"], data.fora[,"data.fora"])
tam.sessoes.fora[,2] <- as.Date(tam.sessoes.fora[,2])

time.stamp.prova1 <- tam.sessoes.fora[tam.sessoes.fora[,3] == data.fora.prova1, ][1,2]
time.stamp.prova2 <- tam.sessoes.fora[tam.sessoes.fora[,3] == data.fora.prova2, ][1,2]
time.stamp.prova3 <- tam.sessoes.fora[tam.sessoes.fora[,3] == data.fora.prova3, ][1,2]

colnames(tam.sessoes.fora) <- c("tam.sessao","time.stamp","date")
write.csv(tam.sessoes.fora, "dados/tam_sessoes_tempo_fora_aula.csv")
