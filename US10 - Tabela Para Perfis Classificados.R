#Script para criacao de tabela para analisar agrupamento e perfis
#Iury Gregory Melo Ferreira - versão 2.0 (Fevereiro 2013)

library(plyr)

dados.atividade <- read.csv("dados/AgrupamentoAtividadeEmAula.csv",header=T)
dados.atividade <- dados.atividade[,-1]
dados.tempo <- read.csv("dados/tableSumDisciplineEmAula.csv",header=T)
dados.sessao <- read.csv("dados/TableSessionLengthEmAula.csv",header=T)
dados.geral <- read.csv("dados/Geral.csv",header=T)
dados.exercicios <- read.csv("dados/SubmissoesHorarioDeAula.csv",header=T)

dados.exercicios <- dados.exercicios[,1:2]
dados.exercicios <- dados.exercicios[order(dados.exercicios$matricula,dados.exercicios$questao,decreasing = F),]
dados.exercicios <- unique(dados.exercicios)

dados.numExercicios <- count(dados.exercicios,"matricula")
colnames(dados.numExercicios) <- c("matricula","num.exercicios")
dados.tempo$sumSession <- dados.tempo$sumSession/3600
colnames(dados.tempo) <- c("matricula","tempo.total.estudo")
dados.tamanhoSessao <- with(dados.sessao, aggregate(timeSession,list(matricula),FUN=median))
colnames(dados.tamanhoSessao) <- c("matricula","mediana.sessao")
dados.nota <- subset(dados.geral,nota.final.pratica >= 0,select=c(matricula,nota.final.pratica))

dados <- merge(dados.tamanhoSessao,dados.nota,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.numExercicios,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.tempo,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.atividade,by.x="matricula",by.y="matricula")

write.table(dados,"dados/TabelaParaPerfisEmAula.csv",sep=",",row.names=F,col.names=T)


##############
#FORA DE AULA#
##############


dados.atividade <- read.csv("dados/AgrupamentoAtividadeForadeAula.csv",header=T)
dados.atividade <- dados.atividade[,-1]
dados.tempo <- read.csv("dados/tableSumDisciplineForadeAula.csv",header=T)
dados.sessao <- read.csv("dados/TableSessionLengthForadeAula.csv",header=T)
dados.geral <- read.csv("dados/Geral.csv",header=T)
dados.exercicios <- read.csv("dados/SubmissoesForaHorariodeAula.csv",header=T)

dados.exercicios <- dados.exercicios[,1:2]
dados.exercicios <- dados.exercicios[order(dados.exercicios$matricula,dados.exercicios$questao,decreasing = F),]
dados.exercicios <- unique(dados.exercicios)

dados.numExercicios <- count(dados.exercicios,"matricula")
colnames(dados.numExercicios) <- c("matricula","num.exercicios")
dados.tempo$sumSession <- dados.tempo$sumSession/3600
colnames(dados.tempo) <- c("matricula","tempo.total.estudo")
dados.tamanhoSessao <- with(dados.sessao, aggregate(timeSession,list(matricula),FUN=median))
colnames(dados.tamanhoSessao) <- c("matricula","mediana.sessao")
dados.nota <- subset(dados.geral,nota.final.pratica >= 0,select=c(matricula,nota.final.pratica))

dados <- merge(dados.tamanhoSessao,dados.nota,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.numExercicios,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.tempo,by.x="matricula",by.y="matricula")
dados <- merge(dados,dados.atividade,by.x="matricula",by.y="matricula")

write.table(dados,"dados/TabelaParaPerfisForadeAula.csv",sep=",",row.names=F,col.names=T)

