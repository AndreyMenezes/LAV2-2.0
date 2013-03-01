#Script para criacao de tabela para analisar agrupamento e perfis
#Iury Gregory Melo Ferreira - versão 2.0 (Fevereiro 2013)

library(lattice)
library(plyr)

dados.atividade <- read.csv("dados/AgrupamentoAtividade.csv",header=T)
dados.atividade <- dados.atividade[,-1]
dados.tempo <- read.csv("dados/tableSumDiscipline.csv",header=T)
dados.sessao <- read.csv("dados/TableSessionLength.csv",header=T)
dados.geral <- read.csv("dados/Geral.csv",header=T)
dados.exercicios <- read.csv("dados/exercicios-20112.csv",header=F)

colnames(dados.exercicios) <- c("matricula","questao","turma","dataHora","submissao","nota")
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


##############################################################################

#Script para eliminacao de variaveis correlatas e criacao de tabela para 
# analise dos perfis.

##############################################################################


#grafico da relacao das variaveis
png("Relacao entre variaveis.png",bg="transparent",width=800, height=800)
splom(dados[,-1])
dev.off()

#analise de correlacao
K1 <- cor.test(dados$mediana.sessao, dados$nota.final.pratica ,method=c("kendall"))
K2 <- cor.test(dados$mediana.sessao, dados$num.exercicios ,method=c("kendall"))
K3 <- cor.test(dados$mediana.sessao, dados$tempo.total.estudo ,method=c("kendall"))
K4 <- cor.test(dados$mediana.sessao, dados$atividade ,method=c("kendall"))
K5 <- cor.test(dados$nota.final.pratica, dados$num.exercicios ,method=c("kendall"))
K6 <- cor.test(dados$nota.final.pratica, dados$tempo.total.estudo ,method=c("kendall"))
K7 <- cor.test(dados$nota.final.pratica, dados$atividade ,method=c("kendall"))
K8 <- cor.test(dados$num.exercicios, dados$tempo.total.estudo ,method=c("kendall"))
K9 <- cor.test(dados$num.exercicios, dados$atividade ,method=c("kendall"))
K10 <- cor.test(dados$tempo.total.estudo, dados$atividade ,method=c("kendall"))

S1 <- cor.test(dados$mediana.sessao, dados$nota.final.pratica ,method=c("spearman"))
S2 <- cor.test(dados$mediana.sessao, dados$num.exercicios ,method=c("spearman"))
S3 <- cor.test(dados$mediana.sessao, dados$tempo.total.estudo ,method=c("spearman"))
S4 <- cor.test(dados$mediana.sessao, dados$atividade ,method=c("spearman"))
S5 <- cor.test(dados$nota.final.pratica, dados$num.exercicios ,method=c("spearman"))
S6 <- cor.test(dados$nota.final.pratica, dados$tempo.total.estudo ,method=c("spearman"))
S7 <- cor.test(dados$nota.final.pratica, dados$atividade ,method=c("spearman"))
S8 <- cor.test(dados$num.exercicios, dados$tempo.total.estudo ,method=c("spearman"))
S9 <- cor.test(dados$num.exercicios, dados$atividade ,method=c("spearman"))
S10 <- cor.test(dados$tempo.total.estudo, dados$atividade ,method=c("spearman"))


tabela.cor <- data.frame(Variaveis = c("mediana e notafinal","mediana e numExercicios",
"mediana e tempoEstudo", "mediana e atividade", "notafinal e numExercicios",
"notafinal e tempoEstudo", "notafinal e atividade", "numExercicios e tempoEstudo",
"numExercicios e atividade", "tempoEstudo e atividade"),
	     Kendal = c(K1$estimate,K2$estimate,K3$estimate,K4$estimate,K5$estimate,
K6$estimate,K7$estimate,K8$estimate,K9$estimate,K10$estimate),
	     Spearman = c(S1$estimate,S2$estimate,S3$estimate,S4$estimate,S5$estimate,
S6$estimate,S7$estimate,S8$estimate,K9$estimate,S10$estimate),
		Tipo = c("nao ha","nao ha","nao ha","nao ha","forte","nao ha",
"moderada","moderada","moderada","forte"))



tabela <- subset(dados,select=c("matricula","mediana.sessao","nota.final.pratica","atividade"))
tabela$mediana.sessao <- (tabela$mediana.sessao - min(tabela$mediana.sessao)) / (max(tabela$mediana.sessao) - min(tabela$mediana.sessao))
tabela$nota.final.pratica <- (tabela$nota.final.pratica - min(tabela$nota.final.pratica)) / (max(tabela$nota.final.pratica) - min(tabela$nota.final.pratica))
tabela$atividade <- (tabela$atividade - min(tabela$atividade)) / (max(tabela$atividade) - min(tabela$atividade))

wirte.table(tabela.cor,"dados/tabelaCorrelacaoPerfis.csv",sep=",",row.names=F,col.names=T)
write.table(tabela,"dados/TabelaParaPerfisNormal.csv",sep=",",row.names=F,col.names=T)





