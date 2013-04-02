# Script para analisar a relação entre o tempo total de estudo e atividade de estudo.
# Iury Gregory Melo Ferreira - Versão 2.0 (Fevereiro 2013)

require(nortest)
require(ggplot2)

#leitura dos dados.aula
dados.aula.atividade.aula <- read.csv("dados/AgrupamentoAtividadeEmAula.csv")
dados.aula.atividade.aula <- dados.aula.atividade.aula[,-1] #remocao das colunas nao necessarias
dados.aula.sessao.aula <- read.csv("dados/tableSumDisciplineEmAula.csv")

#merge para associar para cada aluno seu tempo total de estudo e seu tempo de atividade.
dados.aula <- merge(dados.aula.sessao.aula,dados.aula.atividade.aula,by.x="matricula",by.y="matricula")
dados.aula$sumSession <- dados.aula$sumSession/3600


#testes de normalidade para os dados.aula
print( ad.test(dados.aula$sumSession))
print( shapiro.test(dados.aula$sumSession))

print( ad.test(dados.aula$atividade))
print( shapiro.test(dados.aula$atividade))


#teste de correlação
print( cor.test(dados.aula$sumSession,dados.aula$atividade,method=c("kendall")))
print( cor.test(dados.aula$sumSession,dados.aula$atividade,method=c("spearman")))

#grafico para analise dos dados.aula de tempo total de estudo
png(filename = "US6-TempoTotalEstudo-EmAula.png", width=650)
par(mfrow=c(1,2))
hist(dados.aula$sumSession, main="Histograma Tempo Total de Estudo", xlab="Tempo Total de Estudo", ylab="Frequencia")
qqnorm(dados.aula$sumSession)
qqline(dados.aula$sumSession)
dev.off()

#grafico para analise dos dados.aula da atividade de estudo.
png(filename = "US6-AtividadeEstudo-EmAula.png", width=650)
par(mfrow=c(1,2))
hist(dados.aula$atividade, main="Histograma Atividade", xlab="Atividade de Estudo", ylab="Frequencia")
qqnorm(dados.aula$atividade)
qqline(dados.aula$atividade)
dev.off()

#grafico log log para mostrar a correlacao entre os dados.aula.
grafico <- ggplot(dados.aula,aes(sumSession,atividade)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(c(0,1)) +xlim(c(0,100)) +
	     scale_x_log10() + scale_y_log10(limits=c(0.01,1)) +
	     theme_bw()+labs(x="Tempo Total de Estudo (log)",y="Atividade de Estudo (log)") 

png(filename = "ScatterplotUS6-EmAula.png", width = 480, height = 480)
print(grafico)
dev.off()

max(dados.aula$atividade)
#FORA HORARIO DE AULA#

#leitura dos dados.fora
dados.atividade.fora <- read.csv("dados/AgrupamentoAtividadeForadeAula.csv")
dados.atividade.fora <- dados.atividade.fora[,-1] #remocao das colunas nao necessarias
dados.sessao.fora <- read.csv("dados/tableSumDisciplineForadeAula.csv")

#merge para associar para cada aluno seu tempo total de estudo e seu tempo de atividade.
dados.fora <- merge(dados.sessao.fora,dados.atividade.fora,by.x="matricula",by.y="matricula")
dados.fora$sumSession <- dados.fora$sumSession/3600


#testes de normalidade para os dados.fora
print( ad.test(dados.fora$sumSession))
print( shapiro.test(dados.fora$sumSession))

print( ad.test(dados.fora$atividade))
print( shapiro.test(dados.fora$atividade))


#teste de correlação
print( cor.test(dados.fora$sumSession,dados.fora$atividade,method=c("kendall")))
print( cor.test(dados.fora$sumSession,dados.fora$atividade,method=c("spearman")))

#grafico para analise dos dados.fora de tempo total de estudo
png(filename = "US6-TempoTotalEstudo-ForaAula.png", width=650)
par(mfrow=c(1,2))
hist(dados.fora$sumSession, main="Histograma Tempo Total de Estudo Fora de Aula", xlab="Tempo Total de Estudo", ylab="Frequencia")
qqnorm(dados.fora$sumSession)
qqline(dados.fora$sumSession)
dev.off()

#grafico para analise dos dados.fora da atividade de estudo.
png(filename = "US6-AtividadeEstudo-ForaDeAula.png", width=650)
par(mfrow=c(1,2))
hist(dados.fora$atividade, main="Histograma Atividade Fora de Aula", xlab="Atividade de Estudo", ylab="Frequencia")
qqnorm(dados.fora$atividade)
qqline(dados.fora$atividade)
dev.off()

#grafico log log para mostrar a correlacao entre os dados.fora.
grafico2 <- ggplot(dados.fora,aes(sumSession,atividade)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
	     scale_x_log10() + scale_y_log10(limits=c(0.01,1)) + 
	     labs(x="Tempo Total de Estudo (log)",y="Atividade de Estudo (log)")

png(filename = "ScatterplotUS6-ForaDeAula.png", width = 480, height = 480)
print(grafico2)
dev.off()
