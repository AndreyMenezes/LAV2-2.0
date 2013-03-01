# Script para analisar a relação entre o tempo total de estudo e atividade de estudo.
# Iury Gregory Melo Ferreira - Versão 2.0 (Fevereiro 2013)

require(nortest)
require(ggplot2)

#leitura dos dados
dados.atividade <- read.csv("dados/AgrupamentoAtividade.csv")
dados.atividade <- dados.atividade[,-1] #remocao das colunas nao necessarias
dados.sessao <- read.csv("dados/tableSumDiscipline.csv")

#merge para associar para cada aluno seu tempo total de estudo e seu tempo de atividade.
dados <- merge(dados.sessao,dados.atividade,by.x="matricula",by.y="matricula")
dados$sumSession <- dados$sumSession/3600


#testes de normalidade para os dados
print( ad.test(dados$sumSession))
print( shapiro.test(dados$sumSession))

print( ad.test(dados$atividade))
print( shapiro.test(dados$atividade))


#teste de correlação
print( cor.test(dados$sumSession,dados$atividade,method=c("kendall")))
print( cor.test(dados$sumSession,dados$atividade,method=c("spearman")))

#grafico para analise dos dados de tempo total de estudo
png(filename = "US6-TempoTotalEstudo.png", width=650)
par(mfrow=c(1,2))
hist(dados$sumSession, main="Histograma Tempo Total de Estudo", xlab="Tempo Total de Estudo", ylab="Frequencia")
qqnorm(dados$sumSession)
qqline(dados$sumSession)
dev.off()

#grafico para analise dos dados da atividade de estudo.
png(filename = "US6-AtividadeEstudo.png", width=650)
par(mfrow=c(1,2))
hist(dados$atividade, main="Histograma Atividade", xlab="Atividade de Estudo", ylab="Frequencia")
qqnorm(dados$atividade)
qqline(dados$atividade)
dev.off()

#grafico log log para mostrar a correlacao entre os dados.
grafico <- ggplot(dados,aes(sumSession,atividade)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) +
	     scale_x_log10() + scale_y_log10() +
	     theme_bw()+labs(x="Tempo Total de Estudo (log)",y="Atividade de Estudo (log)") +  
	     theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
	     theme(axis.ticks = element_blank()) 

png(filename = "ScatterplotUS6.png", width = 480, height = 480)
print(grafico)
dev.off()

