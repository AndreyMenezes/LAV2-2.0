# Script para analisar a relação entre número de sessões e tempo total de estudo.
# Iury Gregory - Versão 2.0 (Fevereiro 2013)

require(nortest)
require(ggplot2)

#leitura dos dados
dados <- read.csv("dados/tableSessionLength.csv",header=T)

dados.numSessoes <- with(dados,aggregate(session,list(matricula),FUN=max))
colnames(dados.numSessoes) <- c("matricula","num.sessoes")
dados.tempoEstudo <- with(dados,aggregate(timeSession/3600,list(matricula),FUN=sum))
colnames(dados.tempoEstudo) <- c("matricula","tempo.estudo")

#merge para associar o tempo de estudo e o numero de sessoes dos alunos.
tabela <- merge(dados.numSessoes,dados.tempoEstudo,by.x="matricula",by.y="matricula")



#testes de normalidade para os dados
print( ad.test(tabela$num.sessoes))
print( shapiro.test(tabela$num.sessoes))

print( ad.test(tabela$tempo.estudo))
print( shapiro.test(tabela$tempo.estudo))

#teste de correlação
print( cor.test(tabela$num.sessoes,tabela$tempo.estudo,method=c("kendall")))
print( cor.test(tabela$num.sessoes,tabela$tempo.estudo,method=c("spearman")))


#grafico para analise dos dados de tempo de estudo.
png(filename = "US9-TempoEstudo.png", width=650)
par(mfrow=c(1,2))
hist(tabela$tempo.estudo, main="Histograma Tempo de Estudo", xlab="Tempo de Estudo (hora)", ylab="Frequencia")
qqnorm(tabela$tempo.estudo)
qqline(tabela$tempo.estudo)
dev.off()


#grafico para analise dos dados do numero de sessoes.
png(filename = "US9-NumeroDeSessoes.png", width=650)
par(mfrow=c(1,2))
hist(tabela$num.sessoes, main="Histograma Numero de Sessoes ", xlab="Numero de Sessoes de Estudo", ylab="Numero de Alunos")
qqnorm(tabela$num.sessoes)
qqline(tabela$num.sessoes)
dev.off()


#grafico log log para mostrar a correlacao entre os dados.
grafico <- ggplot(tabela,aes(num.sessoes,tempo.estudo)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) +
	     scale_y_log10() + scale_x_log10() +
	     theme_bw()+labs(x="Numero de Sessões (log)",y="Tempo Total de Estudo (log)") +  
	     theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
	     theme(axis.ticks = element_blank()) 

png(filename = "ScatterplotUS9.png", width = 480, height = 480)
print(grafico)
dev.off()

