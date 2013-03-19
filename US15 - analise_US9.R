#SCRIPT US15 analise US9

require(nortest)
require(ggplot2)

#leitura dos dados.aula
dados.aula <- read.csv("dados/TableSessionLengthEmAula.csv",header=T)

dados.aula.numSessoes <- with(dados.aula,aggregate(session,list(matricula),FUN=max))
colnames(dados.aula.numSessoes) <- c("matricula","num.sessoes")
dados.aula.tempoEstudo <- with(dados.aula,aggregate(timeSession/3600,list(matricula),FUN=sum))
colnames(dados.aula.tempoEstudo) <- c("matricula","tempo.estudo")

#merge para associar o tempo de estudo e o numero de sessoes dos alunos.
tabela.aula <- merge(dados.aula.numSessoes,dados.aula.tempoEstudo,by.x="matricula",by.y="matricula")



#testes de normalidade para os dados.aula
print( ad.test(tabela.aula$num.sessoes))
print( shapiro.test(tabela.aula$num.sessoes))

print( ad.test(tabela.aula$tempo.estudo))
print( shapiro.test(tabela.aula$tempo.estudo))

#teste de correlação
print( cor.test(tabela.aula$num.sessoes,tabela.aula$tempo.estudo,method=c("kendall")))
print( cor.test(tabela.aula$num.sessoes,tabela.aula$tempo.estudo,method=c("spearman")))


#grafico para analise dos dados.aula de tempo de estudo.
png(filename = "US9-TempoEstudo-EmAula.png", width=650)
par(mfrow=c(1,2))
hist(tabela.aula$tempo.estudo, main="Histograma Tempo de Estudo", xlab="Tempo de Estudo (hora)", ylab="Frequencia")
qqnorm(tabela.aula$tempo.estudo)
qqline(tabela.aula$tempo.estudo)
dev.off()


#grafico para analise dos dados.aula do numero de sessoes.
png(filename = "US9-NumeroDeSessoes-EmAula.png", width=650)
par(mfrow=c(1,2))
hist(tabela.aula$num.sessoes, main="Histograma Numero de Sessoes ", xlab="Numero de Sessoes de Estudo", ylab="Numero de Alunos")
qqnorm(tabela.aula$num.sessoes)
qqline(tabela.aula$num.sessoes)
dev.off()


#grafico log log para mostrar a correlacao entre os dados.aula.
grafico <- ggplot(tabela.aula,aes(num.sessoes,tempo.estudo)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) +
	     scale_y_log10() + scale_x_log10() +
	     theme_bw()+labs(x="Numero de Sessões (log)",y="Tempo Total de Estudo (log)") +  
	     theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
	     theme(axis.ticks = element_blank()) 

png(filename = "ScatterplotUS9-EmAula.png", width = 480, height = 480)
print(grafico)
dev.off()




######################
#FORA HORARIO DE AULA#
######################


dados.fora <- read.csv("dados/TableSessionLengthForadeAula.csv",header=T)
dados.fora.numSessoes <- with(dados.fora,aggregate(session,list(matricula),FUN=max))
colnames(dados.fora.numSessoes) <- c("matricula","num.sessoes")
dados.fora.tempoEstudo <- with(dados.fora,aggregate(timeSession/3600,list(matricula),FUN=sum))
colnames(dados.fora.tempoEstudo) <- c("matricula","tempo.estudo")

#merge para associar o tempo de estudo e o numero de sessoes dos alunos.
tabela.fora <- merge(dados.fora.numSessoes,dados.fora.tempoEstudo,by.x="matricula",by.y="matricula")



#testes de normalidade para os dados.fora
print( ad.test(tabela.fora$num.sessoes))
print( shapiro.test(tabela.fora$num.sessoes))

print( ad.test(tabela.fora$tempo.estudo))
print( shapiro.test(tabela.fora$tempo.estudo))

#teste de correlação
print( cor.test(tabela.fora$num.sessoes,tabela.fora$tempo.estudo,method=c("kendall")))
print( cor.test(tabela.fora$num.sessoes,tabela.fora$tempo.estudo,method=c("spearman")))


#grafico para analise dos dados.fora de tempo de estudo.
png(filename = "US9-TempoEstudo-ForadeAula.png", width=650)
par(mfrow=c(1,2))
hist(tabela.fora$tempo.estudo, main="Histograma Tempo de Estudo", xlab="Tempo de Estudo (hora)", ylab="Frequencia")
qqnorm(tabela.fora$tempo.estudo)
qqline(tabela.fora$tempo.estudo)
dev.off()


#grafico para analise dos dados.fora do numero de sessoes.
png(filename = "US9-NumeroDeSessoes-ForadeAula.png", width=650)
par(mfrow=c(1,2))
hist(tabela.fora$num.sessoes, main="Histograma Numero de Sessoes ", xlab="Numero de Sessoes de Estudo", ylab="Numero de Alunos")
qqnorm(tabela.fora$num.sessoes)
qqline(tabela.fora$num.sessoes)
dev.off()


#grafico log log para mostrar a correlacao entre os dados.fora.
grafico <- ggplot(tabela.fora,aes(num.sessoes,tempo.estudo)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) +
	     scale_y_log10() + scale_x_log10() +
	     theme_bw()+labs(x="Numero de Sessões (log)",y="Tempo Total de Estudo (log)") +  
	     theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
	     theme(axis.ticks = element_blank()) 

png(filename = "ScatterplotUS9-ForadeAula.png", width = 480, height = 480)
print(grafico)
dev.off()