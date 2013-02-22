require(nortest)
require(ggplot2)

dados.atividade <- read.csv("dados/AgrupamentoAtividade.csv")
dados.atividade <- dados.atividade[,-1]
dados.sessao <- read.csv("dados/tableSumDiscipline.csv")

dados <- merge(dados.sessao,dados.atividade,by.x="matricula",by.y="matricula")
dados$sumSession <- dados$sumSession/3600

print( ad.test(dados$sumSession))
print( shapiro.test(dados$sumSession))

print( ad.test(dados$atividade))
print( shapiro.test(dados$atividade))

print( cor.test(dados$sumSession,dados$atividade,method=c("kendall")))
print( cor.test(dados$sumSession,dados$atividade,method=c("spearman")))

png(filename = "US6-TamanhoSessoes.png", width=650)
par(mfrow=c(1,2))
hist(dados$sumSession, main="Histograma Tamanho Sessoes", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(dados$sumSession)
qqline(dados$sumSession)
dev.off()

png(filename = "US6-AtividadeEstudo.png", width=650)
par(mfrow=c(1,2))
hist(dados$atividade, main="Histograma Atividade", xlab="Atividade de Estudo", ylab="Frequencia")
qqnorm(dados$atividade)
qqline(dados$atividade)
dev.off()


grafico <- ggplot(dados,aes(sumSession,atividade)) + 
 	     geom_point() + geom_smooth(method=lm,se=FALSE) +
	     scale_x_log10() + scale_y_log10() +
	     theme_bw()+labs(x="Tempo Total de Estudo (log)",y="Atividade de Estudo (log)") +  
	     theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
	     theme(axis.ticks = element_blank()) 

png(filename = "ScatterplotUS6.png", width = 480, height = 480)
print(grafico)
dev.off()

