#Script para US5
#Tamanho Sessões x Notas Prova 1,2,3.
require(nortest)

sessoes <- read.csv("tableMeanPerTest.csv")
geral <- read.csv("Geral.csv")
sessoes$meanSessions <- sessoes$meanSessions/3600
sessao1 <- subset(sessoes,Prova == "Prova1",select=c(matricula,meanSessions))
sessao2 <- subset(sessoes,Prova != "Prova3",select=c(matricula,meanSessions))
sessao3 <- subset(sessoes,Prova != "",select=c(matricula,meanSessions))

p1 <- subset(geral, prova1 >= 0 & status != "#VALUE!", select=c(matricula,prova1))
p2 <- subset(geral, prova2 >= 0 & status != "#VALUE!", select=c(matricula,prova2) )
p3 <- subset(geral, prova3.ou.projeto >= 0 & status != "#VALUE!", select=c(matricula,prova3.ou.projeto) )

#sem acumular tamanho da sessao
sessoes.prova1 <- merge(sessao1,p1,by.x="matricula",by.y="matricula")
colnames(sessoes.prova1) <- c("matricula", "tamanhoSessao", "nota.prova1")
sessoes.prova2 <- merge(sessao2,p2,by.x="matricula",by.y="matricula")
colnames(sessoes.prova2) <- c("matricula", "tamanhoSessao", "nota.prova2")
sessoes.prova3 <- merge(sessao3,p3,by.x="matricula",by.y="matricula")
colnames(sessoes.prova3) <- c("matricula", "tamanhoSessao", "nota.prova3")

#acumulada
tabela2 = with(sessao2, aggregate(meanSessions, list(matricula), FUN=sum))
colnames(tabela2) <- c("matricula","somaSessao")
tabela3 = with(sessoes,aggregate(meanSessions,list(matricula),FUN=sum))
colnames(tabela3) <- c("matricula","somaSessao")


#data frame para acumulada
sessoes.acm2 <- merge(tabela2,p2,by.x="matricula",by.y="matricula")
colnames(sessoes.acm2) <- c("matricula", "tamanhoSessaoAcm", "nota.prova2")
sessoes.acm3 <- merge(tabela3,p3,by.x="matricula",by.y="matricula")
colnames(sessoes.acm3) <- c("matricula", "tamanhoSessaoAcm", "nota.prova3")

#SessoesAcm x Prova2
png(filename = "TamanhoSessoesProva2Acm.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.acm2$tamanhoSessaoAcm, main="Histograma Tamanho Sessoes Acm", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(sessoes.acm2$tamanhoSessaoAcm)
qqline(sessoes.acm2$tamanhoSessaoAcm)
dev.off()

#SessoesAcm x Prova 3
png(filename = "TamanhoSessoesProva3Acm.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.acm3$tamanhoSessaoAcm, main="Histograma Tamanho Sessoes Acm", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(sessoes.acm3$tamanhoSessaoAcm)
qqline(sessoes.acm3$tamanhoSessaoAcm)
dev.off()

#Sessoes x Prova1

png(filename = "TamanhoSessoesProva1.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova1$tamanhoSessao, main="Histograma Tamanho Sessoes", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(sessoes.prova1$tamanhoSessao)
qqline(sessoes.prova1$tamanhoSessao)
dev.off()

png(filename = "NotaProva1.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova1$nota.prova1, main="Histograma notas prova1", xlab="Notas prova1", ylab="Frequencia")
qqnorm(sessoes.prova1$nota.prova1)
qqline(sessoes.prova1$nota.prova1)
dev.off()



#Sessoes x Prova2
png(filename = "TamanhoSessoesProva2.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova2$tamanhoSessao, main="Histograma Tamanho Sessoes", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(sessoes.prova2$tamanhoSessao)
qqline(sessoes.prova2$tamanhoSessao)
dev.off()

png(filename = "NotaProva2.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova2$nota.prova2, main="Histograma notas prova2", xlab="Notas prova2", ylab="Frequencia")
qqnorm(sessoes.prova2$nota.prova2)
qqline(sessoes.prova2$nota.prova2)
dev.off()

#Sessoes x Prova 3
png(filename = "TamanhoSessoesProva3.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova3$tamanhoSessao, main="Histograma Tamanho Sessoes", xlab="Tamanho Sessoes", ylab="Frequencia")
qqnorm(sessoes.prova3$tamanhoSessao)
qqline(sessoes.prova3$tamanhoSessao)
dev.off()

png(filename = "NotaProva3.png", width=650)
par(mfrow=c(1,2))
hist(sessoes.prova3$nota.prova3, main="Histograma notas prova3", xlab="Notas prova3", ylab="Frequencia")
qqnorm(sessoes.prova3$nota.prova3)
qqline(sessoes.prova3$nota.prova3)
dev.off()






ad.test(sessoes.prova1$tamanhoSessao) #FALSE
shapiro.test(sessoes.prova1$tamanhoSessao) #FALSE
ad.test(sessoes.prova2$tamanhoSessao) #FALSE
shapiro.test(sessoes.prova2$tamanhoSessao)  #FALSE
ad.test(sessoes.prova3$tamanhoSessao) #FALSE
shapiro.test(sessoes.prova3$tamanhoSessao) #FALSE

ad.test(sessoes.acm2$tamanhoSessao) #FALSE
shapiro.test(sessoes.acm2$tamanhoSessao) #FALSE
ad.test(sessoes.acm3$tamanhoSessao) #FALSE
shapiro.test(sessoes.acm3$tamanhoSessao) #FALSE


ad.test(sessoes.prova1$nota.prova1) #FALSE
shapiro.test(sessoes.prova1$nota.prova1) # FALSE
ad.test(sessoes.prova2$nota.prova2) #FALSE
shapiro.test(sessoes.prova2$nota.prova2) #FALSE
ad.test(sessoes.prova3$nota.prova3) #FALSE
shapiro.test(sessoes.prova3$nota.prova3) #FALSE

#skewness(sessoes.prova3$nota.prova3)/sqrt(6/100) 
#kurtosis(sessoes.prova3$nota.prova3)/sqrt(24/100)

cor.test(sessoes.prova1$tamanhoSessao,sessoes.prova1$nota.prova1,method=c("kendall"))
cor.test(sessoes.prova1$tamanhoSessao,sessoes.prova1$nota.prova1,method=c("spearman"))

cor.test(sessoes.prova2$tamanhoSessao,sessoes.prova2$nota.prova2,method=c("kendall"))
cor.test(sessoes.prova2$tamanhoSessao,sessoes.prova2$nota.prova2,method=c("spearman"))

cor.test(sessoes.prova3$tamanhoSessao,sessoes.prova3$nota.prova3,method=c("kendall"))
cor.test(sessoes.prova3$tamanhoSessao,sessoes.prova3$nota.prova3,method=c("spearman"))

cor.test(sessoes.acm2$tamanhoSessaoAcm,sessoes.acm2$nota.prova2,method=c("kendall"))
cor.test(sessoes.acm2$tamanhoSessaoAcm,sessoes.acm2$nota.prova2,method=c("spearman"))

cor.test(sessoes.acm3$tamanhoSessaoAcm,sessoes.acm3$nota.prova3,method=c("kendall"))
cor.test(sessoes.acm3$tamanhoSessaoAcm,sessoes.acm3$nota.prova3,method=c("spearman"))
