# Codigo que cria as tabelas dos agrupamentos dos dados
# Andrey Menezes e Iara Ribeiro - vers?o 2.0 (Fevereiro 2013)

data_sum = read.csv("dados/tableSumDiscipline.csv")
data_all = read.csv("dados/Geral.csv")

data_all = subset(data_all, turma.pratica != 5)

intersect = intersect(data_all$matricula, data_mean$matricula)
data_sum = subset(data_sum, matricula%in%intersect)
data_all = subset(data_all, matricula%in%intersect)

#Analisando pré-requisitos para analise de correlação
png(filename = "Pre-requisitosMediasProvas.png", width=650)
par(mfrow=c(1,2))
hist(data_all$nota.final.pratica, prob=T, main="Média nas provas", xlab="Media na disciplina", ylab="Densidade")
qqnorm(data_all$nota.final.pratica)
qqline(data_all$nota.final.pratica)
dev.off()
ad.test(data_all$nota.final.pratica)
shapiro.test(data_all$nota.final.pratica)

png(filename = "Pre-requisitosSomaSessoes.png", width=650)
par(mfrow=c(1,2))
hist(data_sum$sumSession, prob=T, main="Tempo das Sessões", xlab="Tempo das Sessões", ylab="Densidade")
qqnorm(data_sum$sumSession)
qqline(data_sum$sumSession)
dev.off()
ad.test(data_sum$sumSession)
shapiro.test(data_sum$sumSession)

png(filename = "cor-temposessoes.png", width=650)
plot(data_all$nota.final.teoria, 
	(data_sum$sumSession/3600), 
	log="xy", xlab = "Nota na disciplina", ylab = "Tempo total de Estudo", 
	las = 1, col=c("darkorange3","brown1","cornflowerblue","aquamarine3","black")[data_all$status],
	pch=19)
dev.off()

data_all = data_all[with(data_all, order(matricula)), ]
data_sum = data_sum[with(data_sum, order(matricula)), ]
cor.test(data_sum$sumSession, data_all$nota.final.pratica, method="spearman")
