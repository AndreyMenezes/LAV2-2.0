# Script de analise das submissoes US15 - Equivale a US4
# Iury Gregory - versao 2.0 Março 2013

library(nortest)

data_aula = read.csv("dados/tableSumDisciplineEmAula.csv",header=T)
data_fora = read.csv("dados/tableSumDisciplineForadeAula.csv",header=T)
data_all = read.csv("dados/Geral.csv")
data_all = subset(data_all, turma.pratica != 5)
data_aula_all = data_all
data_fora_all = data_all

#################
#Horario de Aula#
#################
intersect.aula = intersect(data_aula_all$matricula, data_aula$matricula)
data_aula = subset(data_aula, matricula%in%intersect.aula)
data_aula_all = subset(data_aula_all, matricula%in%intersect.aula)



#Analisando pré-requisitos para analise de correlação
png(filename = "Pre-requisitosMediasProvas-HorarioDeAula.png", width=650)
par(mfrow=c(1,2))
hist(data_aula_all$nota.final.pratica, prob=T, main="Média nas provas", xlab="Media na disciplina", ylab="Densidade")
qqnorm(data_aula_all$nota.final.pratica)
qqline(data_aula_all$nota.final.pratica)
dev.off()
ad.test(data_aula_all$nota.final.pratica)
shapiro.test(data_aula_all$nota.final.pratica)


png(filename = "Pre-requisitosSomaSessoes-HorarioDeAula.png", width=650)
par(mfrow=c(1,2))
hist(data_aula$sumSession, prob=T, main="Tempo das Sessões", xlab="Tempo das Sessões", ylab="Densidade")
qqnorm(data_aula$sumSession)
qqline(data_aula$sumSession)
dev.off()
ad.test(data_aula$sumSession)
shapiro.test(data_aula$sumSession)

#Realiza os testes da correlação
png(filename = "corr-temposessoes-HorarioDeAula.png", width=650)
plot(data_aula_all$nota.final.teoria,
	(data_aula$sumSession/3600), 
	log="xy", xlab = "Nota na disciplina", ylab = "Tempo total de Estudo", 
	las = 1, col=c("darkorange3","brown1","cornflowerblue","aquamarine3","black")[data_aula_all$status],
	pch=19)

legend("topleft", legend = c("aprovado na final", "aprovado por média", "reprovado na final", "reprovado por média"), pch=19, col=c("brown1","cornflowerblue","aquamarine3","black"))
dev.off()



data_aula_all = data_aula_all[with(data_aula_all, order(matricula)), ]
data_aula = data_aula[with(data_aula, order(matricula)), ]
cor.test(data_aula$sumSession, data_aula_all$nota.final.pratica, method="spearman")




######################
#Fora Horario de Aula#
######################
intersect.fora = intersect(data_fora_all$matricula, data_fora$matricula)
data_fora = subset(data_fora, matricula%in%intersect.fora)
data_fora_all = subset(data_fora_all, matricula%in%intersect.fora)



#Analisando pré-requisitos para analise de correlação
png(filename = "Pre-requisitosMediasProvas-ForaHorarioDeAula.png", width=650)
par(mfrow=c(1,2))
hist(data_fora_all$nota.final.pratica, prob=T, main="Média nas provas", xlab="Media na disciplina", ylab="Densidade")
qqnorm(data_fora_all$nota.final.pratica)
qqline(data_fora_all$nota.final.pratica)
dev.off()
ad.test(data_fora_all$nota.final.pratica)
shapiro.test(data_fora_all$nota.final.pratica)


png(filename = "Pre-requisitosSomaSessoes-ForaHorarioDeAula.png", width=650)
par(mfrow=c(1,2))
hist(data_fora$sumSession, prob=T, main="Tempo das Sessões", xlab="Tempo das Sessões", ylab="Densidade")
qqnorm(data_fora$sumSession)
qqline(data_fora$sumSession)
dev.off()
ad.test(data_fora$sumSession)
shapiro.test(data_fora$sumSession)

#Realiza os testes da correlação
png(filename = "corr-temposessoes-ForaHorarioDeAula.png", width=650)
plot(data_fora_all$nota.final.teoria,
	(data_fora$sumSession/3600), 
	log="xy", xlab = "Nota na disciplina", ylab = "Tempo total de Estudo", 
	las = 1, col=c("darkorange3","brown1","cornflowerblue","aquamarine3","black")[data_fora_all$status],
	pch=19)

legend("topleft", legend = c("aprovado na final", "aprovado por média", "reprovado na final", "reprovado por média"), pch=19, col=c("brown1","cornflowerblue","aquamarine3","black"))
dev.off()



data_fora_all = data_fora_all[with(data_fora_all, order(matricula)), ]
data_fora = data_fora[with(data_fora, order(matricula)), ]
cor.test(data_fora$sumSession, data_fora_all$nota.final.pratica, method="spearman")