# Codigo que cria 3 tabelas, tableSessionLength: com o as sessões e o tamanho delas, tableSumDiscipline: Tempo de estudo por aluno (soma das sessões)
# e tableSumPerTest: tempo de estudo entre provas.
# Andrey Menezes - versão 2.0 (Fevereiro 2013)
require(Hmisc)

dados = read.csv("dados/diferencaTimestamp.csv", stringsAsFactors=F)

#Calculando a sessao de cada submissao
sessaoDeCadaSubmissao = function(dados, limiar) {
	tableSession = data.frame(matricula=c(), session=c(), timeSession=c(), lastSubmission=c())
	dados$session = NA
	amountSubmission = 0
	timeSession = 0
	session = 1
	matricula = dados[1,"matricula"]
	for(i in 1:length(dados$matricula)) {
		if((dados[i,"diferenca"] < limiar) & matricula == dados[i, "matricula"]) {
			dados$session[i] = session
			amountSubmission = amountSubmission+1
			timeSession = timeSession+dados[i,"diferenca"]
		}
		if(matricula != dados[i, "matricula"]) {
			tableSession = rbind(tableSession, data.frame(matricula, session, timeSession, dados[i-1,"dataHora"], amountSubmission))
			matricula = dados[i, "matricula"]
			session = 1
			dados$session[i] = session
			timeSession = 0
			amountSubmission = 1
		}
		if((dados[i,"diferenca"] > limiar) & matricula == dados[i, "matricula"]) {
			tableSession = rbind(tableSession, data.frame(matricula, session, timeSession, dados[i-1,"dataHora"], amountSubmission))
			session = session+1
			dados$session[i] = session
			timeSession = 0
			amountSubmission = 1
		}
	}
	return(tableSession)
}

limiar = quantile(dados$diferenca, 0.78)
tableSession = sessaoDeCadaSubmissao(dados, limiar)


png("Ecdf.png",bg="white",width=700, height=400)
  par(mfrow=c(1,2))
  Ecdf(dados$diferenca/3600,q=(0.78),xlab = "Intervalo entre Submissões",
  ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE,)
  Ecdf(dados$diferenca/3600,q=(0.78),xlab = "Intervalo entre Submissões",
  ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE, xlim=c(0,48))
dev.off()


pdf("Ecdf.pdf",bg="white",width=700/100, height=400/100)
  par(mfrow=c(1,2))
  Ecdf(dados$diferenca/3600,q=(0.78),xlab = "Intervalo entre Submissões",
  ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE,)
  Ecdf(dados$diferenca/3600,q=(0.78),xlab = "Intervalo entre Submissões",
  ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE, xlim=c(0,48))
dev.off()


#Tabela com matricula, sess?o, tempo da sess?o, data da ultima submiss?o da sess?o
colnames(tableSession) = c("matricula", "session", "timeSession", "lastSubmission", "amountSubmission")
write.csv(tableSession, "dados/TableSessionLength.csv", row.names=F)

#Tabela da m?dia das sess?es de cada aluno
tableSumDiscipline = with(tableSession, aggregate(timeSession, list(matricula), FUN=sum))
colnames(tableSumDiscipline) = c("matricula", "sumSession")
write.csv(tableSumDiscipline, "dados/tableSumDiscipline.csv", row.names=F)

#Tabela da m?dia das sess?es de cada aluno separado por cada prova
tableSumPerTest = c(matricula=c(), sumSessions=c(), Prova=c())

test1 = subset(tableSession, as.numeric(as.POSIXct(tableSession[,"lastSubmission"], origin="1970-01-01")) < as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test1 = with(test1, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test1) = c("matricula","sumSessions")
test1$Prova = "Prova1"
tableSumPerTest = rbind(tableSumPerTest, test1)
test2 = subset(tableSession, as.numeric(as.POSIXct(tableSession[,"lastSubmission"], origin="1970-01-01")) < as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(tableSession[,"lastSubmission"], origin="1970-01-01")) > as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test2 = with(test2, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test2) = c("matricula","sumSessions")
test2$Prova = "Prova2"
tableSumPerTest = rbind(tableSumPerTest, test2)
test3 = subset(tableSession, as.numeric(as.POSIXct(tableSession[,"lastSubmission"], origin="1970-01-01")) < as.numeric(as.POSIXct("2011-11-26 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(tableSession[,"lastSubmission"], origin="1970-01-01")) > as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")))
test3 = with(test3, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test3) = c("matricula","sumSessions")
test3$Prova = "Prova3"
tableSumPerTest = rbind(tableSumPerTest, test3)

write.csv(tableSumPerTest, "dados/tableSumPerTest.csv", row.names=F)
