# Script de analise das submissões US15 - Equivale a US3
# Iury Gregory

library(Hmisc)

diferencaTimeStamp = function(dados) {
	matriculas <- unique(dados$matricula)
	dados$diferenca <- 0
	df <- data.frame()
	for( i in 1:length(matriculas)){
		indices <- which(dados$matricula == matriculas[i])
		temp <- dados[indices,]
		for ( j in 1:nrow(temp)){
			if( j == 1){
				temp[j,"diferenca"] = 0
			}else{
				temp[j,"diferenca"] = temp[j,"timestamp"] - temp[j-1,"timestamp"]
			}
		}
		df = rbind(df,temp)
	}
	return (df)
}


diferencaTimeStampAula = function(dados) {
	matriculas <- unique(dados$matricula)
	dados$diferenca <- 0
	df <- data.frame()
	for( i in 1:length(matriculas)){
		indices <- which(dados$matricula == matriculas[i])
		temp <- dados[indices,]
		for ( j in 1:nrow(temp)){
			temp[j,"diferenca"] = temp[j,"timestamp"] - temp[j,"timestampAula"]
		}
		df = rbind(df,temp)
	}
	return (df)
}





sessaoDeCadaSubmissao = function(dados, limiar) {
	tableSession = data.frame(matricula=c(),turma=c(),data.hora = c(),status = c(),nota = c(), turma.pratica = c(), timestamp = c(), classe = c(),
                                session=c(), timeSession=c(), lastSubmission=c())
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
			tableSession = rbind(tableSession,data.frame(matricula,dados[i-1,"turma"],dados[i-1,"data.hora"],dados[i-1,"status"],dados[i-1,"nota"],
                                       dados[i-1,"turma.pratica"],dados[i-1,"timestamp"],dados[i-1,"classe"],session, timeSession, amountSubmission) )
			matricula = dados[i, "matricula"]
			session = 1
			dados$session[i] = session
			timeSession = 0
			amountSubmission = 1
		}
		if((dados[i,"diferenca"] > limiar) & matricula == dados[i, "matricula"]) {
			tableSession = rbind(tableSession, data.frame(matricula,dados[i-1,"turma"],dados[i-1,"data.hora"],dados[i-1,"status"],dados[i-1,"nota"],
                                       dados[i-1,"turma.pratica"],dados[i-1,"timestamp"],dados[i-1,"classe"],session, timeSession, amountSubmission))
			session = session+1
			dados$session[i] = session
			timeSession = 0
			amountSubmission = 1
		}
	}
	return(tableSession)
}


############################################################################
#Em aula
############################################################################

submissoes.aula <- read.csv("dados/SubmissoesHorarioDeAula.csv",header=T)
submissoes.aula <- submissoes.aula[order(submissoes.aula$timestamp,decreasing = F),]


tabela.aula <- diferencaTimeStampAula(submissoes.aula) 

tabela = tabela.aula
limiar.aula = 7200
tabelaSessaoAula = sessaoDeCadaSubmissao(tabela, limiar.aula)

			
#Tabela com matricula, sess?o, tempo da sess?o, data da ultima submiss?o da sess?o
colnames(tabelaSessaoAula ) = c("matricula","turma","data.hora","status","nota","turma.pratica"
,"timestamp","classe","session", "timeSession","amountSubmission")
write.csv(tabelaSessaoAula, "dados/TableSessionLengthEmAula.csv", row.names=F)


#Tabela da m?dia das sess?es de cada aluno
tableSumDiscipline.aula = with(tabelaSessaoAula, aggregate(timeSession, list(matricula), FUN=sum))
colnames(tableSumDiscipline.aula) = c("matricula", "sumSession")
write.csv(tableSumDiscipline.aula, "dados/tableSumDisciplineEmAula.csv", row.names=F)

#Tabela da m?dia das sess?es de cada aluno separado por cada prova
tableSumPerTest.aula = c(matricula=c(), sumSessions=c(), Prova=c())

test1.aula = subset(tabelaSessaoAula , as.numeric(as.POSIXct(strptime(tabelaSessaoAula$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test1.aula = with(test1.aula, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test1.aula) = c("matricula","sumSessions")
test1.aula$Prova = "Prova1"
tableSumPerTest.aula = rbind(tableSumPerTest.aula, test1.aula)
test2.aula = subset(tabelaSessaoAula, as.numeric(as.POSIXct(strptime(tabelaSessaoAula$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(strptime(tabelaSessaoAula$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) > as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test2.aula = with(test2.aula, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test2.aula) = c("matricula","sumSessions")
test2.aula$Prova = "Prova2"
tableSumPerTest.aula = rbind(tableSumPerTest.aula, test2.aula)
test3.aula = subset(tabelaSessaoAula , as.numeric(as.POSIXct(strptime(tabelaSessaoAula$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-11-26 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(strptime(tabelaSessaoAula$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) > as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")))
test3.aula = with(test3.aula, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test3.aula) = c("matricula","sumSessions")
test3.aula$Prova = "Prova3"
tableSumPerTest.aula = rbind(tableSumPerTest.aula, test3.aula)

write.csv(tableSumPerTest.aula, "dados/tableSumPerTestEmAula.csv", row.names=F)
	
############################################################################
#Fora de aula
############################################################################

submissoes.fora <- read.csv("dados/SubmissoesForaHorarioDeAula.csv",header=T)
submissoes.fora <- submissoes.fora[order(submissoes.fora$timestamp,decreasing = F),]
submissoes.fora <- submissoes.fora[,-9]

tabela.fora <- diferencaTimeStamp(submissoes.fora) 

tabela2 = tabela.fora
limiar = quantile(tabela.fora$diferenca, 0.78)
tabelaSessaoFora = sessaoDeCadaSubmissao(tabela2, limiar)


			
#Tabela com matricula, sess?o, tempo da sess?o, data da ultima submiss?o da sess?o
colnames(tabelaSessaoFora ) = c("matricula","turma","data.hora","status","nota","turma.pratica"
,"timestamp","classe","session", "timeSession","amountSubmission")
write.csv(tabelaSessaoFora, "dados/TableSessionLengthForadeAula.csv", row.names=F)


#Tabela da m?dia das sess?es de cada aluno
tableSumDiscipline = with(tabelaSessaoFora, aggregate(timeSession, list(matricula), FUN=sum))
colnames(tableSumDiscipline) = c("matricula", "sumSession")
write.csv(tableSumDiscipline, "dados/tableSumDisciplineForadeAula.csv", row.names=F)

#Tabela da m?dia das sess?es de cada aluno separado por cada prova
tableSumPerTest = c(matricula=c(), sumSessions=c(), Prova=c())

test1 = subset(tabelaSessaoFora , as.numeric(as.POSIXct(strptime(tabelaSessaoFora$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test1 = with(test1, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test1) = c("matricula","sumSessions")
test1$Prova = "Prova1"
tableSumPerTest = rbind(tableSumPerTest, test1)
test2 = subset(tabelaSessaoFora, as.numeric(as.POSIXct(strptime(tabelaSessaoFora$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(strptime(tabelaSessaoFora$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) > as.numeric(as.POSIXct("2011-09-17 10:00:00", origin="1970-01-01")))
test2 = with(test2, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test2) = c("matricula","sumSessions")
test2$Prova = "Prova2"
tableSumPerTest = rbind(tableSumPerTest, test2)
test3 = subset(tabelaSessaoFora , as.numeric(as.POSIXct(strptime(tabelaSessaoFora$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) < as.numeric(as.POSIXct("2011-11-26 10:00:00", origin="1970-01-01")) & as.numeric(as.POSIXct(strptime(tabelaSessaoFora$data.hora,"%m/%d/%Y %H:%M:%S"), origin="1970-01-01")) > as.numeric(as.POSIXct("2011-10-29 10:00:00", origin="1970-01-01")))
test3 = with(test3, aggregate(timeSession, list(matricula), FUN=sum))
colnames(test3) = c("matricula","sumSessions")
test3$Prova = "Prova3"
tableSumPerTest = rbind(tableSumPerTest, test3)

write.csv(tableSumPerTest, "dados/tableSumPerTestForadeAula.csv", row.names=F)
	