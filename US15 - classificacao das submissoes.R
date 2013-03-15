# US15  - Classificacao das submissoes em "realizado em horario de aula"
# e em "realizado fora do horario de aula"
# Iury Gregory -  Versão 2.0 (Fevereiro 2013)

exercicios <- read.csv("dados/exercicios-20112.csv",header=F)
colnames(exercicios) <- c("matricula","questao","turma","data.hora","status","nota")
geral <- read.csv("dados/Geral.csv",header=T)
geral <- subset(geral,select=c("matricula","turma.pratica"))

dados <- merge(exercicios,geral,by.x="matricula",by.y="matricula")
dados$data <- substr(strptime(dados$data.hora,"%m/%d/%Y %H:%M:%S"),1,10)
dados$timestamp <- as.numeric(as.POSIXct(strptime(dados$data.hora,"%m/%d/%Y %H:%M:%S"),origin="1970-01-01"))
dados$comparador <- seq(1,nrow(dados),1)

aulas <- read.csv("dados/aulas.csv",header=F)
colnames(aulas) <- c("data.hora","weekday","turma.pratica")
aulas$data <- substr(aulas$data.hora,1,10) 
aulas$timestampAula <- as.numeric(as.POSIXct(aulas$data.hora,origin="1970-01-01"))
aulas <- aulas[,c(-2)]
aulas$data.hora <- as.character(aulas$data.hora)


classificador <- function(tabela.aluno,tabela.aula,num.turma){
	dados.aluno <- subset(tabela.aluno,tabela.aluno$turma.pratica == num.turma)
	dados.aula <- subset(tabela.aula,tabela.aula$turma.pratica == num.turma)
	tabela.saida <- data.frame(comparador <-c(),classe <- c(),dataHora <-c(),timestampAula <- c())
	
	for(i in 1:nrow(dados.aluno)){
		passou = FALSE
		cont = 0
		tabela.saida[i,"comparador"] <- dados.aluno[i,"comparador"]
		for(j in 1:nrow(dados.aula)){
			if(dados.aluno[i,"data"] == dados.aula[j,"data"]){
				inicio = dados.aula[j,"timestampAula"]
				fim = dados.aula[j,"timestampAula"]+7200
				if(dados.aluno[i,"timestamp"] >= inicio & dados.aluno[i,"timestamp"] <= fim){
					tabela.saida[i,"classe"] = "realizado em horario de aula"
					tabela.saida[i,"timestampAula"] <- dados.aula[j,"timestampAula"]
					tabela.saida[i,"dataHora"] <- dados.aula[j,"data.hora"]
					passou = TRUE
				}

			}
			if(j == nrow(dados.aula) & identical(passou,FALSE)){
				tabela.saida[i,"classe"] = "realizado fora do horario de aula"
				tabela.saida[i,"dataHora"] <- "semdatahora"
				tabela.saida[i,"timestampAula"] <-123456789101112
			}
			
			
		}
	}
	return (tabela.saida)
}

tabela1 <- classificador(dados,aulas,1)
tabela2 <- classificador(dados,aulas,2)
tabela3 <- classificador(dados,aulas,3)
tabela4 <- classificador(dados,aulas,4)
tabela5 <- classificador(dados,aulas,5)
tabela.class <- rbind(tabela1,tabela2,tabela3,tabela4,tabela5)


dados2 <- merge(dados,tabela.class,by="comparador")
dados2 <- dados2[,c(-1,-3,-9)]

sub.aula <- subset(dados2,dados2$classe == "realizado em horario de aula")
sub.fora <- subset(dados2,dados2$classe == "realizado fora do horario de aula")

write.table(sub.fora,"dados/SubmissoesForaHorarioDeAula.csv",sep=",",row.names=F,col.names=T)
write.table(sub.aula,"dados/SubmissoesHorarioDeAula.csv",sep=",",row.names=F,col.names=T)

