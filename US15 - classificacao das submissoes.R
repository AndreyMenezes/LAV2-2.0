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
aulas$timestamp <- as.numeric(as.POSIXct(aulas$data.hora,origin="1970-01-01"))
aulas <- aulas[,c(-1,-2)]


classificador <- function(tabela.aluno,tabela.aula,num.turma){
	dados.aluno <- subset(tabela.aluno,tabela.aluno$turma.pratica == num.turma)
	dados.aula <- subset(tabela.aula,tabela.aula$turma.pratica == num.turma)
	tabela.saida <- data.frame(comparador <-c(),classe <- c())
	for(i in 1:nrow(dados.aluno)){
		for(j in 1:nrow(dados.aula)){
			if(dados.aluno [i,"turma.pratica"] == dados.aula[j,"turma.pratica"]){
				tabela.saida[i,"comparador"] <- dados.aluno[i,"comparador"]
				if(dados[i,"data"] == aulas[j,"data"]){
					if(dados.aluno[i,"timestamp"]-dados.aula[j,"timestamp"] <= 7200){
						tabela.saida[i,"classe"] = "realizado em horario de aula"
					}else{
						tabela.saida[i,"classe"] = "realizado fora do horario de aula"
					}
				}else{
					tabela.saida[i,"classe"] = "realizado fora do horario de aula"		
				}
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
dados2 <- dados2[,c(-1,-4,-9)]

write.table(dados2,"dados/SubmissoesClassificadas.csv",sep=",",row.names=F,col.names=T)