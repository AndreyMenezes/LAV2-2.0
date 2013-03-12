# US15 
# Iury Gregory - 
1312818964 -1312808400
exercicios <- read.csv("dados/exercicios-20112.csv",header=F)
colnames(exercicios) <- c("matricula","questao","turma","data.hora","status","nota")
geral <- read.csv("dados/Geral.csv",header=T)

exercicios.sub <- subset(exercicios,select=c("matricula","data.hora"))
geral <- subset(geral,select=c("matricula","turma.pratica"))

dados <- merge(exercicios.sub,geral,by.x="matricula",by.y="matricula")
dados$dh <- strptime(dados$data.hora,"%m/%d/%Y %H:%M:%S")
dados$timestamp <- as.numeric(as.POSIXct(dados$dh,origin="1970-01-01"))
dados <- dados[,-2]
colnames(dados) <- c("matricula","turma.pratica","data.hora","timestamp")
dados <- dados[order(dados$timestamp,decreasing=F),]
dados$data.hora <- paste(substr(dados$data.hora,1,13),":00:00",sep="")
dados$comparador <- paste(substr(dados$data.hora,1,10),dados$turma.pratica,sep="*")

aulas <- read.csv("dados/aulas.csv",header=F)
colnames(aulas) <- c("data.hora","weekday","turma.pratica")
aulas$timestamp <- as.numeric(as.POSIXct(aulas$data.hora,origin="1970-01-01"))
aulas$comparador <- paste(substr(aulas$data.hora,1,10),aulas$turma.pratica,sep="*")

# Comparador é a juncao da data e turma ja que eu n tava conseguindo resultados
#para dar um merge =/

tabela = merge(dados,aulas,by="comparador",all.x = TRUE)
tabela$classificacao = NA


for(i in 1:nrow(tabela)){
	if(tabela$timestamp.x - tabela$timestamp.y <= 7200){
		tabela[i,"classificacao"] = "realizado em horario de aula"
	}else{
		tabela[i,"classificacao"] = "realizado fora do horario de aula"
	}
}