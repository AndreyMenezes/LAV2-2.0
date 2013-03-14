# Script de analise das submissões US15
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
				temp[j,9] = 0
			}else{
				temp[j,9] = temp[j,7] - temp[j-1,7]
			}
		}
		df = rbind(df,temp)
	}
	return (df)
}


submissoes.aula <- read.csv("dados/SubmissoesHorarioDeAula.csv",header=T)
submissoes.aula <- submissoes.aula[order(submissoes.aula$timestamp,decreasing = F),]
tabela<-diferencaTimeStamp(submissoes.aula)

png("Ecdf1.png",bg="white")
Ecdf(tabela$diferenca/3600,xlab = "Intervalo entre Submissões",
ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE)
dev.off()

png("Ecdf2.png",bg="white")
Ecdf(tabela$diferenca/3600,xlab = "Intervalo entre Submissões",
ylab="Proporção <= x",label.curves=TRUE,col="blue",las=1, subtitles=FALSE, xlim=c(0,48))
dev.off()
