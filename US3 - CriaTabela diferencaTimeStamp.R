# Script para criar a tabela diferencaTimeStamp, que calcula a diferença de tempo entre uma submissão e outra
# Iury Gregory - Versão 2.0 (Fevereiro 2013)
require(ggplot2)

dados <- read.csv("dados/exercicios-20112.csv",header=F,stringsAsFactors=F)
dados.pre <- dados[,c(1,4)]
dados.pre$dh <- strptime(dados.pre[,2],"%m/%d/%Y %H:%M:%S")
dados.pre$timestamp <- as.numeric(as.POSIXct(dados.pre[,3],origin="1970-01-01"))
dados.pre <- dados.pre[,c(1,4,3)]
colnames(dados.pre) <- c("matricula","timestamp","dataHora")
dados.pre <- dados.pre[order(dados.pre$matricula,dados.pre$timestamp,decreasing = F),]

diferencaTimeStamp = function(dados) {
	matriculas <- unique(dados.pre$matricula)
	dados.pre$diferenca <- 0
	df <- data.frame()

	for( i in 1:length(matriculas)){
		indices <- which(dados.pre$matricula == matriculas[i])
		temp <- dados.pre[indices,]
		for ( j in 1:nrow(temp)){
			if( j == 1){
				temp[j,4] = 0
			}else{
				temp[j,4] = temp[j,2] - temp[j-1,2]
			}
		}
		df = rbind(df,temp)
	}
}

df <- diferencaTimeStamp(dados.pre)
write.table(df,"dados/diferencaTimestamp.csv",sep=",",row.names=F,col.names=T)


dados2 <- read.csv("dados/tableMeanDiscipline.csv",header=T)
dados2$matricula <- reorder(dados2$matricula,dados$meanSession, order=T)


grafico<-ggplot(dados2,aes(x=matricula,y=meanSession)) + geom_bar() +
theme_bw()+labs(x="Matriculas",y="Tempo das Sessões")+ylim(c(0,max(dados2$meanSession)))+
theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
theme(axis.ticks = element_blank()) 

png("SesssionLength.png",bg="transparent",width = 800, height = 600)
print(grafico)
dev.off()
