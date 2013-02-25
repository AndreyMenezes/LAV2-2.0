#
#Alberto

sessoes<-read.csv("dados/TableSessionLength.csv")
sessoes<- subset(sessoes,select=lastSubmission)

vetorSession<- as.matrix(sessoes)
vetorSession<- as.vector(vetorSession)

vetorSession<-sub(" .*","",vetorSession)
vetorSession<-sort(vetorSession, decreasing = FALSE)

contador = 0
dataAtual = vetorSession[1]
sessoes = vector(mode="numeric",length=0)
dataSessoes = vector(mode="character",length=0)
contadorPosicao = 1
vetorSession[4177] = ""

for(i in vetorSession){
    if(i==dataAtual){
        contador = contador+1
    }else{
        sessoes[contadorPosicao] = contador
        dataSessoes[contadorPosicao] = dataAtual
        contadorPosicao = contadorPosicao+1
        contador=1
        dataAtual=i
    }
}

sessoesPorData = cbind(dataSessoes,sessoes)
sessoesPorData = as.data.frame(sessoesPorData)

p<-ggplot(sessoesPorData,aes(dataSessoes,sessoes))
p+geom_bar()

png(filename="nome.png")
dev.off()
fix(sessoesPorData)



a = qplot(sessoesPorData$dataSessoes, sessoesPorData$sessoes, geom="histogram", xlab="Tempo", ylab="Numero de sessoes")
a + geom_vline(colour="green", linetype = "longdash")













