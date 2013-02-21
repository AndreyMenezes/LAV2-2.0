#Codigo gerador do grafico do total de sessões por dia durante o periodo
#Alberto - versão 1.0(Fevereiro 2013)

sessoes<-read.csv("dados/tabelaDataSessao.csv")

p<-ggplot(sessoes,aes(dataDasSessoes,sessoes))
p +geom_bar() +geom_segment(aes(x = "09/17/2011", y = 125, xend = "09/17/2011", yend = 150,color="Prova 01"))+geom_segment(aes(x = "10/29/2011", y = 125, xend = "10/29/2011", yend = 150,color="Prova 02"))+geom_segment(aes(x = "11/26/2011", y = 125, xend = "11/26/2011", yend = 150,color="Prova 03"))



#png(filename="nome.png")
#dev.off()

#a = qplot(sessoesPorData$dataSessoes, sessoesPorData$sessoes, geom="histogram", xlab="Tempo", ylab="Numero de sessoes")
#a + geom_vline(colour="green", linetype = "longdash")



