#Codigo gerador do grafico do total de sessões por dia durante o periodo
#Alberto - versão 2.0(Fevereiro 2013)

require(ggplot2)
sessoes<-read.csv("dados/tabelaDataSessao.csv")

p<-ggplot(sessoes,aes(dataDasSessoes,sessoes))+geom_histogram()+labs(x="Datas",y="Total de Sessões")+
geom_segment(aes(x = "09/17/2011", y = 0, xend = "09/17/2011", yend = 150,color="09/17/2011 - Prova 01"))+
geom_segment(aes(x = "10/29/2011", y = 0, xend = "10/29/2011", yend = 150,color="10/29/2011 - Prova 02"))+
geom_segment(aes(x = "11/26/2011", y = 0, xend = "11/26/2011", yend = 150,color="11/26/2011 - Prova 03"))+
geom_segment(aes(x = "12/03/2011", y = 0, xend = "12/03/2011", yend = 150,color="12/03/2011 - Prova Final"))+
scale_colour_manual("Data das Provas", breaks = c("09/17/2011 - Prova 01", "10/29/2011 - Prova 02", "11/26/2011 - Prova 03","12/03/2011 - Prova Final"),values = c("red", "green", "blue","orange"))+
theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
theme(axis.ticks = element_blank())


png(filename="us7.png",bg="transparent",width = 580, height = 480)
print(p)
dev.off()
