# Codigo gerador do grafico do total de sessões por dia durante o periodo
# Alberto - versão 3.0 (Fevereiro 2013)

require(ggplot2)
sessoes<-read.csv("dados/tabelaDataSessao.csv")
sessoes<-data.frame(sessoes,1)

p<-ggplot(sessoes,aes(dataDasSessoes,sessoes,group=X1))+geom_line()+labs(x="Datas",y="Total de Sessões")+
geom_segment(aes(x = "09/17/2011", y = 0, xend = "09/17/2011", yend = 150),colour="red")+
geom_segment(aes(x = "10/29/2011", y = 0, xend = "10/29/2011", yend = 150),colour="pink")+
geom_segment(aes(x = "11/26/2011", y = 0, xend = "11/26/2011", yend = 150),colour="blue")+
geom_segment(aes(x = "12/03/2011", y = 0, xend = "12/03/2011", yend = 150),colour="orange")+
theme_bw()+
theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
theme(axis.ticks = element_blank())+
geom_text(data = NULL, x = 14, y = 145, label = "09/17/2011 - Prova 1",size=4,colour="red")+
geom_text(data = NULL, x = 14, y = 140, label = "10/29/2011 - Prova 2",size=4,colour="pink")+
geom_text(data = NULL, x = 14, y = 135, label = "11/26/2011 - Prova 3",size=4,colour="blue")+
geom_text(data = NULL, x = 14, y = 130, label = "12/03/2011 - Prova Final",size=4,colour="orange")


png(filename="us7.png",bg="transparent",width = 580, height = 480)
print(p)
dev.off()
