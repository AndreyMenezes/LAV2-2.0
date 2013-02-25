

require(ggplot2)

dados <- read.csv("tableMeanDiscipline.csv")
dados$matricula <- reorder(dados$matricula,dados$meanSession, order=T)


grafico<-ggplot(dados,aes(x=matricula,y=meanSession)) + geom_bar() +
theme_bw()+labs(x="Matriculas",y="Tempo das Sessões")+ylim(c(0,max(dados$meanSession)))+
theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
theme(axis.ticks = element_blank()) 

png("SesssionLength.png",bg="transparent",width = 800, height = 600)
print(grafico)
dev.off()

print("o")