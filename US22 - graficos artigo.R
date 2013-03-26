library(ggplot2)


geral <- read.csv("dados/Geral.csv")
geral <- geral[,c(2,7)]



#Atividade
atividade <- read.csv("dados/AgrupamentoAtividadeForadeAula.csv")
atividade <- atividade[,-1]
atividade = atividade[with(atividade,order(atividade,decreasing=T)),]
atividade$matricula <- reorder(atividade$matricula,atividade$atividade, order=T)
atividade$rank = as.factor(seq(1, nrow(atividade), 1))

grafico.atividade <- ggplot(atividade,aes(rank,atividade)) + geom_point()+
 	     theme_bw()+labs(x="Rank dos alunos",y="Atividade de Estudo") +ylim(c(0,0.75))+
           scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "AtividadedeEstudo.png", width = 480, height = 480)
print(grafico.atividade)
dev.off()

pdf("AtividadeDeEstudo.pdf", width = 480/100, height = 480/100)
print(grafico.atividade)
dev.off()


#Tempo total de estudo
tempo <- read.csv("dados/tableSumDisciplineForadeAula.csv")
tempo = tempo[with(tempo,order(sumSession,decreasing=T)),]
tempo$matricula <- reorder(tempo$matricula,tempo$sumSession, order=T)
tempo$rank = as.factor(seq(1, nrow(tempo), 1))


grafico.tempo <- ggplot(tempo,aes(rank,sumSession/3600)) + 
 	     geom_point() + theme_bw()+labs(x="Rank dos alunos",y="Tempo Total de Estudo (horas)") +
           scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "TempoTotalDeEstudoEmHoras.png", width = 480, height = 480)
print(grafico.tempo)
dev.off()

pdf("TempoTotalDeEstudo.pdf", width = 480/100, height = 480/100)
print(grafico.tempo)
dev.off()


#


sessoes <- read.csv("dados/TableSessionLengthForadeAula.csv")


sessoes.mediana <- with(sessoes,aggregate(timeSession,list(matricula),median))
colnames(sessoes.mediana) <- c("matricula","mediana.sessao")
sessoes.mediana <- sessoes.mediana[with(sessoes.mediana,order(mediana.sessao,decreasing = TRUE)),]
sessoes.mediana$matricula <- reorder(sessoes.mediana$matricula,sessoes.mediana$mediana.sessao, order=T)
sessoes.mediana$rank = as.factor(seq(1, nrow(sessoes.mediana), 1))
sessoes.mediana$mediana.sessao <- sessoes.mediana$mediana.sessao/3600

grafico.mediana <- ggplot(sessoes.mediana,aes(rank,mediana.sessao)) + 
 	     geom_point() + theme_bw()+labs(x="Rank dos alunos",y="Mediana das Sessões de Estudo (horas)") +
           scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "MedianaSessao.png", width = 480, height = 480)
print(grafico.mediana)
dev.off()

pdf("MedianaSessao.pdf", width = 480/100, height = 480/100)
print(grafico.mediana)
dev.off()


sessoes <- with(sessoes,aggregate(session,list(matricula),max))
colnames(sessoes) <- c("matricula","num.sessoes")
sessoes <- sessoes[with(sessoes,order(num.sessoes,decreasing = TRUE)),]
sessoes$matricula <- reorder(sessoes$matricula,sessoes$num.sessoes, order=T)
sessoes$rank = as.factor(seq(1, nrow(sessoes), 1))



grafico.ses <- ggplot(sessoes,aes(rank,num.sessoes)) + 
 	     geom_point() + theme_bw()+labs(x="Rank dos alunos",y="Número de Sessões") +
           scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "NumSessoes.png", width = 480, height = 480)
print(grafico.ses)
dev.off()

pdf("NumSessoes.pdf", width = 480/100, height = 480/100)
print(grafico.ses)
dev.off()


