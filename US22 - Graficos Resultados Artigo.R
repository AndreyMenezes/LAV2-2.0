# Gerar os gráficos dos resultados para o artigo
# Andrey Menezes e Iury Gregory LA2.0 (Março 2013)
require(ggplot2)

dados = read.csv("dados/TableSessionLengthForadeAula.csv")

# Numero de sessoes por aluno
tabela = with(dados,aggregate(session,list(matricula),FUN=max))
colnames(tabela) = c("matricula", "numeroSessoes")

tabela = tabela[with(tabela, order(numeroSessoes, decreasing=T)),]
tabela$matricula = reorder(tabela$matricula,tabela$numeroSessoes, order=T)
tabela$rank = as.factor(seq(1, nrow(tabela), 1))

grafico <- ggplot(tabela,aes(x=rank, y=numeroSessoes)) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Número de Sessões") + scale_x_discrete(breaks =seq(from=1,to=100,by=10)) #+ylim(c(0,max(tabela$numeroSessoes)))+
  #theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  #theme(axis.ticks=element_blank())

png("NumSessoesPorAluno.png",bg="transparent",width = 600, height = 400)
print(grafico)
dev.off()

pdf("NumSessoesPorAluno.pdf",bg="transparent", width = 480/100, height = 480/100)
print(grafico)
dev.off()

# Tamanho mediano das sessões por aluno
sessoes.mediana = with(dados, aggregate(timeSession,list(matricula),median))
colnames(sessoes.mediana) = c("matricula", "timeSession")

sessoes.mediana = sessoes.mediana[with(sessoes.mediana, order(timeSession, decreasing=T)),]
sessoes.mediana$matricula = reorder(sessoes.mediana$matricula, sessoes.mediana$timeSession, order=T)
sessoes.mediana$rank = as.factor(seq(1, nrow(sessoes.mediana), 1))

grafico <- ggplot(sessoes.mediana,aes(x=rank, y=timeSession)) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Tamanho Mediana das Sessões") + scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png("MedianaSessoesPorAluno.png",bg="transparent",width = 480, height = 480)
print(grafico)
dev.off()

pdf("MedianaSessoesPorAluno.pdf",bg="transparent", width = 480/100, height = 480/100)
print(grafico)
dev.off()


#Atividade de estudo por aluno
atividade <- read.csv("dados/AgrupamentoAtividadeForadeAula.csv")
atividade <- atividade[,-1]
atividade = atividade[with(atividade,order(atividade,decreasing=T)),]
atividade$matricula <- reorder(atividade$matricula,atividade$atividade, order=T)
atividade$rank = as.factor(seq(1, nrow(atividade), 1))

grafico.atividade <- ggplot(atividade,aes(rank,atividade)) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Atividade de Estudo") +ylim(c(0,0.75))+
  scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "AtividadedeEstudo.png", bg="transparent", width = 480, height = 480)
print(grafico.atividade)
dev.off()

pdf("AtividadeDeEstudo.pdf", bg="transparent", width = 480/100, height = 480/100)
print(grafico.atividade)
dev.off()


#Tempo total de estudo por aluno
tempo <- read.csv("dados/tableSumDisciplineForadeAula.csv")
tempo = tempo[with(tempo,order(sumSession,decreasing=T)),]
tempo$matricula <- reorder(tempo$matricula,tempo$sumSession, order=T)
tempo$rank = as.factor(seq(1, nrow(tempo), 1))


grafico.tempo <- ggplot(tempo,aes(rank,sumSession/3600)) + 
  geom_point() + theme_bw()+labs(x="Matrículas",y="Tempo Total de Estudo (horas)") +
  scale_x_discrete(breaks =seq(from=1,to=100,by=10))

png(filename = "TempoTotalDeEstudoEmHoras.png", bg="transparent", width = 480, height = 480)
print(grafico.tempo)
dev.off()

pdf("TempoTotalDeEstudo.pdf", bg="transparent", width = 480/100, height = 480/100)
print(grafico.tempo)
dev.off()
