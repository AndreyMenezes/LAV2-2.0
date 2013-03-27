# Gerar os gráficos dos resultados para o artigo
# Andrey Menezes e Iury Gregory LA2.0 (Março 2013)
require(ggplot2)

dados = read.csv("dados/TableSessionLength.csv")

# Numero de sessoes por aluno
tabela = with(dados,aggregate(session,list(matricula),FUN=max))
colnames(tabela) = c("matricula", "numeroSessoes")

tabela = tabela[with(tabela, order(numeroSessoes, decreasing=T)),]
tabela$matricula = reorder(tabela$matricula,tabela$numeroSessoes, order=T)
tabela$rank = as.factor(seq(1, nrow(tabela), 1))

grafico <- ggplot(tabela, aes(x=rank, y=numeroSessoes, shape="Aluno")) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Número de Sessões")+
  scale_x_discrete(breaks =seq(from=0,to=100,by=10))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())

png("NumSessoesPorAluno.png",bg="transparent",width = 550, height = 350)
print(grafico)
dev.off()

pdf("NumSessoesPorAluno.pdf",bg="transparent", width = 5.5, height = 3.5)
print(grafico)
dev.off()


# Tamanho mediano das sessões por aluno
sessoes.mediana = with(dados, aggregate(timeSession,list(matricula),median))
colnames(sessoes.mediana) = c("matricula", "timeSession")

sessoes.mediana = sessoes.mediana[with(sessoes.mediana, order(timeSession, decreasing=T)),]
sessoes.mediana$matricula = reorder(sessoes.mediana$matricula, sessoes.mediana$timeSession, order=T)
sessoes.mediana$rank = as.factor(seq(1, nrow(sessoes.mediana), 1))

grafico.mediana <- ggplot(sessoes.mediana, aes(x=rank, y=(timeSession/3600), shape="Aluno")) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Tamanho Mediano das Sessões (horas)")+ #ylim(c(0,1))+
  scale_x_discrete(breaks =seq(from=0,to=100,by=10))+
  scale_y_continuous(breaks =seq(from=0,to=1,by=0.15))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())

png("MedianaSessoesPorAluno.png",bg="transparent",width = 550, height = 350)
print(grafico.mediana)
dev.off()

pdf("MedianaSessoesPorAluno.pdf",bg="transparent", width = 5.5, height = 3.5)
print(grafico.mediana)
dev.off()


#Atividade de estudo por aluno
atividade <- read.csv("dados/AgrupamentoAtividade.csv")
atividade <- atividade[,-1]
atividade = atividade[with(atividade,order(atividade,decreasing=T)),]
atividade$matricula <- reorder(atividade$matricula,atividade$atividade, order=T)
atividade$rank = as.factor(seq(1, nrow(atividade), 1))

grafico.atividade <- ggplot(atividade, aes(x=rank, y=atividade, shape="Aluno")) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Atividade de Estudo") +ylim(c(0,0.8))+
  scale_x_discrete(breaks =seq(from=0,to=100,by=10))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())+

png(filename = "AtividadedeEstudo.png", bg="transparent", width = 550, height = 350)
print(grafico.atividade)
dev.off()

pdf("AtividadeDeEstudo.pdf", bg="transparent", width = 5.5, height = 3.5)
print(grafico.atividade)
dev.off()


#Tempo total de estudo por aluno
tempo <- read.csv("dados/tableSumDiscipline.csv")
tempo = tempo[with(tempo,order(sumSession,decreasing=T)),]
tempo$matricula <- reorder(tempo$matricula,tempo$sumSession, order=T)
tempo$rank = as.factor(seq(1, nrow(tempo), 1))


grafico.tempo <- ggplot(tempo, aes(x=rank, y=(sumSession/3600), shape="Aluno")) + geom_point()+
  theme_bw()+labs(x="Matrículas",y="Tempo Total de Estudo (horas)")+
  scale_x_discrete(breaks =seq(from=0,to=100,by=10))+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.title=element_blank())

png(filename = "TempoTotalDeEstudoEmHoras.png", bg="transparent", width = 550, height = 350)
print(grafico.tempo)
dev.off()

pdf("TempoTotalDeEstudoEmHoras.pdf", bg="transparent", width = 5.5, height = 3.5)
print(grafico.tempo)
dev.off()
