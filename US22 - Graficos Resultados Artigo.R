# Gerar os gráficos dos resultados para o artigo
# Andrey Menezes e Iury Gregory LA2.0 (Março 2013)

library(plyr)
library(ggplot2)
library(scales)

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
  theme_bw()+labs(x="Matrículas",y="Tamanho Típico das Sessões (horas)")+ #ylim(c(0,1))+
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



#Numero Total de Submissoes pelo tempo
dados2 = read.csv("dados/TableSessionLength.csv",header=T)
dados2$data.hora<-strptime(dados2$lastSubmission,"%Y-%m-%d")

df <- count(dados2,"data.hora")

provas_data = c("2011-09-17","2011-10-29","2011-11-26","2011-12-03")
Avaliacao = c("Avaliação 1", "Avaliação 2","Avaliação 3","Avaliação Final")

grafico<- ggplot(df, aes(x=as.Date(data.hora),y=freq)) +
  geom_line()+  
  labs(x = "Datas", y = "Número Total de Sessões") +
  scale_x_date(breaks = "2 week",labels = date_format("%d/%m"))+
  scale_y_continuous(limits=c(0,150)) +
  theme_bw()+
  geom_segment(aes(x=as.Date(provas_data), y=0, xend=as.Date(provas_data), yend=140, linetype=Avaliacao), size = 0.7)+
  theme(legend.position = c(0.14, 0.8), legend.title = element_blank(),legend.text=element_text(size=10))


grafico2<- ggplot(df, aes(x=as.Date(data.hora),y=freq)) +
  geom_line()+  
  labs(x = "Datas", y = "Número Total de Sessões") +
  scale_x_date(breaks = "2 week",labels = date_format("%d/%m"))+
  scale_y_continuous(limits=c(0,150)) +
  theme_bw()+
  geom_segment(aes(x=as.Date(provas_data), y=0, xend=as.Date(provas_data), yend=140, linetype=Avaliacao), size=0.7)+
  theme(legend.position = c(0.125, 0.79), legend.title = element_blank(),legend.text=element_text(size=8))

png(filename = "Grafico_us7_totalSessoes.png", width = 550, height = 350)
print(grafico)
dev.off()

pdf("Grafico_us7_totalSessoes.pdf", width = 5.5, height = 3.5)
print(grafico2)
dev.off()
