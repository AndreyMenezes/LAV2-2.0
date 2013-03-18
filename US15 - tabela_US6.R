#Script US15
# Script para gerar tabela de agrupamento de atividade para ser utilizada 
# no script US6_analise.R
# Iury Gregory Melo Ferreira - Versão 2.0 (Fevereiro 2013)


data_ex = read.csv("dados/SubmissoesHorarioDeAula.csv",header=T)
data_c2 = data_ex
for(i in seq(1:nrow(data_c2))){
  data_c2[i,"data"] = toString(data.frame(strsplit(toString(data_c2[i,"data.hora"]), " "))[1,])
}

data2 = data.frame(data_c2[,"matricula"])
data2$data = data_c2$data
data_c2 = data2[!duplicated(data2),]
colnames(data_c2) = c("matricula","atividade")
dias_de_aula = 125
data_c2$atividade = 1
data_c2 = aggregate(data_c2$atividade, list(data_c2$matricula), FUN=sum)
colnames(data_c2) = c("matricula","atividade")
data_c2$atividade = data_c2$atividade/24

write.csv(data_c2,"dados/AgrupamentoAtividadeEmAula.csv")

data_fora = read.csv("dados/SubmissoesForaHorarioDeAula.csv",header=T)
data_c3 = data_fora
for(i in seq(1:nrow(data_c3))){
  data_c3[i,"data"] = toString(data.frame(strsplit(toString(data_c3[i,"data.hora"]), " "))[1,])
}

data3 = data.frame(data_c3[,1])
data3$data = data_c3$data
#data_c3 = data3[!duplicated(data3),]
colnames(data_c3) = c("matricula","atividade")
data_c3$atividade = 1
data_c3 = aggregate(data_c3$atividade, list(data_c3$matricula), FUN=sum)
colnames(data_c3) = c("matricula","atividade")
data_c3$atividade = data_c3$atividade/dias_de_aula

write.csv(data_c3,"dados/AgrupamentoAtividadeForadeAula.csv")
