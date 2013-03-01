# Script para gerar tabela de agrupamento de atividade para ser utilizada 
# no script US6 - Analise Corr  TempoTotalEstudo-AtividadeEstudo.R
# Iury Gregory Melo Ferreira - Versão 2.0 (Fevereiro 2013)


data_ex = read.csv("dados/exercicios-20112.csv",header=F)
#data_geral = read.csv("dados/Geral.csv")

#excluindo os alunos que desistiram da disciplina
#data_geral = data_geral[data_geral$turma.pratica != 5, ]

#pre-processamento - atividade
data_c2 = data_ex
for(i in seq(1:nrow(data_c2))){
  data_c2[i,7] = toString(data.frame(strsplit(toString(data_c2[i,4]), " "))[1,])
}

data2 = data.frame(data_c2[,1])
data2$data = data_c2$V7
data_c2 = data2[!duplicated(data2),] #remove as linhas duplicadas, as vezes o aluno manda 10 ex. no msm dia
colnames(data_c2) = c("matricula","atividade")
dias_de_aula = 125
data_c2$atividade = 1
data_c2 = aggregate(data_c2$atividade, list(data_c2$matricula), FUN=sum)
colnames(data_c2) = c("matricula","atividade")
data_c2$atividade = data_c2$atividade/dias_de_aula

write.csv(data_c2,"dados/AgrupamentoAtividade.csv")

