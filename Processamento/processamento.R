# Codigo que cria as tabelas dos agrupamentos dos dados
# Iara Ribeiro - versão 2.0 (Janeiro 2013)
library(stats)
library(plyr)

#dados usados
data_ex = data_exerc
data_geral = data_all


#excluindo os alunos que desistiram da disciplina
data_geral = data_geral[(data_geral$prova1 != 0 & data_geral$status != "reprovado por média"), ]
data_geral = data_geral[data_geral$turma.pratica != 5, ]
data_geral = data_geral[with(data_geral,order(matricula)),]


#tabelas do agrupamento
#pre-processamento - quantidade de exercicios submetidos
data_c1 = data_ex[,1:2]
data_c1 = data_c1[!duplicated(data_c1),]
data_c1$quant = 1
data_c1 = aggregate(data_c1[,3], list(data_c1[,1]), FUN = sum)
colnames(data_c1) = c("matricula","quant")
data_c1 = data_c1[is.element(data_c1[,1],data_geral$matricula), ]
data_geral = data_geral[with(data_geral,order(matricula)),]
data_c1 = cbind(data_c1, nota.final = data_geral$nota.final.pratica)
data_c1 = data_c1[with(data_c1,order(matricula)),]

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
data_c2 = data_c2[is.element(data_c2[,1],data_geral$matricula), ]
data_c2 = data_c2[with(data_c2,order(matricula)),]
data_c2 = cbind(data_c2, nota.final = data_geral$nota.final.pratica)

#normalizando os dados e agrupamento
#vec_x = (vec_x-min(vec_x)) /(max(vec_x)-min(vec_x))
data_normal = data_c1
data_normal$quant = (data_normal$quant - min(data_normal$quant)) / (max(data_normal$quant) - min(data_normal$quant))
data_normal$nota.final = (data_normal$nota.final - min(data_normal$nota.final)) / (max(data_normal$nota.final) - min(data_normal$nota.final))
agrup_atividade = kmeans(cbind(data_normal[,2], data_normal[,3]),4)
data_c1$cluster = agrup_atividade$cluster

data_normal = data_c2
data_normal$atividade = (data_normal$atividade - min(data_normal$atividade)) / (max(data_normal$atividade) - min(data_normal$atividade))
data_normal$nota.final = (data_normal$nota.final - min(data_normal$nota.final)) / (max(data_normal$nota.final) - min(data_normal$nota.final))
agrup_atividade = kmeans(cbind(data_normal[,2], data_normal[,3]),4)
data_c2$cluster = agrup_atividade$cluster

#Código que cria o csv da composição das notas.
data_comp = data.frame(matricula = data_geral$matricula)
data_comp = cbind(data_comp, nota.teoria = (data_geral$media.parcial.teoria * 0.3),
                  nota.pratica = (data_geral$media.parcial.pratica * 0.25),
                  nota.listas = (data_geral$nota.listas * 0.05), 
                  nota.provas = (data_geral$media.provas * 0.4), 
                  nota.final = data_geral$nota.final.teoria)

#salvando os dados nas tabelas

write.csv(data_c1, "AgrupamentoExercicios.csv")
write.csv(data_c2, "AgrupamentoAtividade.csv")
write.csv(data_comp, "uc4.csv")