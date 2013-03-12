# script de pre processamento dos dados da UC1 e UC2 (parte de andré)
# Learning Analytics 2.0 - Fevereiro 2012 
# Iara Ribeiro

library(plyr)
library(foreign)

#UC2 - correlacao da nota da prova e quantidade de exercicios submetidos
#codigo da regressao

#carregando os dados
dados.exercicios = data_exerc
dados.prova.um = ajeita.provas(prova.um)
dados.prova.dois = ajeita.provas(prova.dois)
dados.prova.tres = ajeita.provas(prova.tres)


#formatacao do arquivo de log do servidor
colnames(dados.exercicios) = c("matricula","questao","turma","timestamp","resultado","nota")

dados.exercicios.um = dados.exercicios[dados.exercicios$questao < 86, ]
dados.exercicios.um = ddply(dados.exercicios.um, .(matricula), nrow)
colnames(dados.exercicios.um) = c("matricula", "numero.submissoes")

dados.exercicios.dois = dados.exercicios[dados.exercicios$questao > 85 & dados.exercicios$questao < 165, ]
dados.exercicios.dois = ddply(dados.exercicios.dois, .(matricula), nrow)
colnames(dados.exercicios.dois) = c("matricula", "numero.submissoes")

dados.exercicios.tres = dados.exercicios[dados.exercicios$questao > 164, ]
dados.exercicios.tres = ddply(dados.exercicios.tres, .(matricula), nrow)
colnames(dados.exercicios.tres) = c("matricula", "numero.submissoes")


#funcoes que formatam os dados
ajeitar.nota = function(string) {
  return(as.double(paste(substr(string, 1, 1), ".", substr(string, 3, 3), sep="")))
}

filtra.dados = function(dados) {
  df = data.frame(matricula=c(), nota=c())
  
  for (i in 1:nrow(dados))
    if (paste(dados[i, "Nota"], "", sep="") != "#VALUE!" & paste(dados[i, "Nota"], "", sep="") != "")
      df = rbind(df, data.frame(matricula=dados[i, "Matrícula"], nota=dados[i, "Nota"]))
  
  return(df)
}

ajeita.provas = function(dados) {
  dados = filtra.dados(dados)
  dados$nota = ajeitar.nota(dados[, "nota"])
  
  return(dados)
}

prepara.data.frame.provas = function(dados, limiar=7) {
  
  df = data.frame(matricula=c(), questao=c())
  
  for (i in 1:nrow(dados)) {
    q = c()
    
    if (!is.na(dados[i, "questao1"]) & dados[i, "questao1"] >= limiar)
      q[length(q) + 1] = 1
    if (!is.na(dados[i, "questao2"]) & dados[i, "questao2"] >= limiar)
      q[length(q) + 1] = 2
    if (!is.na(dados[i, "questao3"]) & dados[i, "questao3"] >= limiar)
      q[length(q) + 1] = 3
    if (!is.na(dados[i, "questao4"]) & dados[i, "questao4"] >= limiar)
      q[length(q) + 1] = 4
    
    for (j in q)
      df = rbind(df, data.frame(matricula=dados[i, "Matrícula"], questao=j))
  }
  
  return(df)
}


#calculo da correlacao
prova.exercicio.um = merge(dados.prova.um, dados.exercicios.um)
prova.exercicio.dois = merge(dados.prova.dois, dados.exercicios.dois)
prova.exercicio.tres = merge(dados.prova.tres, dados.exercicios.tres)

correlacao.prova.um = cor.test(prova.exercicio.um$nota, prova.exercicio.um$numero.submissoes, method="spearman")
correlacao.prova.dois = cor.test(prova.exercicio.dois$nota, prova.exercicio.dois$numero.submissoes, method="spearman")
correlacao.prova.tres = cor.test(prova.exercicio.tres$nota, prova.exercicio.tres$numero.submissoes, method="spearman")

correlacoes = data.frame(correlacao.prova.um=c(correlacao.prova.um$estimate),
                         correlacao.prova.dois=c(correlacao.prova.dois$estimate),
                         correlacao.prova.tres=c(correlacao.prova.tres$estimate))

						 
# armazenando os dados
write.csv(correlacoes, "correlacoes.csv", quote=F, row.names=F, col.names=F)
write.csv(prova.exercicio.um[, c("nota", "numero.submissoes")], "UC2-Prova1.csv", quote=F, row.names=F, col.names=F)
write.csv(prova.exercicio.dois[, c("nota", "numero.submissoes")], "UC2-Prova2.csv", quote=F, row.names=F, col.names=F)
write.csv(prova.exercicio.tres[, c("nota", "numero.submissoes")], "UC2-Prova3.csv", quote=F, row.names=F, col.names=F)
