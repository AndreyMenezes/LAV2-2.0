# Codigo para analise de número de submissões pelo tempo
# Andrey Menezes - versão 2.0 (Fevereiro 2013)
library(plyr)

dados = read.csv("dados/exercicios-20112.csv", head=F)

dados = dados[,c("V1","V4")]
dados$V4 = sub(' .*', '', dados$V4)
colnames(dados) = c("matricula","data")
submission = count(dados, "data")

a = qplot(submission$data, submission$freq, geom="histogram", xlab="Tempo", ylab="Numero de submissoes")
a + geom_vline(xintercept = "9/26/2011", colour="green", linetype = "longdash")

#dados = dados[order(as.Date(dados$V4,format="%m/%d/%Y"),dados$V1,decreasing = F),]

#dados$dh = strptime(dados$V4,"%m/%d/%Y %H:%M:%S")
#dados$timestamp = as.numeric(as.POSIXct(dados$dh,origin="1970-01-01"))

#dados = dados[,c("V1","timestamp")]
#colnames(dados) = c("matricula", "timestamp")
#dados = dados[order(dados$timestamp,dados$matricula,decreasing = F),]

