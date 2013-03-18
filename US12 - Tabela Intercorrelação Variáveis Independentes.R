# Criação de tabela de intercorrelação das variáveis independentes
# Andrey Menezes LA2.0 (Março 2013)

tabela1 = read.csv("dados/TabelaParaPerfis.csv")
tabela2 = read.csv("dados/TableSessionLength.csv")
tabela3 = read.csv("dados/submissoes_corretas_tempo.csv")
tabela4 = read.csv("dados/Geral.csv")

tabela4 = tabela4[,c("matricula", "nota.final.pratica")]
tabela1 = tabela1[,-3]
tabela2 = with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("matricula", "numeroSessoes")

tabela3.calculos1 = with(tabela3,aggregate(amountSubmission,list(matricula),FUN=sum))
colnames(tabela3.calculos1) = c("matricula", "amountSubmission")
tabela3.calculos2 = with(tabela3,aggregate(correct.submissions,list(matricula),FUN=sum))
colnames(tabela3.calculos2) = c("matricula", "correctSubmissions")
tabela3 = merge(tabela3.calculos1,tabela3.calculos2,by.x="matricula",by.y="matricula")
tabela3$ProporcaoSubCorretas = tabela3$correctSubmissions/tabela3$amountSubmission

tabelaCompleta = merge(tabela1,tabela2,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela3,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela4,by.x="matricula",by.y="matricula")

colnames(tabelaCompleta) = c("matricula","Mediana Sessao","Numero Exercicios","Tempo Total Estudo","Atividade","Numero Sessoes","Total Submissoes","Submissoes Corretas","Proporcao Submissoes Corretas","Nota Final")

tabelaCorrelacao = cor(tabelaCompleta[,-1], use="complete.obs", method=c("spearman"))

write.csv(tabelaCorrelacao, "dados/TabelaIntercorrelação.csv")
