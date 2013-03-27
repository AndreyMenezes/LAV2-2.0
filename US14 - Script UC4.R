# Expansão da tabela UC4.csv para gerar informações do hover
# Andrey Menezes LA2.0 (Março 2013)

tabela1 = read.csv("dados/TabelaParaPerfis.csv")
tabela2 = read.csv("dados/TableSessionLength.csv")
tabela3 = read.csv("dados/submissoes_corretas_tempo.csv")
tabela4 = read.csv("dados/Geral.csv")
uc4 = read.csv("dados/uc4.csv")

tabela1 = tabela1[,c("matricula","mediana.sessao","tempo.total.estudo")]

tabela2 = with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("matricula", "numeroSessoes")

tabela3.calculos1 = with(tabela3,aggregate(amountSubmission,list(matricula),FUN=sum))
colnames(tabela3.calculos1) = c("matricula", "amountSubmission")
tabela3.calculos2 = with(tabela3,aggregate(correct.submissions,list(matricula),FUN=sum))
colnames(tabela3.calculos2) = c("matricula", "correctSubmissions")
tabela3 = merge(tabela3.calculos1,tabela3.calculos2,by.x="matricula",by.y="matricula")
tabela3$ProporcaoSubCorretas = tabela3$correctSubmissions/tabela3$amountSubmission
tabela3 = tabela3[,c("matricula","ProporcaoSubCorretas")]

tabela4 = tabela4[,c("matricula", "status")]

uc4 = uc4[,-1]
uc4$nota.final = (uc4$nota.teoria + uc4$nota.pratica + uc4$nota.listas + uc4$nota.provas)

tabelaCompleta = merge(uc4,tabela1,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela2,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela3,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela4,by.x="matricula",by.y="matricula")

tabelaUC4 = tabelaCompleta[with(tabelaCompleta, order(nota.final, decreasing=T)),]

write.csv(tabelaUC4, "dados/tabelaUC4.csv")