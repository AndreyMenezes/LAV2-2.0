# Criação de tabela de intercorrelação das variáveis independentes
# Andrey Menezes LA2.0 (Março 2013)

tabela1 = read.csv("dados/TabelaParaPerfis.csv")
tabela2 = read.csv("dados/TableSessionLength.csv") # calcular numero de sessoes
tabela3 = read.csv("dados/exercicios-20112.csv", head=F)

tabela1 = tabela1[,c(-1,-3)]
tabela2 <- with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("Matricula", "NumeroSessoes")

tabelaCorrelacao = cor(tabela1,use="complete.obs", method=c("spearman"))

write.csv(tabelaCorrelacao, "dados/TabelaIntercorrelação.csv")
