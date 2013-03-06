# Criação de tabela de intercorrelação das variáveis independentes
# Andrey Menezes LA2.0 (Março 2013)

tabela1 = read.csv("dados/TabelaParaPerfis.csv")
tabela2 = read.csv("dados/TableSessionLength.csv") # calcular numero de sessoes
tabela3 = read.csv("dados/")

tabela1 = tabela1[,-3]
tabela2 <- with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("Matricula", "NumeroSessoes")
