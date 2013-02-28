# Codigo que calcula a tendencia de agrupamento dos dados
# Andrey Menezes - versão 2.0 (Fevereiro 2013)

dados = read.csv("dados/TabelaParaPerfis.csv")
dados = dados[,2:6]

library("FNN")

hopkins = function(dados, p, objetos, minimos) {
  
  amostra = sample(length(dados[,1]), size=p, replace = FALSE)
  
  soma_w = 0
  for(i in 1:length(amostra)) {
    soma_w = soma_w + minimos[i]
  }
  
  soma_u = 0
  for(i in 1:length(objetos[,1])) {
    dist = knnx.dist(as.matrix(dados),as.matrix(objetos),2,"kd_tree")
    soma_u = sum(dist[,2])
  }
  if(soma_u > 0) {
    return(soma_u/(soma_u + soma_w))
  } else {return(0)}
}

hopkins.table = data.frame()

#Calcula a menor distancia de cada linha da tabela para ser utilizado em hopkins
minimo = knn.dist(dados, k=1, algorithm="kd_tree")
stats = 0

# Monta a tabela com as estatísticas de hopkins
col=0
for(n in 1:10) {
  count=0
  col=col+1
  for(i in 1:(length(dados)-1)) {
    for(e in (i+1):length(dados)) {
      if(i != e) {
        objetos = data.frame(runif(25, min(dados[,i]), max(dados[,i])), runif(25, min(dados[,e]), max(dados[,e])))
        parcial = hopkins(dados[,c(i,e)], 25, objetos, minimo)
        if (parcial > stats) {
          stats = parcial
          atrib1 = i
          atrib2 = e
        }
        count = count+1
        hopkins.table[col,count] = parcial
      }
    }
  }
}

colnames(hopkins.table) = c("1_2", "1_3", "1_4", "1_5", "2_3", "2_4", "2_5", "3_4", "3_5", "4_5")

png(filename="BoxSplot Tendencia Agrupamento.png")
boxplot(hopkins.table)
dev.off()