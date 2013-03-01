# Codigo que calcula a tendencia de agrupamento dos dados
# Andrey Menezes - versão 2.0 (Fevereiro 2013)

# Tabela normalizada e com as características correlatas eliminadas
dados = read.csv("dados/TabelaParaPerfisNormal.csv")
caracteristicas = dados[,-1]

# Funções para calcular a estatística de hoopkins
intervalo.confianca<-function(dados, significancia = 0.05){
  tamanho = length(dados)
  confianca = 1-(significancia/2)
  desvio.padrao = sd(dados)
  media = mean(dados)
  erro <- qnorm(confianca)*desvio.padrao/sqrt(tamanho)
  return(data.frame(media - erro, media + erro, media))
}

distancia = function(ponto1, ponto2){
  resultado = 0
  for (i in 1:ncol(ponto1)){
    resultado = resultado + ((ponto1[,i] - ponto2[,i])^2)
  }
  resultado = sqrt(resultado)
}

ponto.mais.proximo = function(ponto, dados){
  distancia = distancia(ponto, dados[1,])
  for (i in 2:nrow(dados)){
    d = distancia(ponto, dados[i,])
    if (!(d==0) && d < distancia){
      distancia = d
    }
  }
  return(distancia)
}

gerar.pontos = function(dados, tamanho){
  pontos = NA
  for (i in 1:ncol(dados)){
    sample = sample(0:100, tamanho)/100
    pontos = cbind(pontos, sample)
  }
  pontos = pontos[,-1]
  colnames(pontos) = colnames(dados)
  return(pontos)
  
}

amostragem = function(dados, tamanho){
  sample = sample(nrow(dados), tamanho)
  amostra = dados[sample,]
}

hopkins = function(dados, n){
  amostra = amostragem(caracteristicas, n)  
  novos.pontos = as.data.frame(gerar.pontos(caracteristicas, n))
  
  w = 0
  u = 0
  
  for (i in 1:n){
    w = w + ponto.mais.proximo(amostra[i,], dados)
    u = u + ponto.mais.proximo(novos.pontos[i,], dados)
  }
  
  hopkins = u/(u+w)
}

hopkins.para.combinacoes = function(caracteristicas, combinacoes){
  intervalos = NA
  for (i in 1:ncol(combinacoes)){
    valores = 0
    for (j in 1:50){
      valor = hopkins(caracteristicas[combinacoes[,i],], 10)
      valores = c(valores, valor)
    }
    valores = valores[2:length(valores)]
    ic = intervalo.confianca(valores)
    
    intervalos = rbind(intervalos, data.frame(Atributos=paste(colnames(caracteristicas[combinacoes[,i]]),collapse="+"),
                                                Hopkins=ic))
  }
  
  intervalos = intervalos[-1,]
}
#

combinacoes = combn(ncol(caracteristicas),3)

intervalos = hopkins.para.combinacoes(caracteristicas, combinacoes)