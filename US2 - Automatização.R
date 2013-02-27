# Codigo que cria as tabelas dos agrupamentos dos dados
# Andrey Menezes - versão 2.0 (Fevereiro 2013)

#dados usados
data_exerc = read.csv("../dados/exercicios-20112.csv")
data_all = read.csv("../dados/Geral.csv")

prova.um = read.csv("../dados/Prova1.csv")
prova.dois = read.csv("../dados/Prova2.csv")
prova.tres = read.csv("../dados/Prova3.csv")

#Script de automatiza??o UC3 e UC4
source("../Processamento/processamento.R")

#Script de automatiza??o UC1
source("../Processamento/UC1-A Priori.R")

#Script de automatiza??o UC2
source("../Processamento/UC2.R")