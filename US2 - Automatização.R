# Codigo para automatização do LA1.0
# Andrey Menezes - versão 2.0 (Fevereiro 2013)

#dados usados
data_exerc = read.csv("../dados/exercicios-20112.csv")
data_all = read.csv("../dados/Geral.csv")

prova.um = read.csv("../dados/Prova1.csv")
prova.dois = read.csv("../dados/Prova2.csv")
prova.tres = read.csv("../dados/Prova3.csv")

#Script de automatizacao UC3 e UC4
source("../Processamento/processamento.R")

#Script de automatizacao UC1
source("../Processamento/UC1-A Priori.R")
source("../Processamento/UC1_Passo1.R")

#Script de automatizacao UC2
source("../Processamento/UC2.R")
