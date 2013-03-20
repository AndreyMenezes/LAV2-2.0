#script com os codigos que geram os graficos default utilizados no relatorio do projeto
# Iara Ribeiro Versao 2.0 (Fev 2013)

library(ggplot2)
library(scales)

#data das provas
provas_data = c("2011-09-17","2011-10-29","2011-11-26","2011-12-03")
Provas = c("Prova 1", "Prova 2","Prova 3","Prova Final")

#Definindo o tema padrao em todos os plots

theme_white <- function() {
  theme_update(#tamanho do texto do eixo x e y
               axis.title = element_text(size=16),
               #tamanho do texto dos valores do eixo x e y
               axis.text = element_text(size=14),
               #removendo a caixa da legenda
               legend.title = element_blank(),
               #Tamanho do texto da legenda
               legend.title = element_text(size = 16),
               legend.text = element_text(size = 16)
               )
}

theme_set(theme_bw())
theme_white()

#para gerar o gráfico: 
#1- chame o arquivo para a variavel data.
#2- Coloque as colunas no lugar das variaveis x e y em aes(as.Date(x),y).
#3- Altere o nome dos eixos.

data <- read.csv("data.csv", header = T)
png(filename = "Grafico_us.png", width = 850, height = 480)
plot <- ggplot(data, aes(as.Date(x),y)) + geom_line()+ 
        labs(x = "Nome eixo X", y = "Nome eixo y") +
        scale_x_date(breaks = "1 week",labels = date_format("%d/%m")) + theme_white()
plot + geom_segment(aes(x = as.Date(provas_data), y = 0, xend = as.Date(provas_data), yend = 1, color =Provas), size = 1.2)
dev.off()
