dados = read.csv("dados/SubmissoesHorarioDeAula.csv")
dados <- subset(dados,select=data.hora)
dados$x<-1
dados$data.hora<-strptime(dados$data.hora,"%m/%d/%Y")
fix(dados)
dados <-aggregate(dados$x, list("data.hora" = dados$data.hora), FUN = sum)



library(ggplot2)
library(scales)

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


png(filename = "Grafico_us7.png", width = 850, height = 480)
plot <- ggplot(dados, aes(as.Date(data.hora),x)) + geom_line()+ 
        labs(x = "Datas", y = "Número de Submissões") +
        scale_x_date(breaks = "1 week",labels = date_format("%d/%m")) + theme_white()

plot
dev.off()
