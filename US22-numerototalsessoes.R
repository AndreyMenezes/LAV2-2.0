library(plyr)
library(ggplot2)
library(scales)


dados = read.csv("dados/TableSessionLength.csv",header=T)
dados$data.hora<-strptime(dados$lastSubmission,"%Y-%m-%d")

df <- count(dados,"data.hora")

provas_data = c("2011-09-17","2011-10-29","2011-11-26","2011-12-03")
Avaliacao = c("Avaliação 1", "Avaliação 2","Avaliação 3","Avaliação Final")

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

grafico<- ggplot(df, aes(x=as.Date(data.hora),y=freq)) +
          geom_line()+  
	    labs(x = "Datas", y = "Número Total de Sessões") +
          scale_x_date(breaks = "2 week",labels = date_format("%d/%m"))+
          scale_y_continuous(limits=c(0,150)) +
          theme_white()+
	    geom_segment(aes(x = as.Date(provas_data), y = 0, xend = as.Date(provas_data), yend = 140, linetype =Avaliacao),size = 1.2)+
	    theme(legend.position = c(0.14, 0.8), legend.title = element_blank(),legend.text=element_text(size=10))


grafico2<- ggplot(df, aes(x=as.Date(data.hora),y=freq)) +
          geom_line()+  
	    labs(x = "Datas", y = "Número Total de Sessões") +
          scale_x_date(breaks = "2 week",labels = date_format("%d/%m"))+
          scale_y_continuous(limits=c(0,150)) +
          theme_bw()+
	    geom_segment(aes(x = as.Date(provas_data), y = 0, xend = as.Date(provas_data), yend = 140, linetype =Avaliacao),size = 1.2)+
	    theme(legend.position = c(0.125, 0.82), legend.title = element_blank(),legend.text=element_text(size=8))

png(filename = "Grafico_us7_totalSessoes.png", width = 550, height = 350)
print(grafico)
dev.off()

pdf("Grafico_us7_totalSessoes.pdf", width = 5.5, height = 3.5)
print(grafico2)
dev.off()