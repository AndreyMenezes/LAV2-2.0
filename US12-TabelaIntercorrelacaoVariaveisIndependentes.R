#Script que calcula a correlcao das variaveis
# Versao 1.0 - Andrey Mezes
# Versao 1.1 - Iara Ribeiro (Adicionando o calculo para as variaveis na aula e fora de aula.)

library(ggplot2)
dir= "Data/12"
tabela1 = read.csv("dados/TabelaParaPerfisEmAula.csv")
tabela2 = read.csv("dados/TableSessionLengthEmAula.csv")
tabela3 = read.csv("dados/submissoes_corretas_tempo_aula.csv")
tabela4 = read.csv("dados/Geral.csv")

tabela4 = tabela4[,c("matricula", "nota.final.pratica")]
tabela1 = tabela1[,-3]
tabela2 = with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("matricula", "numeroSessoes")

tabela3.calculos1 = with(tabela3,aggregate(amountSubmission,list(matricula),FUN=sum))
colnames(tabela3.calculos1) = c("matricula", "amountSubmission")
tabela3.calculos2 = with(tabela3,aggregate(correct.submissions,list(matricula),FUN=sum))
colnames(tabela3.calculos2) = c("matricula", "correctSubmissions")
tabela3 = merge(tabela3.calculos1,tabela3.calculos2,by.x="matricula",by.y="matricula")
tabela3$ProporcaoSubCorretas = tabela3$correctSubmissions/tabela3$amountSubmission

tabelaCompleta = merge(tabela1,tabela2,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela3,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela4,by.x="matricula",by.y="matricula")

colnames(tabelaCompleta) = c("matricula","MedianaSessao","NumeroExercicios","TempoTotalEstudo","Atividade","NumeroSessoes","TotalSubmissoes","SubmissoesCorretas","ProporcaoSubmissoesCorretas","NotaFinal")
tabelaCompleta$MedianaSessao <- tabelaCompleta$MedianaSessao + 1


tabelaCorrelacao = cor(tabelaCompleta[,-1], use="complete.obs", method=c("spearman"))
write.csv(tabelaCorrelacao, "TabelaIntercorrelacaoEmAula.csv")

#1
png(filename = paste(dir,"/Results/Images/MedianaNumExerciciosD.png",sep=""), width=240, height= 240, units = "px" )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NumeroExercicios)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + 
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Exercícios (log)") +theme_bw())
dev.off()



#2
png(filename = paste(dir,"/Results/Images/MedianaTempoTotalEstudoD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Tempo Total Estudo (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/MedianaAtividadeD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=F) + theme_bw() +
    scale_x_log10() + scale_y_log10(limits=c(0.01,1)) + labs(x="Mediana Sessão(log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/MedianaNumeroSessoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10(limits=c(1,100)) + labs(x="Mediana Sessão(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/MedianaTotalSubmissoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/MedianaSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/MedianaProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10(limits=c(0.1,1)) + labs(x="Mediana Sessão(log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/MedianaNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Nota Final (log)"))
dev.off()




#2
png(filename = paste(dir,"/Results/Images/NumeroExerciciosTempoTotalEstudoD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Tempo Total Estudo (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/NumeroExerciciosAtividadeD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(1,1000)) + scale_y_log10(limits=c(0.1,1)) + labs(x="Numero de Exercicios (log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/NumeroExerciciosNumeroSessoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/NumeroExerciciosTotalSubmissoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/NumeroExerciciosSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/NumeroExerciciosProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10(limits=c(0.1,1)) + labs(x="NumeroExercicios (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/NumeroExerciciosNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Nota Final (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoAtividadeD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,100)) + scale_y_log10(limits=c(0.1,1)) + labs(x="Tempo Total de Estudo(log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoNumeroSessoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoTotalSubmissoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Nota Final (log)"))
dev.off()




#4
png(filename = paste(dir,"/Results/Images/AtividadeNumeroSessoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,1)) + scale_y_log10(limits=c(1,100)) + labs(x="Atividade(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/AtividadeTotalSubmissoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,1)) + scale_y_log10(limits=c(1,1000)) + labs(x="Atividade (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/AtividadeSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,1)) + scale_y_log10(limits=c(1,100)) + labs(x="Atividade (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/AtividadeProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,1)) + scale_y_log10(limits=c(0.1,1)) + labs(x="Atividade (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/AtividadeNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10(limits=c(0.1,1)) + scale_y_log10(limits=c(0.1,10)) + labs(x="Atividade (log)",y="Nota Final (log)"))
dev.off()



#5
png(filename = paste(dir,"/Results/Images/NumeroSessoesTotalSubmissoesD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/NumeroSessoesSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/NumeroSessoesProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/NumeroSessoesNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Nota Final (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/TotalSubmissoesSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/TotalSubmissoesProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/TotalSubmissoesNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Nota Final (log)"))
dev.off()


#7
png(filename = paste(dir,"/Results/Images/SubmissoesCorretasProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(SubmissoesCorretas, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/SubmissoesCorretasNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(SubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Nota Final (log)"))
dev.off()


#8
png(filename = paste(dir,"/Results/Images/ProporcaoSubmissoesCorretasNotaFinalD.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(ProporcaoSubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Proporção de Submissões Corretas (log)",y="Nota Final (log)"))
dev.off()









#Fora de aula

tabela1 = read.csv("dados/TabelaParaPerfisForadeAula.csv")
tabela2 = read.csv("dados/TableSessionLengthForadeAula.csv")
tabela3 = read.csv("dados/submissoes_corretas_tempo_fora_aula.csv")
tabela4 = read.csv("dados/Geral.csv")

tabela4 = tabela4[,c("matricula", "nota.final.pratica")]
tabela1 = tabela1[,-3]
tabela2 = with(tabela2,aggregate(session,list(matricula),FUN=max))
colnames(tabela2) = c("matricula", "numeroSessoes")

tabela3.calculos1 = with(tabela3,aggregate(amountSubmission,list(matricula),FUN=sum))
colnames(tabela3.calculos1) = c("matricula", "amountSubmission")
tabela3.calculos2 = with(tabela3,aggregate(correct.submissions,list(matricula),FUN=sum))
colnames(tabela3.calculos2) = c("matricula", "correctSubmissions")
tabela3 = merge(tabela3.calculos1,tabela3.calculos2,by.x="matricula",by.y="matricula")
tabela3$ProporcaoSubCorretas = tabela3$correctSubmissions/tabela3$amountSubmission

tabelaCompleta = merge(tabela1,tabela2,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela3,by.x="matricula",by.y="matricula")
tabelaCompleta = merge(tabelaCompleta,tabela4,by.x="matricula",by.y="matricula")

colnames(tabelaCompleta) = c("matricula","MedianaSessao","NumeroExercicios","TempoTotalEstudo","Atividade","NumeroSessoes","TotalSubmissoes","SubmissoesCorretas","ProporcaoSubmissoesCorretas","NotaFinal")
tabelaCompleta$MedianaSessao <- tabelaCompleta$MedianaSessao +1
tabelaCorrelacao = cor(tabelaCompleta[,-1], use="complete.obs", method=c("spearman"))

write.csv(tabelaCorrelacao, "TabelaIntercorrelacaoForaAula.csv")


#1
png(filename = paste(dir,"/Results/Images/MedianaNumExerciciosF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NumeroExercicios)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Exercícios (log)"))
dev.off()


#2
png(filename = paste(dir,"/Results/Images/MedianaTempoTotalEstudoF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Tempo Total Estudo (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/MedianaAtividadeF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/MedianaNumeroSessoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/MedianaTotalSubmissoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/MedianaSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/MedianaProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/MedianaNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(MedianaSessao, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Nota Final (log)"))
dev.off()




#2
png(filename = paste(dir,"/Results/Images/NumeroExerciciosTempoTotalEstudoF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Tempo Total Estudo (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/NumeroExerciciosAtividadeF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/NumeroExerciciosNumeroSessoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/NumeroExerciciosTotalSubmissoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/NumeroExerciciosSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/NumeroExerciciosProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/NumeroExerciciosNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroExercicios, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Nota Final (log)"))
dev.off()


#3
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoAtividadeF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Atividade (log)"))
dev.off()

#4
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoNumeroSessoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoTotalSubmissoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/TempoTotalEstudoNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TempoTotalEstudo, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Nota Final (log)"))
dev.off()




#4
png(filename = paste(dir,"/Results/Images/AtividadeNumeroSessoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade(log)",y="Número Sessoes(log)"))
dev.off()

#5
png(filename = paste(dir,"/Results/Images/AtividadeTotalSubmissoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/AtividadeSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/AtividadeProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/AtividadeNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(Atividade, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Nota Final (log)"))
dev.off()



#5
png(filename = paste(dir,"/Results/Images/NumeroSessoesTotalSubmissoesF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Total de Submissões (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/NumeroSessoesSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/NumeroSessoesProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/NumeroSessoesNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(NumeroSessoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Nota Final (log)"))
dev.off()

#6
png(filename = paste(dir,"/Results/Images/TotalSubmissoesSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Número de Submissões Corretas(log)"))
dev.off()

#7
png(filename = paste(dir,"/Results/Images/TotalSubmissoesProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/TotalSubmissoesNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(TotalSubmissoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Nota Final (log)"))
dev.off()


#7
png(filename = paste(dir,"/Results/Images/SubmissoesCorretasProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(SubmissoesCorretas, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Proporção de Submissões Corretas(log)"))
dev.off()

#8
png(filename = paste(dir,"/Results/Images/SubmissoesCorretasNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(SubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Nota Final (log)"))
dev.off()


#8
png(filename = paste(dir,"/Results/Images/ProporcaoSubmissoesCorretasNotaFinalF.png",sep=""), width=240, height= 240 )
print(ggplot(tabelaCompleta,aes(ProporcaoSubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Proporção de Submissões Corretas (log)",y="Nota Final (log)"))
dev.off()