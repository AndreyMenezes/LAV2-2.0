#Script que calcula a correlcao das variaveis
# Versao 1.0 - Andrey Mezes
# Versao 1.1 - Iara Ribeiro (Adicionando o calculo para as variaveis na aula e fora de aula.)

library(ggplot2)
dir = "Data/12"
#tabela1 = read.csv("dados/TabelaParaPerfisEmAula.csv")
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

tabelaCorrelacao = cor(tabelaCompleta[,-1], use="complete.obs", method=c("spearman"))
write.csv(tabelaCorrelacao, "TabelaIntercorrelacaoEmAula.csv")

#1
png(filename = paste(dir,"/Results/MedianaNumExerciciosD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NumeroExercicios)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Exercícios (log)")
dev.off()

#2
png(filename = paste(dir,"/Results/MedianaTempoTotalEstudoD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Tempo Total Estudo (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/MedianaAtividadeD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/MedianaNumeroSessoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/MedianaTotalSubmissoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/MedianaSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/MedianaProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/MedianaNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Nota Final (log)")
dev.off()




#2
png(filename = paste(dir,"/Results/NumeroExerciciosTempoTotalEstudoD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Tempo Total Estudo (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/NumeroExerciciosAtividadeD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/NumeroExerciciosNumeroSessoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/NumeroExerciciosTotalSubmissoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/NumeroExerciciosSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/NumeroExerciciosProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/NumeroExerciciosNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Nota Final (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/TempoTotalEstudoAtividadeD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/TempoTotalEstudoNumeroSessoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/TempoTotalEstudoTotalSubmissoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/TempoTotalEstudoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/TempoTotalEstudoProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/TempoTotalEstudoNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Nota Final (log)")
dev.off()




#4
png(filename = paste(dir,"/Results/AtividadeNumeroSessoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/AtividadeTotalSubmissoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/AtividadeSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/AtividadeProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/AtividadeNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Nota Final (log)")
dev.off()



#5
png(filename = paste(dir,"/Results/NumeroSessoesTotalSubmissoesD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/NumeroSessoesSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/NumeroSessoesProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/NumeroSessoesNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Nota Final (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/TotalSubmissoesSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/TotalSubmissoesProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/TotalSubmissoesNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Nota Final (log)")
dev.off()


#7
png(filename = paste(dir,"/Results/SubmissoesCorretasProporcaoSubmissoesCorretasD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(SubmissoesCorretas, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/SubmissoesCorretasNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(SubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Nota Final (log)")
dev.off()


#8
png(filename = paste(dir,"/Results/ProporcaoSubmissoesCorretasNotaFinalD.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(ProporcaoSubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Proporção de Submissões Corretas (log)",y="Nota Final (log)")
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

colnames(tabelaCompleta) = c("matricula","Mediana Sessao","Numero Exercicios","Tempo Total Estudo","Atividade","Numero Sessoes","Total Submissoes","Submissoes Corretas","Proporcao Submissoes Corretas","Nota Final")

tabelaCorrelacao = cor(tabelaCompleta[,-1], use="complete.obs", method=c("spearman"))

write.csv(tabelaCorrelacao, "TabelaIntercorrelacaoForaAula.csv")



#1
png(filename = paste(dir,"/Results/MedianaNumExerciciosF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NumeroExercicios)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Exercícios (log)")
dev.off()

#2
png(filename = paste(dir,"/Results/MedianaTempoTotalEstudoF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Tempo Total Estudo (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/MedianaAtividadeF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/MedianaNumeroSessoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/MedianaTotalSubmissoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/MedianaSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/MedianaProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/MedianaNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(MedianaSessao, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Mediana Sessão(log)",y="Nota Final (log)")
dev.off()




#2
png(filename = paste(dir,"/Results/NumeroExerciciosTempoTotalEstudoF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, TempoTotalEstudo)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Tempo Total Estudo (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/NumeroExerciciosAtividadeF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/NumeroExerciciosNumeroSessoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/NumeroExerciciosTotalSubmissoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Numero de Exercicios (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/NumeroExerciciosSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/NumeroExerciciosProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/NumeroExerciciosNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroExercicios, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="NumeroExercicios (log)",y="Nota Final (log)")
dev.off()


#3
png(filename = paste(dir,"/Results/TempoTotalEstudoAtividadeF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, Atividade)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Atividade (log)")
dev.off()

#4
png(filename = paste(dir,"/Results/TempoTotalEstudoNumeroSessoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/TempoTotalEstudoTotalSubmissoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/TempoTotalEstudoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/TempoTotalEstudoProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/TempoTotalEstudoNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TempoTotalEstudo, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Tempo Total de Estudo(log)",y="Nota Final (log)")
dev.off()




#4
png(filename = paste(dir,"/Results/AtividadeNumeroSessoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, NumeroSessoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade(log)",y="Número Sessoes(log)")
dev.off()

#5
png(filename = paste(dir,"/Results/AtividadeTotalSubmissoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/AtividadeSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/AtividadeProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/AtividadeNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(Atividade, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Atividade (log)",y="Nota Final (log)")
dev.off()



#5
png(filename = paste(dir,"/Results/NumeroSessoesTotalSubmissoesF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, TotalSubmissoes)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Total de Submissões (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/NumeroSessoesSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/NumeroSessoesProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/NumeroSessoesNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(NumeroSessoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x=" Número de Sessões (log)",y="Nota Final (log)")
dev.off()

#6
png(filename = paste(dir,"/Results/TotalSubmissoesSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, SubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Número de Submissões Corretas(log)")
dev.off()

#7
png(filename = paste(dir,"/Results/TotalSubmissoesProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/TotalSubmissoesNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(TotalSubmissoes, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Total de Submissões (log)",y="Nota Final (log)")
dev.off()


#7
png(filename = paste(dir,"/Results/SubmissoesCorretasProporcaoSubmissoesCorretasF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(SubmissoesCorretas, ProporcaoSubmissoesCorretas)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Proporção de Submissões Corretas(log)")
dev.off()

#8
png(filename = paste(dir,"/Results/SubmissoesCorretasNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(SubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Número de Submissões Corretas (log)",y="Nota Final (log)")
dev.off()


#8
png(filename = paste(dir,"/Results/ProporcaoSubmissoesCorretasNotaFinalF.png",sep=""), width=240, height= 240 )
ggplot(tabelaCompleta,aes(ProporcaoSubmissoesCorretas, NotaFinal)) + 
    geom_point() + geom_smooth(method=lm,se=FALSE) + theme_bw() +
    scale_x_log10() + scale_y_log10() + labs(x="Proporção de Submissões Corretas (log)",y="Nota Final (log)")
dev.off()
