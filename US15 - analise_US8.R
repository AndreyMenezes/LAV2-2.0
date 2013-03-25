# Codigo para analise da variação da proporção do número de questões corretas por sessão ao longo do semestre
# Iara Ribeiro - versao 2.0 (Fevereiro 2013)

exercicios.aula <- read.csv("dados/SubmissoesHorarioDeAula.csv",header=T,stringsAsFactors=F)
exercicios.aula <- exercicios.aula[exercicios.aula$nota == 10,]
exercicios.aula$data <- substr(exercicios.aula$dataHora,1,10)

matriculas <- unique(exercicios.aula$matricula)

sessoes.aula <- read.csv("dados/TableSessionLengthEmAula.csv", header = T)
sessoes.aula$data <- strptime(sessoes.aula$data.hora,format="%m/%d/%Y")

submissoes.corretas <- c()

for(i in 1:length(matriculas)){
  aluno <- matriculas[i]
  sessoes.aluno <- sessoes.aula[sessoes.aula$matricula == aluno, ]
  submissoes.aluno <- exercicios.aula[exercicios.aula == aluno, ]
  for( j in 1:nrow(sessoes.aluno)){
	sessoes.aluno[j,"correct.submissions"] <- nrow(submissoes.aluno[submissoes.aluno$data == sessoes.aluno[j,"data"],])
  }
  submissoes.corretas <- rbind(submissoes.corretas, sessoes.aluno)  
}

#submissoes.corretas[,"timestamp"] = sub(' .*', '', submissoes.corretas[,"data.hora"])
submissoes.corretas$proporcao.sub.corretas <- submissoes.corretas$correct.submissions/submissoes.corretas$amountSubmission
write.csv(submissoes.corretas, "dados/submissoes_corretas_tempo_aula.csv")


##############
#FORA DE AULA#
##############

exercicios.fora <- read.csv("dados/SubmissoesForaHorarioDeAula.csv",header=T,stringsAsFactors=F)
exercicios.fora <- exercicios.fora[exercicios.fora$nota == 10,]
exercicios.fora$data <- strptime(exercicios.fora$data.hora,format="%m/%d/%Y")

sessoes.fora <- read.csv("dados/TableSessionLengthForadeAula.csv", header = T,stringsAsFactors=F)
sessoes.fora$data <- strptime(sessoes.fora$data.hora,format="%m/%d/%Y")

matriculas.fora <- unique(sessoes.fora$matricula)

submissoes.corretas.fora <- c()

for(k in 1:length(matriculas.fora)){
  aluno.fora <- matriculas.fora[k]
  sessoes.aluno.fora <- sessoes.fora[sessoes.fora$matricula == aluno.fora, ]
  submissoes.aluno.fora <- exercicios.fora[exercicios.fora == aluno.fora, ]
  for( p in 1:nrow(sessoes.aluno.fora)){
    sessao.fim <- sessoes.aluno.fora[p,"timestamp"]
    sessao.inicio <- sessao.fim - sessoes.aluno.fora[p,"timeSession"]
    sub.setaluno <- subset(submissoes.aluno.fora, timestamp >= sessao.inicio)
    sub.setaluno <- subset(sub.setaluno, timestamp <= sessao.fim)
    sessoes.aluno.fora[p,"correct.submissions"] <- nrow(sub.setaluno)
  }
  submissoes.corretas.fora <- rbind(submissoes.corretas.fora, sessoes.aluno.fora)  
}

submissoes.corretas.fora$proporcao.sub.corretas <- submissoes.corretas.fora$correct.submissions/submissoes.corretas.fora$amountSubmission
max(submissoes.corretas.fora$proporcao.sub.corretas)


write.csv(submissoes.corretas.fora, "dados/submissoes_corretas_tempo_fora_aula.csv")