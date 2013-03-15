# Codigo para analise da variação da proporção do número de questões corretas por sessão ao longo do semestre
# Iara Ribeiro - versao 2.0 (Fevereiro 2013)

exercicios.aula <- read.csv("dados/SubmissoesHorarioDeAula.csv",header=T,stringsAsFactors=F)
exercicios.aula <- exercicios.aula[exercicios.aula$nota == 10,]

sessoes.aula <- read.csv("dados/TableSessionLengthEmAula.csv", header = T)
sessoes.aula$correct.submissions = 0
sessoes.aula$timestamp <- as.numeric(as.POSIXct(strptime(sessoes.aula$data.hora,"%m/%d/%Y %H:%M:%S"),origin="1970-01-01"))

matriculas <- unique(sessoes.aula$matricula)

submissoes.corretas <- c()

for (i in 1:length(matriculas)){
  aluno <- matriculas[i]
  sessoes.aluno <- sessoes.aula[sessoes.aula$matricula == aluno, ]
  submissoes.aluno <- exercicios.aula[exercicios.aula$matricula == aluno, ]
  for(i in 0:nrow(sessoes.aluno)){
    if (i == nrow(sessoes.aluno)){
      sessao.inicio <- sessoes.aluno[i,"timestamp"]
      sessoes.aluno[i,"correct.submissions"] <- nrow(submissoes.aluno[submissoes.aluno$timestamp >= sessao.inicio, ])
    }else if(i == 1){
      sessao.inicio <- sessoes.aluno[i,"timestamp"]
      sessoes.aluno[i,"correct.submissions"] <- nrow(submissoes.aluno[submissoes.aluno$timestamp == sessao.inicio, ])
    }else{
      sessao.inicio <- sessoes.aluno[i,"timestamp"]
      sessao.fim <- sessoes.aluno[i+1,"timestamp"]
      sessoes.aluno[i+1,"correct.submissions"] <- nrow(submissoes.aluno[submissoes.aluno$timestamp >= sessao.inicio
                                                 & submissoes.aluno$timestamp < sessao.fim, ])
    }
  }
  submissoes.corretas <- rbind(submissoes.corretas, sessoes.aluno)  
}

submissoes.corretas[,"timestamp"] = sub(' .*', '', submissoes.corretas[,"data.hora"])
submissoes.corretas$proporcao.sub.corretas <- submissoes.corretas$correct.submissions/submissoes.corretas$amountSubmission

write.csv(submissoes.corretas, "dados/submissoes_corretas_tempo_aula.csv")


##############
#FORA DE AULA#
##############

exercicios.fora <- read.csv("dados/SubmissoesForaHorarioDeAula.csv",header=T,stringsAsFactors=F)
exercicios.fora <- exercicios.fora[exercicios.fora$nota == 10,]

sessoes.fora <- read.csv("dados/TableSessionLengthForadeAula.csv", header = T)
sessoes.fora$correct.submissions = 0
sessoes.fora$timestamp <- as.numeric(as.POSIXct(strptime(sessoes.fora$data.hora,"%m/%d/%Y %H:%M:%S"),origin="1970-01-01"))

matriculas <- unique(sessoes.fora$matricula)

submissoes.corretas.fora <- c()

for (i in 1:length(matriculas)){
  aluno <- matriculas[i]
  sessoes.aluno.fora <- sessoes.fora[sessoes.fora$matricula == aluno, ]
  submissoes.aluno.fora <- exercicios.fora[exercicios.fora$matricula == aluno, ]
  for(i in 0:nrow(sessoes.aluno.fora)){
    if (i == nrow(sessoes.aluno.fora)){
      sessao.inicio <- sessoes.aluno.fora[i,"timestamp"]
      sessoes.aluno.fora[i,"correct.submissions"] <- nrow(submissoes.aluno.fora[submissoes.aluno.fora$timestamp >= sessao.inicio, ])
    }else if(i == 1){
      sessao.inicio <- sessoes.aluno.fora[i,"timestamp"]
      sessoes.aluno.fora[i,"correct.submissions"] <- nrow(submissoes.aluno.fora[submissoes.aluno.fora$timestamp == sessao.inicio, ])
    }else{
      sessao.inicio <- sessoes.aluno.fora[i,"timestamp"]
      sessao.fim <- sessoes.aluno.fora[i+1,"timestamp"]
      sessoes.aluno.fora[i+1,"correct.submissions"] <- nrow(submissoes.aluno.fora[submissoes.aluno.fora$timestamp >= sessao.inicio
                                                 & submissoes.aluno.fora$timestamp < sessao.fim, ])
    }
  }
  submissoes.corretas.fora <- rbind(submissoes.corretas.fora, sessoes.aluno.fora)  
}

submissoes.corretas.fora[,"timestamp"] = sub(' .*', '', submissoes.corretas.fora[,"data.hora"])
submissoes.corretas.fora$proporcao.sub.corretas <- submissoes.corretas.fora$correct.submissions/submissoes.corretas.fora$amountSubmission

write.csv(submissoes.corretas.fora, "dados/submissoes_corretas_tempo_fora_aula.csv")
