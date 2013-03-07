# Codigo para analise da variação da proporção do número de questões corretas por sessão ao longo do semestre
# Iara Ribeiro - versao 2.0 (Fevereiro 2013)

exercicios <- read.csv("dados/exercicios-20112.csv",header=F,stringsAsFactors=F)
exercicios <- exercicios[exercicios$V6 == 10, ]
exercicios$V7 <- strptime(exercicios$V4,"%m/%d/%Y %H:%M:%S")

sessoes <- read.csv("dados/TableSessionLength.csv", header = T)
sessoes$correct.submissions = 0

#adicionar a coluna com timestamp
exercicios$V7 <- as.numeric(as.POSIXct(exercicios$V7,origin="1970-01-01"))
sessoes$time.stamp <- as.numeric(as.POSIXct(sessoes$lastSubmission,origin="1970-01-01"))

matriculas <- unique(sessoes$matricula)

submissoes.corretas <- c()

for (i in 1:length(matriculas)){
  aluno <- matriculas[i]
  sessoes.aluno <- sessoes[sessoes$matricula == aluno, ]
  submissoes.aluno <- exercicios[exercicios$V1 == aluno, ]
  for(i in 0:nrow(sessoes.aluno)){
    if (i == nrow(sessoes.aluno)){
      sessao.inicio <- sessoes.aluno[i,7]
      sessoes.aluno[i,6] <- nrow(submissoes.aluno[submissoes.aluno$V7 >= sessao.inicio, ])
    }else if(i == 1){
      sessao.inicio <- sessoes.aluno[i,7]
      sessoes.aluno[i,6] <- nrow(submissoes.aluno[submissoes.aluno$V7 == sessao.inicio, ])
    }else{
      sessao.inicio <- sessoes.aluno[i,7]
      sessao.fim <- sessoes.aluno[i+1,7]
      sessoes.aluno[i+1,6] <- nrow(submissoes.aluno[submissoes.aluno$V7 >= sessao.inicio
                                                 & submissoes.aluno$V7 < sessao.fim, ])
    }
  }
  submissoes.corretas <- rbind(submissoes.corretas, sessoes.aluno)  
}

submissoes.corretas[,7] = sub(' .*', '', submissoes.corretas[,4])
submissoes.corretas$proporcao.sub.corretas <- submissoes.corretas$correct.submissions/submissoes.corretas$amountSubmission

write.csv(submissoes.corretas, "dados/submissoes_corretas_tempo.csv")
