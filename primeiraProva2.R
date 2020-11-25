################################################
# PRIMEIRA PROVA  - PARTE 2                    #
# Aluna: Marcela Neves Belchior                #
# Matrícula: 11721BCC044                       #
# Data: 24/11/2020                             #
################################################

#-- QUESTÃO 1 ---------------------------------------------------------------#
# (a)
y <- runif(10000,0,1) 
y <- ((1/y) - 1)/(1+2*((1/y) - 1)**2+((1/y) - 1)**4) * 1/y**2
resp <- sum(e)/10000 

#-- QUESTÃO 2 ---------------------------------------------------------------#

# cara = 1; coroa = 0
moeda <- function(){
  sample(0:1,1)
}

# função responsável por lançar a moeda um número n de vezes, retorna um vetor 
# com a posição depois de cada lançamento
lancamentos <- function(n){
  pos <- 0               # posição inicial
  lanc <- c()            # vetor com as posições 
  for(i in 1:n){
    m <- moeda()         # lançamento da moeda
    if(m==0){            # se for coroa, anda para a esquerda (-1)
      pos <- pos - 1
      lanc <- c(lanc,pos)
    }else{               # se for cara, anda para a direita (+1)
      pos <- pos + 1
      lanc <- c(lanc,pos)
    }
  }
  return(lanc)
}

# função responsável por calcular a probabilidade de retornar a origem depois 
# de uma quantidade p de passos
passos <- function(p){
  l <- c() 
  sucessos <- c()
  for(i in 1:10000){    # realiza dez mil vezes o p lançamentos da moeda
    l <- lancamentos(p) # vetor lanc recebe o retorno da função lancamentos()
    if(l[p]==0)         # se a última posição do vetor for igual a zero, sucesso
      sucessos <- c(sucessos,1)
    else                # se a última posição do vetor não for igual a zero, fracasso
      sucessos <- c(sucessos,0)
  }
  
  return(sum(sucessos)/10000)
}
