################################################
# PRIMEIRA PROVA  - PARTE 2                    #
# Aluna: Marcela Neves Belchior                #
# Matr�cula: 11721BCC044                       #
# Data: 24/11/2020                             #
################################################

#-- QUEST�O 1 ---------------------------------------------------------------#
# (a)
y <- runif(10000,0,1) 
y <- ((1/y) - 1)/(1+2*((1/y) - 1)**2+((1/y) - 1)**4) * 1/y**2
resp <- sum(e)/10000 

#-- QUEST�O 2 ---------------------------------------------------------------#

# cara = 1; coroa = 0
moeda <- function(){
  sample(0:1,1)
}

# fun��o respons�vel por lan�ar a moeda um n�mero n de vezes, retorna um vetor 
# com a posi��o depois de cada lan�amento
lancamentos <- function(n){
  pos <- 0               # posi��o inicial
  lanc <- c()            # vetor com as posi��es 
  for(i in 1:n){
    m <- moeda()         # lan�amento da moeda
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

# fun��o respons�vel por calcular a probabilidade de retornar a origem depois 
# de uma quantidade p de passos
passos <- function(p){
  l <- c() 
  sucessos <- c()
  for(i in 1:10000){    # realiza dez mil vezes o p lan�amentos da moeda
    l <- lancamentos(p) # vetor lanc recebe o retorno da fun��o lancamentos()
    if(l[p]==0)         # se a �ltima posi��o do vetor for igual a zero, sucesso
      sucessos <- c(sucessos,1)
    else                # se a �ltima posi��o do vetor n�o for igual a zero, fracasso
      sucessos <- c(sucessos,0)
  }
  
  return(sum(sucessos)/10000)
}
