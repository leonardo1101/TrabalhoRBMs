#####################################
# Implementacao simples da rede BAM #
# Ricardo Cerri - 05/06/2018 #
#####################################

func.ativacao <- function(v){
    v[v < 0] <- -1
    v[v > 0] <- +1
    v[v == 0] <- sample(c(+1,-1),size=1)
    
    return(v)
}

bam.armazena <- function(X,Y) {
    # Matriz para armazenar os pesos
    pesos <- matrix(rep(0,ncol(X)*ncol(Y)), ncol(X), ncol(Y))
    
    for (i in 1:nrow(X)) {
        pesos <- pesos + X[i,]%o%Y[i,] # %o% = Outer product
    }
    
    return(list(pesos = pesos))
}

bam.teste <- function(M,x){
    
    H.antigo <- 100
    variacao.H <- 1
    
    while(variacao.H > 0){
    
        # Em direcao a Y
        sx <- as.vector(x %*% M)
        sx <- func.ativacao(sx)
        
        # Entropia -> quanto menor, menos confusao (melhor)
        H <- -(x %*% M %*% sx)
        
        # Em direcao a X
        x <- as.vector(M %*% sx)
        x <- func.ativacao(x) 
        
        # Variacao na Entropia
        variacao.H <- abs(H - H.antigo)
        H.antigo <- H
        
        cat("\nVariacao na Entropia = ",variacao.H,"\n")
    }
    
    cat("Saida: ")
    cat(sx,"\n")
    cat("Entrada: ")
    cat(x,"\n")
}

# Exemplo - Armazenar letras A e C
X <- matrix(c(-1,1,-1, 1,-1,1, 1,1,1, 1,-1,1, 1,-1,1,-1,1,1, 1,-1,-1, 1,-1,-1, 1,-1,-1, -1,1,1), nrow=2, byrow=T)
Y <- matrix(c(-1,1, 1,1), nrow=2,byrow=T)

# Cria a matriz de pesos
bam <- bam.armazena(X,Y)

# Testa nos próprios padrões de entrada
p <- X[1,]
q <- X[2,]
bam.teste(bam$pesos,p)

##
## Variacao na Entropia = 130
##
## Variacao na Entropia = 0
## Saida: -1 1
## Entrada: -1 1 -1 1 -1 1 1 1 1 1 -1 1 1 -1 1

bam.teste(bam$pesos,q)

##
## Variacao na Entropia = 130
##
## Variacao na Entropia = 0
## Saida: 1 1
## Entrada: -1 1 1 1 -1 -1 1 -1 -1 1 -1 -1 -1 1 1

# Faz variacoes nos padroes de entrada
p[1] <- 1
p[7] <- -1
q[2] <- -1
q[5] <- 1
q[10] <- -1
bam.teste(bam$pesos,p)

##
## Variacao na Entropia = 122
##
## Variacao na Entropia = 8
##
## Variacao na Entropia = 0
## Saida: -1 1
## Entrada: -1 1 -1 1 -1 1 1 1 1 1 -1 1 1 -1 1

bam.teste(bam$pesos,q)
##
## Variacao na Entropia = 118
##
## Variacao na Entropia = 12
##
## Variacao na Entropia = 0
## Saida: 1 1
## Entrada: -1 1 1 1 -1 -1 1 -1 -1 1 -1 -1 -1 1 1
