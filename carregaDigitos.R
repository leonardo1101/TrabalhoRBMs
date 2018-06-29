library(png)

carrega.digitos <- function(digitos = c(1,3,4,7,9)){
	
	############## carrega os padroes #############
	padroes <<- list()
	orgdim <<- NULL
	for (i in digitos) {
		img <- readPNG(paste("Digitos/",as.character(i),'.png',sep='')) # Le as imagens
		orgdim <<- dim(img) # Armazena as dimensoes originais
		dim(img) <- NULL
		img <- img*2 - 1   # Passa os valores para -1 e +1 (fica tudo em um vetor)
		padroes[[length(padroes)+1]] <<- img # Armazena os padroes
	}

	matriz <- matrix(unlist(padroes), ncol = 256, byrow = TRUE)

	return(matriz)
}

A <- carrega.digitos()
B <- matrix(c(
-1,-1,-1,-1,1,
-1,-1,-1,1,1,
-1,-1,1,-1,1,
-1,-1,1,1,1,
-1,1,-1,-1,1,
-1,1,-1,1,1,
-1,1,1,-1,1,
-1,1,1,1,1,
1,-1,-1,-1,1,
1,-1,-1,1,1), nrow=10,byrow=T)


