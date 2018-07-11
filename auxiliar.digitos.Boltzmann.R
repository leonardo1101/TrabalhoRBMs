# Funcao para carregar os digitos -> Tem variaveis globais
carrega.digitos2 <- function(digitos = c(1,3,4,7,9), taxa.ruido = 0.1){
	
	digitos <<- digitos # Padroes a serem armazenados
	taxa.ruido <<- taxa.ruido # Probabilidade de um pixel ser trocado 
	                          # (probabilidade de ruido)

	############## carrega os padroes #############
	padroes <<- list()
	orgdim <<- NULL
	for (i in digitos) {
		img <- readPNG(paste("Digitos/",as.character(i),'.png',sep='')) # Le as imagens
		orgdim <<- dim(img) # Armazena as dimensoes originais
		dim(img) <- NULL
		padroes[[length(padroes)+1]] <<- img # Armazena os padroes
	}
}



carrega.digitos <- function(digitos = c(1,3,4,7,9), quantidade = 100, 
                            treino=1, taxa.ruido = 0.1){
	
	digitos <<- digitos # Padroes a serem armazenados
	taxa.ruido <<- taxa.ruido # Probabilidade de um pixel ser trocado 
	                          # (probabilidade de ruido) 

	############## carrega os padroes #############
	padroes <<- list()
	orgdim <<- NULL

	cat("\nCarregando digitos...\n")

	for(i in digitos){

		if(treino == 1){

			arquivos <- list.files(paste("mnist_png/training/",
			                             as.character(i),"/",sep=""))[1:quantidade]

			for(j in 1:quantidade){
				img <- readPNG(paste("mnist_png/training/",
				                     as.character(i),"/",arquivos[j], sep=""))
				orgdim <<- dim(img)
				dim(img) <- NULL
				# Armazena os padroes
				padroes[[length(padroes)+1]] <<- as.vector(ifelse(img >= 0.2, 1, 0)) 
			}
		}
		else{
			arquivos <- list.files(paste("mnist_png/testing/",
			                             as.character(i),"/",sep=""))[1:quantidade]

			for(j in 1:quantidade){
				img <- readPNG(paste("mnist_png/testing/",
				                     as.character(i),"/",arquivos[j], sep=""))
				orgdim <<- dim(img)
				dim(img) <- NULL
				# Armazena os padroes
				padroes[[length(padroes)+1]] <<- as.vector(ifelse(img >= 0.2, 1, 0)) 
			}
		}
	}
}

############ Teste Visual ###########
testa.digitos <- function(modelo){

	# Carrega digitos de teste
	carrega.digitos(digitos = c(0,1,2,3,4,5,6,7,8,9),1,0)	
	
	plotdim = 2*orgdim
	plot(c(1,(plotdim[1]+5)*length(digitos)), c(1,(plotdim[2]+5)*3), 
	          type="n", xlab="", ylab="")
	x = 1

	for (i in 1:length(padroes)) {
		padrao = padroes[[i]]

		ruido = (runif(length(padrao), 0, 1) > taxa.ruido ) * 1	
		entrada = padrao * ruido
		ret <- reconstrucao(modelo,entrada)

		# Padrao original
		img <- padrao; 
		dim(img) <- orgdim
		image <- as.raster((img+1)/2)
		rasterImage(image, x, 1, x + plotdim[1], plotdim[2], interpolate=F)

		# Entrada com ruido
		img <- entrada; 
		dim(img) <- orgdim
		image <- as.raster((img+1)/2)
		rasterImage(image, x, 1+(plotdim[2]+5), x + plotdim[1],
		            1+2*(plotdim[2]+5), interpolate=F)

		# Imagem recuperada
		img <- ret; 
		dim(img) <- orgdim
		image <- as.raster((img+1)/2)
		rasterImage(image, x, 1+2*(plotdim[2]+5), x + plotdim[1],
		            1+2*(plotdim[2]+5)+plotdim[2], interpolate=F)

		x = x + plotdim[1]+5
	}
}
