library(png)
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
        retorno <- autoencoder.propagacao(modelo,entrada)
		ret <- retorno$y.escondida.saida
		ret <- as.vector(ifelse(ret >= 0.5,1,0))

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



#Duncao de ativação
funcao.ativacao <- function(v){
	return(1 / (1+exp(-v)))
}

# Derivada da funcao de ativacao
der.funcao.ativacao <- function(y){
	return(y*(1 - y))
}
#Arquitetura
arquitetura <-function(num.entrada,num.escondida,funcao.ativacao,der.funcao.ativacao){
	arq <- list()
	arq$num.entrada.saida <- num.entrada
	arq$num.escondida <- num.escondida
	arq$funcao.ativacao <- funcao.ativacao
	arq$der.funcao.ativacao <- der.funcao.ativacao
	
	#Pesos conectando camada de entrada e escondida
	num.pesos.entrada.escondida <- (num.entrada +1) * num.escondida
	arq$escondida <- matrix(runif(min=-0.5,max=0.5, num.pesos.entrada.escondida),nrow=num.escondida,ncol=num.entrada +1)
	
	#Pesos conectando camada escondida e saida
	num.pesos.escondida.saida <- (num.escondida + 1)*num.entrada
	arq$saida <- matrix(runif(min=-0.5,max=0.5,num.pesos.escondida.saida),nrow=num.entrada,ncol=num.escondida + 1 )

	return(arq)
}

autoencoder <- function(arq,dados,n,limiar){
	loss <- 10
	loss.anterior <- 0
	epocas <- 0

	#Treino entre a diferenta entre os erros for maior que limiar
	while (abs(loss - loss.anterior) > limiar){

		loss.anterior <- loss
		loss <- 0
		
		# Treino para todos os exemplos (epoca)
		for(i in 1:length(dados)){
            # Pego um exemplo de entrada
			x <- dados[[i]]

			# Pego a saida da rede para o exemplo
			resultado <- autoencoder.propagacao(arq,x)
			y <- resultado$y.escondida.saida

			# Calculo do erro para o exemplo
            erro <- x - y
			loss <- loss + - (sum(x*log(y) + (1-x)*log(1-y)))
			
            # Gradiente local no neuronio de saida
            # erro * derivada da funcao de ativacao
			grad.local.saida <- erro * arq$der.funcao.ativacao(y)


            # Gradiente local no neuronio escondido
            # derivada da funcao de ativacao no neuronio escondido * soma dos gradientes
            # locais dos neuronios conectados na proxima camada * pesos conectando a camada
			pesos.saida <- arq$saida[,1:arq$num.escondida]
			
			grad.local.escondida <- as.numeric(arq$der.funcao.ativacao(resultado$y.entrada.escondida)) * (as.vector(grad.local.saida) %*% pesos.saida)

			# Ajuste de pesos
			arq$saida <- arq$saida + n * (grad.local.saida %*% c(resultado$y.entrada.escondida, 1))
			arq$escondida <- arq$escondida + n * (t(grad.local.escondida) %*% as.numeric(c(x,1)))
		}

		
		loss <- loss / length(dados)
		
		cat("Epocas = ", epocas,"/ Erro = ", loss, "/", abs(loss-loss.anterior),"\n")
		epocas <- epocas + 1
		
	}
	cat("Done ;D \n")

	retorno <- list()
	retorno$arq <- arq
    retorno$epocas <- epocas

	return(retorno)
}

autoencoder.propagacao <- function(arq,exemplo){

	# Entrada -> Escondida
	v.entrada.escondida <- arq$escondida %*% as.numeric(c(exemplo,1))
	y.entrada.escondida <- arq$funcao.ativacao(v.entrada.escondida)

	# Escondida -> Saida
	v.escondida.saida <- arq$saida %*% c(y.entrada.escondida,1)
	y.escondida.saida <- arq$funcao.ativacao(v.escondida.saida)

	resultado <- list()
	resultado$v.entrada.escondida <- v.entrada.escondida
	resultado$y.entrada.escondida <- y.entrada.escondida
	resultado$v.escondida.saida <- v.escondida.saida
	resultado$y.escondida.saida <- y.escondida.saida
	return(resultado)
}

#Carrega digitos

carrega.digitos(digitos=c(0,1,2,3,4,5,6,7,8,9), 10, 1, 0.3)

arq <- arquitetura(length(padroes[[1]]),length(padroes[[1]])+60,funcao.ativacao,der.funcao.ativacao)
modelo <- autoencoder(arq,padroes,0.2,0.1)
