testa.digitos <- function(digitos,padroes,taxa.ruido){

	plotdim = 2*orgdim
	plot(c(1,(plotdim[1]+5)*length(digitos)),c(1,(plotdim[2]+5)*3),
	     type="n",xlab="",ylab="")
	x = 1
	
	for (i in 1:length(padroes)) {
		padrao = padroes[[i]]

		ruido = (runif(length(padrao), 0, 1) > taxa.ruido ) * 2 - 1	
		entrada = padrao * ruido
		ret <- run.hopfield(rede, entrada, maxit = 1000)

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
