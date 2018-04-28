# Hausaufgabe 5 

# Diffusionsprozesse. 
 setwd("\\\\fs.univie.ac.at\\homedirs\\a1277687\\Desktop\\NA für finanzmathematik")

ITO_SIM_PR <- function( iter = 100, anz_perioden = 50, 
                          anfang = 1.05, anz_schritte=900, sigma=0.05, mlt= 0.7){
		
	 	
	
     
	 random_vals <- matrix(rnorm(iter*anz_schritte*anz_perioden), ncol=iter)
	 X <- matrix(NA_real_, ncol = iter, nrow = anz_schritte*anz_perioden + 1)
	 X[1,] <- anfang
	 
	 for(i in 2:nrow(X)) {
		 X[i, ] <- X[i-1, ] +(random_vals[i-1, ]*sqrt(X[(i-1),])/sqrt(anz_schritte))*sigma+mlt*(6-2*(X[(i-1),]))/anz_schritte
	 }
	 return(X)
}
 
 
 # Jetzt simulieren  wir unseren Prozess mit verschieden Parametern
 png("ITO_Prozess_mit_Parametern_sigma=0.05anfang=3, mlt=0.7.png", width= 1280, height= 980)
idp<- ITO_SIM_PR( anfang=3, sigma =0.05, mlt= 0.7 )
matplot(seq(0,10, le=45001), idp, col=rainbow(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=0.05, anfang=3, mlt=0.7" ,xlab= "T", ylab= "Wert", cex=4)

dev.off()

 png("ITO_Prozessmit_Parametern_sigma=0.5anfang=3.png", width= 1280, height= 980)
 
idp<- ITO_SIM_PR( anfang=3, sigma =0.5, mlt= 0.7)
matplot(seq(0,10, le=45001), idp, col=rainbow(100), type="l", lty=1, main= "ITO Prozess mit Parametern: sigma=0.5, anfang=3mlt= 0.7" ,xlab= "T", ylab= "Wert", cex=4)

dev.off()

png("ITO_Prozessmit_Parametern_sigma=0.8_anfang=3,mlt= 0.7.png", width= 1280, height= 980)
idp<- ITO_SIM_PR( anfang=3, sigma =0.8,mlt= 0.7)
matplot(seq(0,10, le=45001), idp, col=rainbow(100), type="l", lty=1, main= "ITO Prozess mit Parametern: sigma=0.8, anfang=3, mlt= 0.7" ,xlab= "T", ylab= "Wert", cex=4)

dev.off()

png("ITO_Prozessmit_Parametern_sigma=1_anfang=3.png", width= 1280, height= 980)
idp<- ITO_SIM_PR( anfang=3, sigma =1, mlt= 0.7)
matplot(seq(0,10, le=45001), idp, col=rainbow(100), type="l", lty=1, main= "ITO Prozess mit Parametern: sigma=1, anfang=3,  mlt= 0.7" ,xlab= "T", ylab= "Wert", cex=4)
 dev.off()

 
 
 png("ITO_Prozessmit_Parametern_sigma=2_anfang=3.png", width= 1280, height= 980)
idp<- ITO_SIM_PR( anfang=3, sigma =2, mlt= 0.7)
matplot(seq(0,10, le=45001), idp, col=rainbow(100), type="l", lty=1, main= "ITO Prozess mit Parametern: sigma=2, anfang=3, mlt= 0.7" ,xlab= "T", ylab= "Wert", cex=4)

dev.off()
x11(1280, 960) 






ITO_SIM_PR <- function( iter = 100, anz_perioden = 50, 
                          anfang = 1.05,anz_schritte=900, sigma=0.05, Multiplikator=0.7){
		
	 	
	
     
	 random_vals <- matrix(rnorm(iter*anz_schritte*anz_perioden), ncol=iter)
	 X <- matrix(NA_real_, ncol = iter, nrow = anz_schritte*anz_perioden + 1)
	 X[1,] <- anfang
	 
	
	 for(i in 2:nrow(X)) {
		 X[i, ] <- X[i-1, ] + random_vals[i-1, ]*sqrt(X[(i-1),])/sqrt(anz_schritte)*sigma+Multiplikator*(6-2*X[(i-1),]+cos((i-1)/anz_schritte))/anz_schritte
	 }
	 return(X)
}

 # Jetzt simulieren  wir unseren Prozess mit verschieden Parametern

 png("ITO_Prozessmit_Parametern2_sigma=1_anfang=3_MP=0.7.png", width= 1280, height= 980)
 
idp<- ITO_SIM_PR(anfang =3,sigma=1)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1, anfang=3, Multiplikator=0.7.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()


 png("ITO_Prozessmit_Parametern2_sigma=1.5_anfang=3_MP=0.7.png",width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=1.5)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1.5, anfang=3,Multiplikator=0.7.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()

 png("ITO_Prozessmit_Parametern2_sigma=0.2_anfang=3_MP=0.7.png", width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=0.2)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=0.2, anfang=3,Multiplikator=0.7.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()




 png("ITO_Prozessmit_Parametern2_sigma=0.05_anfang=3_MP=0.7.png", width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=0.05)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=0.05, anfang=3, Multiplikator=0.7.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()



png("ITO_Prozessmit_Parametern2_sigma=1_anfang=3_MP=10.png", width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=1, Multiplikator=10)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1, anfang=3, Multiplikator=1.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()


png("ITO_Prozessmit_Parametern2_sigma=1_anfang=3_MP=2.png", width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=1, Multiplikator=2)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1, anfang=3, Multiplikator=2.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()



png("ITO_Prozessmit_Parametern2_sigma=1_anfang=3_MP=0.1.png", width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=1, Multiplikator=0.1)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1, anfang=3, Multiplikator=0.1.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()



png("ITO_Prozessmit_Parametern2_sigma=1_anfang=3_MP=0.2.png",width= 1280, height= 980)
idp<- ITO_SIM_PR(anfang =3,sigma=1, Multiplikator=0.2)

matplot(seq(0,50, le=45001), idp, col=topo.colors(100), type="l", lty=1, main= " ITO Prozess mit Parametern: sigma=1, anfang=3, Multiplikator=0.2.",xlab= "T", ylab= "Wert", cex=4)
lines(seq(0,50, le=45001), rowMeans(idp), col = "red")

dev.off()















