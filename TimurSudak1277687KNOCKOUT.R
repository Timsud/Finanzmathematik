



# payment function of the knock-out certificate
fun<-function(x,strk) pmax(x-strk,0)*0.01


chng_kn_ot <- function(act_val,chng_prc,strk){
  z <- 100*((fun(act_val*(1 + chng_prc/100),strk)) - fun(act_val,strk))/fun(act_val,strk)
  z
}
# Die  Zahlen sind von dem Beispiel aus dem Buch genommen.
chng_kn_ot(5000,0.15,4500) #derivative increases by 150% if underlying increases by 15% at value 5000
chng_kn_ot(5000,-0.07,4500) #derivative decreases by 70% if underlying decreases by 7% at value 5000
### Beispiel###
# x... value of index (underlying) at maturity
# x follows a normal distribution N(5000,300)
set.seed(1)
x<-rnorm(1000000,mean=5000,sd=300)
head(x)
fn_mit_x <- fun(x,4500) 
cert_mean<-mean(fun(x,4500)) #expected value of certificate at maturity in 1 EUR/pt
cert_mean
# Variance
cert_var <- var(x)
cert_var
# underlying... price changes of underlying (assumption)
undrl<-c(-50,-35,-18,-8,0,16,27) #7 price changes



knkt_crt<- numeric(length = length(undrl))
for(i in 1:length(undrl)) knkt_crt[i]<-chng_kn_ot(mean(x),undrl[i],strk = 4500)
knkt_crt

# plot knockout certificate changes against underlying changes
plot(knkt_crt,undrl,col="blue",type="l",xlim=c(-0.38,0.18),ylim=c(-0.38,0.18))
lines(undrl, knkt_crt,col="green")


# plot of Kernel Density Estimation.
plot(density(fn_mit_x), main = "Kern.Denst.Est. of Payment")
abline(v = cert_mean, col = "yellow", lty = "dashed")
legend(x = "topright", col = "yellow", lty = "dashed",
       lwd = 2, legend = paste0("Mean: ", round(cert_mean,3)))
