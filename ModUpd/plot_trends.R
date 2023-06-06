# Plots characteristic trends for BUS
  
plot_trends <- function(bi,ccdf,Yi,L){
labs <- 0:(L-1)

# Trend 1 in paper
plot(bi,log(ccdf),pch = 20,xlab = 'b',ylab = 'log( P[Y>b] )',
#main = 'Characteristic Trend 1', xlim=c(Yi[1],Yi[L])) 
main = 'Characteristic Trend 1', xlim=c(bi[850],max(bi))) 
abline(v=Yi,lty=2)
text(Yi, -10, labels = labs, pos = 2)
text(max(bi), -10, labels = L, pos = 2,col='red')

# Trend 2 in paper
plot(bi,bi+log(ccdf),pch = 20,xlab = 'b',ylab = 'b + log( P[Y>b] )',
#main = 'Characteristic Trend 2', xlim=c(Yi[1],Yi[L])) 
main = 'Characteristic Trend 2', xlim=c(bi[850],max(bi))) 
abline(v=Yi,lty=2)
text(Yi, -400, labels = labs, pos = 2)
text(max(bi), -400, labels = L, pos = 2,col='red')
}