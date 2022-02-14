# windowsFonts('ssp'='Source Sans Pro')
par(mfrow=c(2,4), mar=c(1,1,1,1), oma=c(0.7,0,1.3,0),mgp=c(1,0.2,0),
    cex=1, lend="square", ljoin="mitre", family='ssp')
# rectangular (regular)
plot(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
       rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
     c(rep(seq(1,8),10)), col="blue3", pch=19, 
     xaxt="n", yaxt="n", ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
mtext("Rectangular (regular)",3,0.35, adj=0.05, cex=1.2)
# triangular / staggered start
plot(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
       rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
     c(rep(c(seq(0.75,8,1),seq(1.25,8.5,1)),5)), 
     col="blue3", pch=19, 
     xaxt="n",yaxt="n",ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
mtext("Triangular /",3,1.1, adj=0.05, cex=1.2)
mtext("staggered start",3,0.2, adj=0.05, cex=1.2)
# random start
y0 <- rep(NA,10)
for (i in 1:10){
  y0[i] <- sample(seq(0,1,0.001),1)+0.5
}
y0
plot(rep(1,8),seq(y0[1],8.50001,1), col="blue3", pch=19, xaxt="n",yaxt="n",ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
for (i in 2:10){
  points(rep(i,8),seq(y0[i],8.50001,1), col="blue3", pch=19)
}
mtext("Random start",3,0.35, adj=0.05, cex=1.2)
rm(y0)
# systematic unaligned
plot(jitter(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
              rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8))),
     jitter(c(rep(seq(1,8),10))), col="blue3", pch=19, xaxt="n",yaxt="n",ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
mtext("Systematic unaligned",3,0.35, adj=0.05, cex=1.2)
# random cluster
plot(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),
       rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4)),
     c(rep(c(seq(1,8,2),seq(2,8.5,2)),5)), col="blue3", pch=19, xaxt="n",yaxt="n",ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
points(jitter(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),
       rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4)),amount=0.4),
       jitter(c(rep(c(seq(1,8,2),seq(2,8.5,2)),5)),amount=0.4), 
       col="darkred", pch=4, lwd=2)
mtext("Random cluster",1,0.35, adj=0.05, cex=1.2)
# randomised
x0 <- rep(NA,80)
y0 <- rep(NA,80)
for (i in 1:80){
  x0[i] <- (10*sample(seq(0,1,0.001),1))+0.5
  y0[i] <- (8*sample(seq(0,1,0.001),1))+0.5
}
# cbind(x0,y0)
plot(x0,y0, col="blue3", pch=19, xaxt="n",yaxt="n",ann=F, 
     xlim=c(0.5,10.5), ylim=c(0.5,8.5), xaxs="i", yaxs="i")
mtext("Randomised",1,0.35, adj=0.05, cex=1.2, family = "ssp")
rm(list=c('x0','y0','i'))
#
# stratified grid
x0 <- rep(seq(0.5,9.5), 10)
y0 <- c(rep(0.5,10),rep(1.5,10),rep(2.5,10),rep(3.5,10),
        rep(4.5,10),rep(5.5,10),rep(6.5,10),rep(7.5,10))
plot(c(0,10),c(0,10), type = "n", xaxt="n", yaxt="n", ann=F, 
     xlim=c(0,10), ylim=c(0,8), xaxs="i", yaxs="i")
abline(-10,2, lty = 3)
lines(c(0,7), c(7,4), lty = 3)
for(i in 1:100){
  if(y0[i]+0.1 > -10+(2*x0[i]) & y0[i]+0.1 > 7-((3/7)*x0[i])){
    points(x0[i], y0[i]+0.3, col="blue3", pch=19)
  }
}
for(i in 1:100){
  if(y0[i] < -10+(2*x0[i])){
    points(x0[i], y0[i]-0.2, col="red3", pch=2)
  }
}
for(i in 1:100){
  if(y0[i] > -10+(2*x0[i]) & y0[i] < 7-((3/7)*x0[i])){
    points(x0[i], y0[i], col="grey40", pch=3)
  }
}
mtext("Stratified grid",1,0.35, adj=0.05, cex=1.2, family = "ssp")
#
# randomised within strata
x0 <- sample(seq(0.05, 9.95, 0.1))
y0 <- sample(seq(0.05, 7.95, 0.1))
plot(c(0,10),c(0,8), type = "n", xaxt="n", yaxt="n", ann=F, 
     xlim=c(0,10), ylim=c(0,8), xaxs="i", yaxs="i")
abline(-10,2, lty = 3)
lines(c(0,7), c(7,4), lty = 3)
for(i in 1:100){
  if(y0[i] > -10+(2*x0[i]) & y0[i] > 7-((3/7)*x0[i])){
    points(x0[i], y0[i], col="blue3", pch=19)
  }
}
for(i in 1:100){
  if(y0[i] < -10+(2*x0[i])){
    points(x0[i], y0[i], col="red3", pch=2)
  }
}
for(i in 1:100){
  if(y0[i] > -10+(2*x0[i]) & y0[i] < 7-((3/7)*x0[i])){
    points(x0[i], y0[i], col="grey40", pch=3)
  }
}
mtext("Randomised in strata",1,0.35, adj=0.05, cex=1.2, family = "ssp")
rm(list=c('x0','y0','i'))
