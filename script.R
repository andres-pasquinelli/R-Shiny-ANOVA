library(lattice)
levelplotz(z ~ Column*Row, data=ECR)
#------
dat <- data.frame("year"= c(2000:2005),"X1"=runif(6,-3,3),"X2"=runif(6,-3,3),"X3"=runif(6,-3,3),"X4"=runif(6,-3,3),"X5"=runif(6,-3,3),"X6"=runif(6,-3,3),"X7"=runif(6,-3,3),"X8"=runif(6,-3,3),"X9"=runif(6,-3,3),"X10"=runif(6,-3,3),"X11"=runif(6,-3,3),"X12"=runif(6,-3,3))
newd <- data.frame("year"=c(2000:2005), "val"=c(40,45,50,35,40,55), "sd"=c(5,6,8,4,5,9))
idx <- c(1:12)
dat2 <- expand.grid(y=dat[,1], x=idx)
dat2$z <- as.vector(as.matrix(dat[,-1]))
levelplot(z ~ y*x, data=dat2)

p1 = levelplot(z ~ y*x, data=dat2) 
p2 = lattice::xyplot(val ~ year, data=newd, type="l") 
latticeExtra::doubleYScale(p1, p2, add.axis=TRUE) 

p2 = lattice::xyplot(val ~ year, data=newd, type="l", lwd=3, ylim=c(30, 60)) + lattice::xyplot(I(val+sd) ~ year, data=newd, type="l") + lattice::xyplot(I(val-sd) ~ year, data=newd, type="l")
latticeExtra::doubleYScale(p1, p2, add.axis=TRUE) 
#-------