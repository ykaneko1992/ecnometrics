library(plm)

#1
nu1 <- rnorm(1000)
nu2 <- rnorm(1000)
nu3 <- rnorm(1000)
alpha <- rnorm(1000)
zeta <- rnorm(1000)
u11 <- rnorm(1000)
u12 <- rnorm(1000)
u13 <- rnorm(1000)
u21 <- rnorm(1000)
u22 <- rnorm(1000)
u23 <- rnorm(1000)
x11 <- alpha + u11
x12 <- alpha + u12
x13 <- alpha + u13
x21 <- alpha + u21
x22 <- alpha + u22
x23 <- alpha + u23
#Model
y1 <- 1 + x11 + x21 + zeta + alpha + nu1 
y2 <- 1 + x12 + x22+ zeta + alpha + nu2 
y3 <- 1 + x13 + x23 + zeta + alpha + nu3

#(a)
#FE estimate
data=data.frame(time=c(rep(1,1000),rep(2,1000)),
                group= c(c(1:1000),c(1:1000)),
                y=c(y1,y2),x1=c(x11,x12),x2=c(x21,x22)
                ,zet= c(zeta,zeta))

# pdata.frame (panel data frame) 作成。グループ列, 時間列で index 作成　 
idx <- c("group","time")
pdata <- pdata.frame(data,index = idx ,drop.index = T, row.names = T)
FE <- plm(y ~ x1 + x2 + zet , data =pdata, model="within")
summary(FE)

#OLS estimate

yols <- y2 - y1
x1ols <- x12 - x11
x2ols <- x22 - x21

OLSa <- lm (yols ~ x1ols + x2ols-1)
summary(OLSa)


#(d)

#題意不明だがx21dはこうじゃないとOLSEと整合的にならない
x21d <- alpha + u21 + nu1
x22d <- alpha + u22 + nu1


datad=data.frame(time=c(rep(1,1000),rep(2,1000)),
                group= c(c(1:1000),c(1:1000)),
                y=c(y1,y2),x1=c(x11,x12),x2=c(x21d,x22d)
                ,zet= c(zeta,zeta))
pdatad <- pdata.frame(datad,index = idx ,drop.index = T, row.names = T)
FEd <- plm(y ~ x1 + x2 + zet , data =pdatad, model="within")
summary(FEd)

#OLSE
yolsd <- y2 - y1
x1olsd <- x12 - x11
x2olsd <- x22d - x21d

OLSd <- lm (yolsd ~ x1olsd + x2olsd)
summary(OLSd)

#(f)
timef <- rep(1:20, each=1000)
groupf <- rep(c(1:1000),20)
alphaf <- rep(alpha, 20)
zetaf <- rep(zeta,20)
u1f <- rnorm(20000)
u2f <- rnorm(20000)
nuf <- rnorm(20000)
#(d)のようなx2を作成するのに必要
nufx2 <- nuf
nufx2[1001:20000] <- nuf[1:19000]
x1f <- alphaf + u1f
x2f <- alphaf + u2f + nufx2
yf <- 1 + x1f + x2f + zetaf + alphaf + nuf 

dataf=data.frame(time=timef,
                 group=groupf,
                 y=yf,x1=x1f,x2=x2f
                 ,zet= zetaf)
pdataf <- pdata.frame(dataf,index = idx ,drop.index = T, row.names = T)
FEf <- plm(y ~ x1 + x2 +zet , data =pdataf, model="within")
summary(FEf)

#2
#(a)
timea <- rep(1:20, each=1000)
groupa <- rep(c(1:1000),20)
alphaa <- rep(alpha, 20)
zetaa <- rep(zeta,20)
u1a <- rnorm(20000)
u2a <- rnorm(20000)
nua <- rnorm(20000)
x1a <- alphaa + u1a
x2a <- alphaa + u2a
y0 <- rnorm(20000)
ya <- 1 + 0.6*y0 +x1a + x2a + zetaa + alphaa + nua 
yates <- ya
for (i in c(1:19)) {
  yates[(i*1000+1):((i+1)*1000)] <-1 +  0.6*yates[((i-1)*1000+1): (i*1000)] + x1a[(i*1000+1):((i+1)*1000)]  + x2a[(i*1000+1):((i+1)*1000)]  + zetaa[(i*1000+1):((i+1)*1000)]  + alphaa[(i*1000+1):((i+1)*1000)] + nua[(i*1000+1):((i+1)*1000)]  
}
yt1mi <- y0
yt1mi[1001:20000] <- yates[1:19000]
dataa=data.frame(time=timea,
                 group=groupa,
                 y=yates,yt1 =yt1mi ,x1=x1a,x2=x2a
                 ,zet= zetaa)
pdataa <- pdata.frame(dataa,index = idx ,drop.index = T, row.names = T)

#(b)
#t=1~20
FEa <- plm(y ~ x1 + x2+ yt1+zet , data =pdataa, model="within")
summary(FEa)

datab=data.frame(time=timea[1:2000],
                 group=groupa[1:2000],
                 y=yates[1:2000],yt1 =yt1mi[1:2000],x1=x1a[1:2000],x2=x2a[1:2000]
                 ,zet= zetaa[1:2000])
pdatab <- pdata.frame(datab,index = idx ,drop.index = T, row.names = T)
FEb <- plm(y ~ x1 + x2 + yt1+zet , data =pdatab, model="within")
summary(FEb)

#(e)
x1IV <- numeric(18000)
x2IV <- numeric(18000)
yaIV <- numeric(18000)
for (i in c(1:18)) {
  x1IV[((i-1)*1000 +1):(i*1000)] <- x1a[((i+1)*1000 + 1):((i+2)*1000)] - x1a[((i)*1000 + 1):((i+1)*1000)]
  x2IV[((i-1)*1000 +1):(i*1000)] <- x2a[((i+1)*1000 + 1):((i+2)*1000)] - x2a[((i)*1000 + 1):((i+1)*1000)]
  yaIV[((i-1)*1000 +1):(i*1000)] <- yates[((i)*1000 + 1):((i+1)*1000)] - yates[((i-1)*1000 + 1):((i)*1000)]
  }
library(AER)

eIVreg <- ivreg(yates[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mi[2001:20000]|x1IV+x2IV+yaIV)
summary(eIVreg)


#(f)
x1IVf <- numeric(18000)
x2IVf <- numeric(18000)
for (i in c(1:18)) {
  x1IVf[((i-1)*1000 +1):(i*1000)] <- x1a[((i)*1000 + 1):((i+1)*1000)] - x1a[((i-1)*1000 + 1):((i)*1000)]
  x2IVf[((i-1)*1000 +1):(i*1000)] <- x2a[((i)*1000 + 1):((i+1)*1000)] - x2a[((i-1)*1000 + 1):((i)*1000)]
}
eIVregf <- ivreg(yates[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mi[2001:20000]|x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregf)

#(g)
yg <- 1 + 0.9*y0 +x1a + x2a + zetaa + alphaa + nua 
ygtes <- yg
for (i in c(1:19)) {
  ygtes[(i*1000+1):((i+1)*1000)] <-1 +  0.9*ygtes[((i-1)*1000+1): (i*1000)] + x1a[(i*1000+1):((i+1)*1000)]  + x2a[(i*1000+1):((i+1)*1000)]  + zetaa[(i*1000+1):((i+1)*1000)]  + alphaa[(i*1000+1):((i+1)*1000)] + nua[(i*1000+1):((i+1)*1000)]  
}
yt1mig <- y0
yt1mig[1001:20000] <- ygtes[1:19000]

eIVregg <- ivreg(ygtes[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mig[2001:20000]|x1IV+x2IV+ygtes[1:18000])
summary(eIVregg)

eIVregfg <- ivreg(ygtes[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mig[2001:20000]|x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregfg)

#(h)

ep = rnorm(20000)
nuh <- numeric(20000)
nuh [1:1000] <- 0.5*rnorm(1000) + ep[1:1000]
for (i in c(1:19)) {
  nuh[(i*1000+1):((i+1)*1000)] <- 0.5*nuh[((i-1)*1000+1): (i*1000)] + ep[((i)*1000+1): ((i+1)*1000)]
}

yh <- 1 + 0.6*y0 +x1a + x2a + zetaa + alphaa + nuh
yhtes <- yh
for (i in c(1:19)) {
  yhtes[(i*1000+1):((i+1)*1000)] <-1 +  0.6*yhtes[((i-1)*1000+1): (i*1000)] + x1a[(i*1000+1):((i+1)*1000)]  + x2a[(i*1000+1):((i+1)*1000)]  + zetaa[(i*1000+1):((i+1)*1000)]  + alphaa[(i*1000+1):((i+1)*1000)] + nuh[(i*1000+1):((i+1)*1000)]  
}
yt1mih <- y0
yt1mih[1001:20000] <- yhtes[1:19000]

eIVregh <- ivreg(yhtes[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mih[2001:20000]|x1IV+x2IV+yhtes[1:18000])
summary(eIVregh)

eIVregfh <- ivreg(yhtes[2001:20000] ~ x1a[2001:20000] + x2a[2001:20000] +yt1mih[2001:20000]|x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregfh)


#####改訂版


#(e)
x1IV <- numeric(18000)
x2IV <- numeric(18000)
yaIV <- numeric(18000)
yareg <- numeric(18000)
for (i in c(1:18)) {
  x1IV[((i-1)*1000 +1):(i*1000)] <- x1a[((i+1)*1000 + 1):((i+2)*1000)] - x1a[((i)*1000 + 1):((i+1)*1000)]
  x2IV[((i-1)*1000 +1):(i*1000)] <- x2a[((i+1)*1000 + 1):((i+2)*1000)] - x2a[((i)*1000 + 1):((i+1)*1000)]
  yaIV[((i-1)*1000 +1):(i*1000)] <- yates[((i)*1000 + 1):((i+1)*1000)] - yates[((i-1)*1000 + 1):((i)*1000)]
  yareg[((i-1)*1000 +1):(i*1000)] <- yates[((i+1)*1000 + 1):((i+2)*1000)] - yates[((i)*1000 + 1):((i+1)*1000)]
}
library(AER)

eIVrege <- ivreg(yareg ~ x1IV + x2IV +yaIV-1 |x1IV+x2IV+yates[1:18000])
summary(eIVrege)

#f

x1IVf <- numeric(18000)
x2IVf <- numeric(18000)
for (i in c(1:18)) {
  x1IVf[((i-1)*1000 +1):(i*1000)] <- x1a[((i)*1000 + 1):((i+1)*1000)] - x1a[((i-1)*1000 + 1):((i)*1000)]
  x2IVf[((i-1)*1000 +1):(i*1000)] <- x2a[((i)*1000 + 1):((i+1)*1000)] - x2a[((i-1)*1000 + 1):((i)*1000)]
}
eIVregf <- ivreg(yareg ~ x1IV + x2IV +yaIV-1 |x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregf)

#(g)
yg <- 1 + 0.9*y0 +x1a + x2a + zetaa + alphaa + nua 
ygtes <- yg
for (i in c(1:19)) {
  ygtes[(i*1000+1):((i+1)*1000)] <-1 +  0.9*ygtes[((i-1)*1000+1): (i*1000)] + x1a[(i*1000+1):((i+1)*1000)]  + x2a[(i*1000+1):((i+1)*1000)]  + zetaa[(i*1000+1):((i+1)*1000)]  + alphaa[(i*1000+1):((i+1)*1000)] + nua[(i*1000+1):((i+1)*1000)]  
}
yt1mig <- y0
yt1mig[1001:20000] <- ygtes[1:19000]

ygIV <- numeric(18000)
ygreg <- numeric(18000)
for (i in c(1:18)) {
  ygIV[((i-1)*1000 +1):(i*1000)] <- ygtes[((i)*1000 + 1):((i+1)*1000)] - ygtes[((i-1)*1000 + 1):((i)*1000)]
  ygreg[((i-1)*1000 +1):(i*1000)] <- ygtes[((i+1)*1000 + 1):((i+2)*1000)] - ygtes[((i)*1000 + 1):((i+1)*1000)]
}

eIVregg <- ivreg(ygreg ~ x1IV + x2IV +ygIV-1 |x1IV+x2IV+ygtes[1:18000])
summary(eIVregg)

eIVregfg <- ivreg(ygreg ~ x1IV + x2IV +ygIV-1 |x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregfg)

#(h)

ep <- rnorm(20000)
nuh <- numeric(20000)
nuh [1:1000] <- 0.5*rnorm(1000) + ep[1:1000]
for (i in c(1:19)) {
  nuh[(i*1000+1):((i+1)*1000)] <- 0.5*nuh[((i-1)*1000+1): (i*1000)] + ep[((i)*1000+1): ((i+1)*1000)]
}

yh <- 1 + 0.6*y0 +x1a + x2a + zetaa + alphaa + nuh
yhtes <- yh
for (i in c(1:19)) {
  yhtes[(i*1000+1):((i+1)*1000)] <-1 +  0.6*yhtes[((i-1)*1000+1): (i*1000)] + x1a[(i*1000+1):((i+1)*1000)]  + x2a[(i*1000+1):((i+1)*1000)]  + zetaa[(i*1000+1):((i+1)*1000)]  + alphaa[(i*1000+1):((i+1)*1000)] + nuh[(i*1000+1):((i+1)*1000)]  
}
yt1mih <- y0
yt1mih[1001:20000] <- yhtes[1:19000]

yhIV <- numeric(18000)
yhreg <- numeric(18000)
for (i in c(1:18)) {
  yhIV[((i-1)*1000 +1):(i*1000)] <- yhtes[((i)*1000 + 1):((i+1)*1000)] - yhtes[((i-1)*1000 + 1):((i)*1000)]
  yhreg[((i-1)*1000 +1):(i*1000)] <- yhtes[((i+1)*1000 + 1):((i+2)*1000)] - yhtes[((i)*1000 + 1):((i+1)*1000)]
}

eIVregh <- ivreg(yhreg ~ x1IV + x2IV +yhIV-1 |x1IV+x2IV+yhtes[1:18000])
summary(eIVregh)

eIVregfh <- ivreg(yhreg ~ x1IV + x2IV +yhIV-1 |x1IV+x2IV+x1IVf+x2IVf)
summary(eIVregfh)
