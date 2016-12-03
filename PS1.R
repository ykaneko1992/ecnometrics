library(plm)

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
#(b)
OLS1 <- lm (y1 ~ x11 + x21 +zeta)
summary(OLS1)

#(c)
data=data.frame(time=c(rep(1,1000),rep(2,1000),rep(3,1000)),
                group= c(c(1:1000),c(1:1000),c(1:1000)),
                 y=c(y1,y2,y3),x1=c(x11,x12,x13),x2=c(x21,x22,x23)
                 ,zet= c(zeta,zeta,zeta))

# pdata.frame (panel data frame) 作成。グループ列, 時間列で index 作成　 
idx <- c("group","time")
pdata <- pdata.frame(data,index = idx ,drop.index = T, row.names = T)
FE <- plm(y ~ x1 + x2 + zet , data =pdata, model="within")
summary(FE)

#(d)
x21d <- alpha + 0.1*u21
x22d <- alpha + 0.1*u22
x23d <- alpha + 0.1*u23
y1d <- 1 + x11 + x21d + zeta + alpha + nu1 
y2d <- 1 + x12 + x22d+ zeta + alpha + nu2 
y3d <- 1 + x13 + x23d + zeta + alpha + nu3
datad =data.frame(time=c(rep(1,1000),rep(2,1000),rep(3,1000)),
                group= c(c(1:1000),c(1:1000),c(1:1000)),
                y=c(y1d,y2d,y3d),x1=c(x11,x12,x13),x2=c(x21d,x22d,x23d)
                ,zet= c(zeta,zeta,zeta))
pdatad <- pdata.frame(datad,index = idx ,drop.index = T, row.names = T)
FEd <- plm(y ~ x1 + x2 + zet , data =pdatad, model="within")
summary(FEd)
