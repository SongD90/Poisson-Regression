### Funciton of input data
# In order to change data set more efficiently
add.data<- c("sentinel_filter_simplified.csv")

data.input<- function(){
  data.t<- read.csv(add.data)
  names(data.t)<- c("date","lead","result_pb","copper","result_cu","add", "city","zip","site_code","pipe_material","sample_loc","group num")
  data.t$zip<-as.character(data.t$zip)
  
  data.t$resp<- c((data.t$result_pb>15)+0)
  return (data.t)
}
# input data
data.pos<- data.input()

zipcode<- names(table(data.pos$zip))
material<- names(table(data.pos$pipe_material))
location<- names(table(data.pos$sample_loc))

n.zipcode<- length(zipcode)
n.material<- length(material)
n.location<- length(location)
n<-n.zipcode*n.material*n.location

pos.d.t<- matrix(0,n,5)
colnames(pos.d.t)<- c("y","result_cu","zip","material","location")
pos.d.t<- data.frame(pos.d.t)

pos.d.t$y<- rep(0,n)
pos.d.t$result_cu<- rep(0,n)
pos.d.t$zip<- rep("0",n)
pos.d.t$material<- rep("0",n)
pos.d.t$location<- rep("0",n)

## generate poisson dataset

t1<- data.pos


for(i in 1:n.zipcode){
  t2<- t1[which(t1$zip==zipcode[i]),]
  for(j in 1:n.material){
    t3<- t2[which(t2$pipe_material==material[j]),]
    for(k in 1:n.location){
      t4<- t3[which(t3$sample_loc==location[k]),]
      pos.d.t$result_cu[n.material*n.location*(i-1)+(j-1)*n.location+k]<- mean(t4$result_cu)
      pos.d.t$zip[n.material*n.location*(i-1)+(j-1)*n.location+k]<- zipcode[i]
      pos.d.t$material[n.material*n.location*(i-1)+(j-1)*n.location+k]<- material[j]
      pos.d.t$location[n.material*n.location*(i-1)+(j-1)*n.location+k]<- location[k]
      pos.d.t$y[n.material*n.location*(i-1)+(j-1)*n.location+k]<- sum((t4$result_pb>15)+0)
    }
  }
}

###############
reg.pos.t.1<- glm(data=pos.d.t, y~ result_cu+zip+material+location, family = "poisson")
summary(reg.pos.t.1)

anova(reg.pos.t.1)

pos.d.t$mat.fac<- as.factor(pos.d.t$material)
plot(pos.d.t$y~ pos.d.t$mat.fac, xlab="Material", ylab="Number of exceed point")
mean(pos.d.t$y)
var(pos.d.t$y)
par(mfrow=c(2,2))
plot(reg.pos.t.1)
## model with y=0 deleted
pos.d.t.ana.wo_0<- pos.d.t[-which(pos.d.t$y==0),]
reg.pos.t.2<- glm(data = pos.d.t.ana.wo_0,y~ result_cu+zip+material+location, family = "poisson")
summary(reg.pos.t.2)
reg.pos.t.2.r<- glm(data = pos.d.t.ana.wo_0,y~ result_cu+zip+material, family = "poisson")
anova(reg.pos.t.2, reg.pos.t.2.r, test = "Chisq")

mean(pos.d.t.ana.wo_0$y)
var(pos.d.t.ana.wo_0$y)
par(mfrow=c(2,2))
plot(reg.pos.t.2)
### Quasi-poisson regression, based on reg.pos.1
reg.pos.quasi<- glm(data=pos.d.t, y~result_cu+ zip+ material+ location, family = "quasipoisson")
summary(reg.pos.quasi)
par(mfrow=c(2,2))
plot(reg.pos.quasi)

dispersiontest(reg.pos.t.1, trafo = 1)
dispersiontest(reg.pos.t.2, trafo = 1)

ecoef <- exp(coef(reg.pos.quasi))

res <- residuals(reg.pos.t.1)
plot(res~fitted(reg.pos.quasi), xlab= "log(y)", ylab = "residual")

###############



