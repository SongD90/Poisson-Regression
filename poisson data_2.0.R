
##############################################
## Function of input data
add.data<- c("sentinel_filter_simplified.csv")

data.input<- function(){
data.t<- read.csv(add.data)
names(data.t)<- c("date","lead","result_pb","copper","result_cu","add", "city","zip","site_code","pipe_material","sample_loc","group num")
data.t$zip<-as.character(data.t$zip)

data.t$resp<- c((data.t$result_pb>15)+0)
return (data.t)
}

############################################
###Input data and calculate the dimension.
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

#################################
###Function of create poisson data
pos.d.f<- function(l,u){
  pos.d.ft<- pos.d.t
  int.1<- c(l:u)
  t1<- data.pos[which(data.pos$result_pb%in%int.1),]
  
  
  for(i in 1:n.zipcode){
    t2<- t1[which(t1$zip==zipcode[i]),]
    for(j in 1:n.material){
      t3<- t2[which(t2$pipe_material==material[j]),]
      for(k in 1:n.location){
        t4<- t3[which(t3$sample_loc==location[k]),]
        pos.d.ft$result_cu[n.material*n.location*(i-1)+(j-1)*n.location+k]<- mean(t4$result_cu)
        pos.d.ft$zip[n.material*n.location*(i-1)+(j-1)*n.location+k]<- zipcode[i]
        pos.d.ft$material[n.material*n.location*(i-1)+(j-1)*n.location+k]<- material[j]
        pos.d.ft$location[n.material*n.location*(i-1)+(j-1)*n.location+k]<- location[k]
        pos.d.ft$y[n.material*n.location*(i-1)+(j-1)*n.location+k]<- nrow(t4)
      }
    }
  }
  return(pos.d.ft)
}

##################################################################################

##0-15
pos.d.0_15<- pos.d.f(0,15)
##16-20
pos.d.16_20<- pos.d.f(16,20)
##21-30
pos.d.21_30<- pos.d.f(21,30)
##31-40
pos.d.31_40<- pos.d.f(31,40)
##41-80
pos.d.41_80<- pos.d.f(41,80)
##81-100
pos.d.81_100<- pos.d.f(81,100)
##101-150
pos.d.101_150<- pos.d.f(101,150)
##151-200
pos.d.151_200<- pos.d.f(151,200)
##201-500
pos.d.201_500<- pos.d.f(201,500)
##>500
pos.d.500<- pos.d.f(500,999999)

## Combine data
pos.d.ana<- rbind(pos.d.0_15, pos.d.16_20, pos.d.21_30, pos.d.31_40, pos.d.41_80, 
                  pos.d.81_100, pos.d.101_150, pos.d.151_200, pos.d.201_500, 
                  pos.d.500)
pos.d.ana.wo_0<- pos.d.ana[-which(pos.d.ana$y==0),]


## Poisson Regression 1
reg.pos.1<- glm(data = pos.d.ana.wo_0, y~ result_cu+zip+ material+ location, family = "poisson")
summary(reg.pos.1)

Pear.res<-residuals(reg.pos.1, type = "pearson")
plot(Pear.res.t~fitted(reg.pos.1)[which(Pear.res< 100)])
Pear.res.t<- Pear.res[which(Pear.res< 100)]

plot(residuals(reg.pos.1)~ fitted(reg.pos.1))

## Poisson Regression 2
reg.pos.2<- glm(data = pos.d.ana, y~ result_cu+zip+ material+ location, family = "poisson")
summary(reg.pos.2)

## Over or under dispersion Poisson regression model
reg.pos.quasi<- glm(data=pos.d.ana, y~zip+ material+ location, family = "quasipoisson")
summary(reg.pos.quasi)


plot(reg.pos.2)
pos.d.ana$mat.fac<- as.factor(pos.d.ana$material)
pos.d.ana$loc.fac<- as.factor(pos.d.ana$location)
pos.d.ana$zip.fac<- as.factor(pos.d.ana$zip)

plot(pos.d.ana$y~pos.d.ana$mat.fac)
plot(pos.d.ana$y~pos.d.ana$loc.fac)
plot(pos.d.ana$y~pos.d.ana$zip.fac)

