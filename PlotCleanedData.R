library(profvis)
library(png)
library(RColorBrewer)
library(colourschemes)
library(leaflet)
setwd("~/Desktop/UOLeedsMSC/UoL Diss/OneDriveStuffs/Tomography data/Examples_of_plotting_data_in_R")

#Slug_Flow-  196, 202, 208, 210, 214, 216

#Stratified_Flow-  198

#Plug_Flow- 200, 206, 212

#Annular_Flow- 204

#Bubble_Flow- 218, 220, 221, 222, 223, 224, 225, 226, 227, 228

fileno = "225"
flowtype = "Bubble2_AV30"
#tmp = as.matrix(read.csv("ET_images/202-ert.csv", skip=2, header=F))
tmp = as.matrix(read.csv(paste0("ET_images/", fileno, "_AV30.csv"), skip=1, header=F))
#tmp = as.matrix(read.csv(paste0("ET_images/Cleaned40_", fileno, "-ert.csv"), skip=1, header=F))
#tmp = as.matrix(read.csv(paste0("ET_images/", fileno, "_std.csv"), skip=1, header=F))
#tmp = as.matrix(read.csv("ET_images/202_std.csv", skip=2, header=F))
#tmp = as.matrix(read.csv(paste0("ET_images/", fileno, "_autocorrelation.csv"), skip=1, header=F))
#tmp = as.matrix(read.csv(paste0("ET_images/", fileno, "_AC5.csv"), skip=1, header=F))
df = data.frame(tmp)
head(df)

tmp[2,]

offset = c(7,5,3,2,2,1,1,rep(0,6),1,1,2,2,3,5,7) #Approximation of a circle
start = cumsum(c(1,20-offset[-20]*2))
end   = cumsum(c(20-offset*2))

nt = nrow(tmp)

x = array(NA, dim=c(nt,20,20))

for(j in 1:nt){
  for (i in 1:20){
    x[j, i, (offset[i]+1):(20-offset[i])] = as.numeric(tmp[j,start[i]:end[i]])
  }
}


for(j in 1:nt){
  test = x[j,,];
  test[!is.na(test) ] <- test[!is.na(test) ]*0.9;
  test[is.na(test)] <- 1;
  image(test, col=grey.colors(250,start = 0, end = 1))
  #writePNG(test, paste0("ET_images/", flowtype, "/" ,fileno,"-ert_", j, ".png"))
}
rnorm


#NOISY DATA
noise1 <- rnorm(400, 0, 0.15)
noise2 <- rnorm(400, 0, 0.05)
noise3 <- rnorm(400, 0, 0.01)
noisified1 <- x[98,,] + noise1
noisified2 <- x[98,,] + noise2
noisified3 <- x[98,,] + noise3
noisified1 <- apply(noisified1, 2, rev)
noisified2 <- apply(noisified2, 2, rev)
noisified3 <- apply(noisified3, 2, rev)
noisified1[noisified1<0] <- 0
noisified2[noisified2<0] <- 0
noisified3[noisified3<0] <- 0
example = x[98,,]
example <- apply(example, 2, rev)

noisified1
noisified2
noisified3
image(x[98,,], col=gray.colors(250))
x[6,,]
x[7,,]

image(t(example), col=gray.colors(250))
image(t(noisified1), col=gray.colors(250))
image(t(noisified2), col=gray.colors(250))
image(t(noisified3), col=gray.colors(250))
image(t(example), col=gray.colors(250))


Classes = c(10,6,3,1,1)
classnames = c("Bubble","Slug","Plug","Annluar","Stratified")
barplot(Classes, names.arg= classnames)
abline(h=mean(Classes))
mean(Classes)

z = array(NA, dim=c(nt,316,1))

for(j in 1:nt){
  for (i in 1:316){
    z[j, i,1] = as.numeric(tmp[j,i+2])
  }
}

z[1,1,1]

image(z[,,1], axes=T, zlim=c(0,1), col=topo.colors(250), main=paste("Spectogram of",fileno)); box()
#for(j in 1:nt){

#  image(x[j,,], zlim=c(0,1), axes=T,col=grey.colors(250), main=paste("Time index",j)); box()
#  pause(0.2)


#}


