setwd("~/Desktop/UOLeedsMSC/UoL Diss/OneDriveStuffs/Tomography data/Examples_of_plotting_data_in_R")
library(heplots)
library(abind)
library(Rfast)
library(GGally)
library(fields)

#Slug_Flow-  196, 202, 208, 210, 214, 216
tmp196 = as.matrix(read.csv(paste0("ET_images/", "196", "-ert.csv"), skip=2, header=F))
tmp202 = as.matrix(read.csv(paste0("ET_images/", "202", "-ert.csv"), skip=2, header=F))
tmp208 = as.matrix(read.csv(paste0("ET_images/", "208", "-ert.csv"), skip=2, header=F))
tmp210 = as.matrix(read.csv(paste0("ET_images/", "210", "-ert.csv"), skip=2, header=F))
tmp214 = as.matrix(read.csv(paste0("ET_images/", "214", "-ert.csv"), skip=2, header=F))
tmp216 = as.matrix(read.csv(paste0("ET_images/", "216", "-ert.csv"), skip=2, header=F))

#Stratified_Flow-  198
tmp198 = as.matrix(read.csv(paste0("ET_images/", "198", "-ert.csv"), skip=2, header=F))

#Plug_Flow- 200, 206, 212
tmp200 = as.matrix(read.csv(paste0("ET_images/", "200", "-ert.csv"), skip=2, header=F))

tmp206 = as.matrix(read.csv(paste0("ET_images/", "206", "-ert.csv"), skip=2, header=F))
tmp212 = as.matrix(read.csv(paste0("ET_images/", "212", "-ert.csv"), skip=2, header=F))

#Annular_Flow- 204
tmp2041 = as.matrix(read.csv(paste0("ET_images/Cleaned40_", "204", "-ert.csv"), skip=2, header=F))
tmp2042 = as.matrix(read.csv(paste0("ET_images/Cleaned20_", "204", "-ert.csv"), skip=2, header=F))
tmp2043 = as.matrix(read.csv(paste0("ET_images/Cleaned10_", "204", "-ert.csv"), skip=2, header=F))

tmp204 = as.matrix(read.csv(paste0("ET_images/", "204", "-ert.csv"), skip=2, header=F))

#Bubble_Flow- 218, 220, 221, 222, 223, 224, 225, 226, 227, 228
tmp218 = as.matrix(read.csv(paste0("ET_images/", "218", "-ert.csv"), skip=2, header=F))
tmp220 = as.matrix(read.csv(paste0("ET_images/", "220", "-ert.csv"), skip=2, header=F))
tmp221 = as.matrix(read.csv(paste0("ET_images/", "221", "-ert.csv"), skip=2, header=F))
tmp222 = as.matrix(read.csv(paste0("ET_images/", "222", "-ert.csv"), skip=2, header=F))
tmp223 = as.matrix(read.csv(paste0("ET_images/", "223", "-ert.csv"), skip=2, header=F))
tmp224 = as.matrix(read.csv(paste0("ET_images/", "224", "-ert.csv"), skip=2, header=F))
tmp225 = as.matrix(read.csv(paste0("ET_images/", "225", "-ert.csv"), skip=2, header=F))
tmp226 = as.matrix(read.csv(paste0("ET_images/", "226", "-ert.csv"), skip=2, header=F))
tmp227 = as.matrix(read.csv(paste0("ET_images/", "227", "-ert.csv"), skip=2, header=F))
tmp228 = as.matrix(read.csv(paste0("ET_images/", "228", "-ert.csv"), skip=2, header=F))


colMeans(tmp196)

#############Slug MEAN
Slug_tot = do.call("rbind", list(tmp196, tmp202, tmp208, tmp210, tmp214, tmp216))
Slug_tot = Slug_tot[complete.cases(Slug_tot), ]

nr1 = nrow(Slug_tot)
nc1 = (ncol(Slug_tot)-2)

x1 = array(NA, dim=c(nr1,nc1))

for(j in 1:nr1){
  for (i in 1:nc1){
    x1[j, i] = as.numeric(Slug_tot[j,i+2])
  }
}
slug_rw_means = rowmeans(x1)
SlugVar = colVars(x1)
Slugsd = colVars(x1, sd)
Slug_means = colMeans(x1)
Slug_means1 = Slug_means[1:316]
Slug_means2 = Slug_means[317:632]
SlugMean = colMeans(rbind(Slug_means1,Slug_means2))
x1_1 = x1[,1:316]
x1_2 = x1[,317:632]
x1_tot = rbind(x1_1,x1_2)
Slug_sd = apply(x1_tot,2,sd)

#############Stratified MEAN
Stratified_tot = tmp198[complete.cases(tmp198), ]

nr2 = nrow(Stratified_tot)
nc2 = (ncol(Stratified_tot)-2)

x2 = array(NA, dim=c(nr2,nc2))

for(j in 1:nr2){
  for (i in 1:nc2){
    x2[j, i] = as.numeric(Slug_tot[j,i+2])
  }
}
nrow(x2)
ncol(x2)

strat_rw_means = rowmeans(x2)
StratVar = colVars(x2)
Stratsd = colVars(x2, sd)
Stratified_means = colMeans(x2)
Stratified_means1 = Stratified_means[1:316]
Stratified_means2 = Stratified_means[317:632]
StratifiedMean = colMeans(rbind(Stratified_means1,Stratified_means2))
x2_1 = x2[,1:316]
x2_2 = x2[,317:632]
x2_tot = rbind(x2_1,x2_2)
Stratified_sd = apply(x2_tot,2,sd)

#############ANNULAR MEAN
Annular_tot = tmp204[complete.cases(tmp204), ]

nr3 = nrow(Annular_tot)
nc3 = (ncol(Annular_tot)-2)

x3 = array(NA, dim=c(nr3,nc3))

for(j in 1:nr3){
  for (i in 1:nc3){
    x3[j, i] = as.numeric(Annular_tot[j,i+2])
  }
}
nrow(x3)
ncol(x3)
annular_rw_means = rowmeans(x3)
AnnularVar = colVars(x3)
Annularsd = colVars(x3, sd)
Annular_means = colMeans(x3)
Annular_means1 = Annular_means[1:316]
Annular_means2 = Annular_means[317:632]
AnnularMean = colMeans(rbind(Annular_means1,Annular_means2))
x3_1 = x3[,1:316]
x3_2 = x3[,317:632]
x3_tot = rbind(x3_1,x3_2)
Annular_sd = apply(x3_tot,2,sd)

############Plug_Flow- 200, 206, 212
Plug_tot = do.call("rbind", list(tmp200, tmp206, tmp212))
Plug_tot = Plug_tot[complete.cases(Plug_tot), ]

nr4 = nrow(Plug_tot)
nc4 = (ncol(Plug_tot)-2)

x4 = array(NA, dim=c(nr4,nc4))

for(j in 1:nr4){
  for (i in 1:nc4){
    x4[j, i] = as.numeric(Plug_tot[j,i+2])
  }
}

plug_rw_means = rowmeans(x4)
PlugVar = colVars(x4)
Plugsd = colVars(x4, sd)
Plug_means = colMeans(x4)
Plug_means1 = Plug_means[1:316]
Plug_means2 = Plug_means[317:632]
PlugMean = colMeans(rbind(Plug_means1,Plug_means2))
x4_1 = x4[,1:316]
x4_2 = x4[,317:632]
x4_tot = rbind(x4_1,x4_2)
Plug_sd = apply(x4_tot,2,sd)


#############Bubble MEAN 218, 220, 221, 222, 223, 224, 225, 226, 227, 228
Bubble_tot = do.call("rbind", list(tmp218, tmp220, tmp221, tmp222, tmp223, tmp224, tmp225, tmp226, tmp227, tmp228))
Bubble_tot = Bubble_tot[complete.cases(Bubble_tot), ]

nr5 = nrow(Bubble_tot)
nc5 = (ncol(Bubble_tot)-2)

x5 = array(NA, dim=c(nr5,nc5))

for(j in 1:nr5){
  for (i in 1:nc5){
    x5[j, i] = as.numeric(Bubble_tot[j,i+2])
  }
}
bubble_rw_means = rowmeans(x5)
BubbleVar = colVars(x5)
Bubblesd = colVars(x5, sd)
Bubble_means = colMeans(x5)
Bubble_means1 = Bubble_means[1:316]
Bubble_means2 = Bubble_means[317:632]
BubbleMean = colMeans(rbind(Bubble_means1,Bubble_means2))
x5_1 = x5[,1:316]
x5_2 = x5[,317:632]
x5_tot = rbind(x5_1,x5_2)
Bubble_sd = apply(x5_tot,2,sd)



#############Slug MEAN WANG
Slug_tot_wang = do.call("rbind", list(tmp202, tmp210, tmp216, tmp220, tmp221, tmp222))
Slug_tot_wang = Slug_tot_wang[complete.cases(Slug_tot_wang), ]

nr1w = nrow(Slug_tot_wang)
nc1w = (ncol(Slug_tot_wang)-2)

x1w = array(NA, dim=c(nr1w,nc1w))

for(j in 1:nr1w){
  for (i in 1:nc1w){
    x1w[j, i] = as.numeric(Slug_tot_wang[j,i+2])
  }
}
slug_w_rw_means = rowmeans(x1w)
SlugVar_wang = colVars(x1w)
Slugsd_wang = colVars(x1w, sd)
Slug_means_wang = colMeans(x1w)
Slug_means1_wang = Slug_means_wang[1:316]
Slug_means2_wang = Slug_means_wang[317:632]
SlugMean_wang = colMeans(rbind(Slug_means1_wang,Slug_means2_wang))
x1w_1 = x1w[,1:316]
x1w_2 = x1w[,317:632]
x1w_tot = rbind(x1w_1,x1w_2)
Slug_w_sd = apply(x1w_tot,2,sd)




#############Bubble MEAN WANG
Bubble_tot_wang = do.call("rbind", list(tmp223, tmp224, tmp225))
Bubble_tot_wang = Bubble_tot_wang[complete.cases(Bubble_tot_wang), ]

nr5w = nrow(Bubble_tot_wang)
nc5w = (ncol(Bubble_tot_wang)-2)

x5w = array(NA, dim=c(nr5w,nc5w))

for(j in 1:nr5w){
  for (i in 1:nc5w){
    x5w[j, i] = as.numeric(Bubble_tot_wang[j,i+2])
  }
}
bubble_w_rw_means = rowmeans(x5w)
BubbleVar_wang = colVars(x5w)
Bubblesd_wang = colVars(x5w, sd)
Bubble_means_wang = colMeans(x5w)
Bubble_means1_wang = Bubble_means_wang[1:316]
Bubble_means2_wang = Bubble_means_wang[317:632]
BubbleMean_wang = colMeans(rbind(Bubble_means1_wang,Bubble_means2_wang))
x5w_1 = x5w[,1:316]
x5w_2 = x5w[,317:632]
x5w_tot = rbind(x5w_1,x5w_2)
Bubble_w_sd = apply(x5w_tot,2,sd)



#############Transient MEAN
Transient_tot = tmp228[complete.cases(tmp228), ]

nr3w = nrow(Transient_tot)
nc3w = (ncol(Transient_tot)-2)

x3w = array(NA, dim=c(nr3w,nc3w))

for(j in 1:nr3w){
  for (i in 1:nc3w){
    x3w[j, i] = as.numeric(Transient_tot[j,i+2])
  }
}
nrow(x3w)
ncol(x3w)

transient_w_rw_means = rowmeans(x3w)
TransientVar = colVars(x3w)
Transientsd = colVars(x3w, sd)
Transient_means = colMeans(x3w)
Transient_means1 = Transient_means[1:316]
Transient_means2 = Transient_means[317:632]
TransientMean = colMeans(rbind(Transient_means1,Transient_means2))
x3w_1 = x3w[,1:316]
x3w_2 = x3w[,317:632]
x3w_tot = rbind(x3w_1,x3w_2)
Transient_sd = apply(x3w_tot,2,sd)

Number_of_features = length(BubbleMean)

SlugMean
PlugMean
AnnularMean
StratifiedMean
BubbleMean

SlugVar
PlugVar
AnnularVar
StratifiedVar
BubbleVar

Subbed_means = SlugMean - PlugMean - AnnularMean - StratifiedMean - BubbleMean

n = Number_of_features - 1

Sample_pool=(n*SlugVar + n*PlugVar + n*AnnularVar + n*StratifiedVar + n*BubbleVar)/((5*Number_of_features)-5)

Maximise_Vec = solve(Sample_pool)

qr(Sample_pool)$rank
qr(Sample_pool)

MegaTable = do.call("rbind", list(x1,x2,x3,x4,x5))
nrow(MegaTable)
MegaTable1 = MegaTable[,1:316]
MegaTable2 = MegaTable[,317:632]
TheTable = rbind(MegaTable1,MegaTable2)
nrow(TheTable)
ncol(TheTable)

cov(TheTable)
det(cov(TheTable))

col_means = colMeans(TheTable)

offset = c(7,5,3,2,2,1,1,rep(0,6),1,1,2,2,3,5,7) #Approximation of a circle
start = cumsum(c(1,20-offset[-20]*2))
end   = cumsum(c(20-offset*2))


xz = array(NA, dim=c(20,20))
for (i in 1:20){
  xz[i, (offset[i]+1):(20-offset[i])] = as.numeric(AnnularMean[start[i]:end[i]])
}
image(t(xz), main = 'Mean annular flow'); box()

xb = array(NA, dim=c(20,20))
for (i in 1:20){
  xb[i, (offset[i]+1):(20-offset[i])] = as.numeric(SlugMean[start[i]:end[i]])
}
image(t(xb), main = 'Mean slug flow (primary)'); box()


xb2 = array(NA, dim=c(20,20))
for (i in 1:20){
  xb2[i, (offset[i]+1):(20-offset[i])] = as.numeric(SlugMean_wang[start[i]:end[i]])
}
image(t(xb2), main = 'Mean slug flow (secondary)'); box()

xa = array(NA, dim=c(20,20))
for (i in 1:20){
  xa[i, (offset[i]+1):(20-offset[i])] = as.numeric(BubbleMean[start[i]:end[i]])
}
image(t(xa), main = 'Mean bubble flow (primary)'); box()

xa2 = array(NA, dim=c(20,20))
for (i in 1:20){
  xa2[i, (offset[i]+1):(20-offset[i])] = as.numeric(BubbleMean_wang[start[i]:end[i]])
}
image(t(xa2), main = 'Mean bubble flow (secondary)'); box()

xc = array(NA, dim=c(20,20))
for (i in 1:20){
  xc[i, (offset[i]+1):(20-offset[i])] = as.numeric(PlugMean[start[i]:end[i]])
}
image(t(xc), main = 'Mean plug flow'); box()

xd = array(NA, dim=c(20,20))
for (i in 1:20){
  xd[i, (offset[i]+1):(20-offset[i])] = as.numeric(StratifiedMean[start[i]:end[i]])
}
image(t(xd), main = 'Mean stratified flow'); box()

xf = array(NA, dim=c(20,20))
for (i in 1:20){
  xf[i, (offset[i]+1):(20-offset[i])] = as.numeric(TransientMean[start[i]:end[i]])
}
image(t(xf), main = 'Mean transient flow'); box()

xz = array(NA, dim=c(20,20))
for (i in 1:20){
  xz[i, (offset[i]+1):(20-offset[i])] = as.numeric(Annularsd[start[i]:end[i]])
}
image(t(xz), main = '`Annular flow standard deviation'); box()

xb = array(NA, dim=c(20,20))
for (i in 1:20){
  xb[i, (offset[i]+1):(20-offset[i])] = as.numeric(Slugsd[start[i]:end[i]])
}
image(t(xb), main = 'Slug flow standard deviation (primary)'); box()


xb2 = array(NA, dim=c(20,20))
for (i in 1:20){
  xb2[i, (offset[i]+1):(20-offset[i])] = as.numeric(Slugsd_wang[start[i]:end[i]])
}
image(t(xb2), main = 'Slug flow standard deviation (secondary)'); box()

xa = array(NA, dim=c(20,20))
for (i in 1:20){
  xa[i, (offset[i]+1):(20-offset[i])] = as.numeric(Bubblesd[start[i]:end[i]])
}
image(t(xa), main = 'Bubble flow standard deviation (primary)'); box()

xa2 = array(NA, dim=c(20,20))
for (i in 1:20){
  xa2[i, (offset[i]+1):(20-offset[i])] = as.numeric(Bubblesd_wang[start[i]:end[i]])
}
image(t(xa2), main = 'Bubble flow standard deviation (secondary)'); box()

xc = array(NA, dim=c(20,20))
for (i in 1:20){
  xc[i, (offset[i]+1):(20-offset[i])] = as.numeric(Plugsd[start[i]:end[i]])
}
image(t(xc), main = 'Plugflow standard deviation'); box()

xd = array(NA, dim=c(20,20))
for (i in 1:20){
  xd[i, (offset[i]+1):(20-offset[i])] = as.numeric(Stratsd[start[i]:end[i]])
}
image(t(xd), main = 'Stratified flow standard deviation'); box()

xf = array(NA, dim=c(20,20))
for (i in 1:20){
  xf[i, (offset[i]+1):(20-offset[i])] = as.numeric(Transientsd[start[i]:end[i]])
}
image(t(xf), main = 'Transient flow standard deviation'); box()


m_220 = tmp220[complete.cases(tmp220), ]
nr220 = nrow(m_220)
nc220 = (ncol(m_220)-2)
x220 = array(NA, dim=c(nr220,nc220))
for(j in 1:nr220){
  for (i in 1:nc220){
    x220[j, i] = as.numeric(m_220[j,i+2])
  }
}
m_220_means = colMeans(x220)
m_220_mean1 = m_220_means[1:316]
m_220_mean2 = m_220_means[317:632]
m_220_mean = colMeans(rbind(m_220_mean1,m_220_mean2))
m_220_1 = x220[,1:316]
m_220_2 = x220[,317:632]
m_220_tot = rbind(m_220_1,m_220_2)
m_220_sd = apply(m_220_tot,2,sd)

m_221 = tmp221[complete.cases(tmp221), ]
nr221 = nrow(m_221)
nc221 = (ncol(m_221)-2)
x221 = array(NA, dim=c(nr221,nc221))
for(j in 1:nr221){
  for (i in 1:nc221){
    x221[j, i] = as.numeric(m_221[j,i+2])
  }
}
m_221_means = colMeans(x221)
m_221_mean1 = m_221_means[1:316]
m_221_mean2 = m_221_means[317:632]
m_221_mean = colMeans(rbind(m_221_mean1,m_221_mean2))
m_221_1 = x221[,1:316]
m_221_2 = x221[,317:632]
m_221_tot = rbind(m_221_1,m_221_2)
m_221_sd = apply(m_221_tot,2,sd)

m_222 = tmp222[complete.cases(tmp222), ]
nr222 = nrow(m_222)
nc222 = (ncol(m_222)-2)
x222 = array(NA, dim=c(nr222,nc222))
for(j in 1:nr222){
  for (i in 1:nc222){
    x222[j, i] = as.numeric(m_222[j,i+2])
  }
}
m_222_means = colMeans(x222)
m_222_mean1 = m_222_means[1:316]
m_222_mean2 = m_222_means[317:632]
m_222_mean = colMeans(rbind(m_222_mean1,m_222_mean2))
m_222_1 = x222[,1:316]
m_222_2 = x222[,317:632]
m_222_tot = rbind(m_222_1,m_222_2)
m_222_sd = apply(m_222_tot,2,sd)

normalise <- function(a) {
  a = a - min(a)/max(a)-min(a)
}

slgrw = as.numeric(formatC(slug_rw_means, 2))
strtrw = as.numeric(formatC(strat_rw_means, 2))
annrw = as.numeric(formatC(annular_rw_means, 2))
plgrw = as.numeric(formatC(plug_rw_means, 2))
bubrw = as.numeric(formatC(bubble_rw_means, 2))
slg2rw = as.numeric(formatC(slug_w_rw_means, 2))
bub2rw = as.numeric(formatC(bubble_w_rw_means, 2))
tranrw = as.numeric(formatC(transient_w_rw_means, 2))

p1 = hist(slgrw, col=rgb(0,0,1,1), xlim = c(0,1), xlab = 'Void fraction', main = 'Slug void fraction (primary)',breaks = (100*(max(slgrw)-min(slgrw))))
p2 = hist(strtrw, col=rgb(1,0,0,1), xlab = 'Void fraction', xlim = c(0,1), main = 'Stratified void fraction',breaks = (100*(max(strtrw)-min(strtrw))))
p3 = hist(annrw, col=rgb(1,0,1,1/2), xlab = 'Void fraction', xlim = c(0,1), main = 'Annular void fraction (primary)',breaks = (100*(max(annrw)-min(annrw))))
p4 = hist(plgrw, col=rgb(0,1,0,1/4), xlab = 'Void fraction', xlim = c(0,1), main = 'Plug void fraction',breaks = (100*(max(plgrw)-min(plgrw))))
p5 = hist(bubrw, col=rgb(3/4,3/4,3/4,1/4), xlab = 'Void fraction', xlim = c(0,1), main = 'Bubble void fraction (primary)',breaks = (100*(max(bubrw)-min(bubrw))))
p6 = hist(slg2rw, col=rgb(0,0,1,1), xlab = 'Void fraction', xlim = c(0,1), main = 'Slug void fraction (secondary)',breaks = (100*(max(slg2rw)-min(slg2rw))))
p7 = hist(bub2rw,  col=rgb(3/4,3/4,3/4,1/4), xlab = 'Void fraction', xlim = c(0,1), main = 'Bubble void fraction (secondary)',breaks = (100*(max(bub2rw)-min(bub2rw))))
p8 = hist(tranrw, col=rgb(1,0,1,1/2), xlab = 'Void fraction', xlim = c(0,1), main = 'Transient void fraction (secondary)',breaks = (100*(max(tranrw)-min(tranrw))))

plot( p1, col=rgb(0,0,1,1), xlim=c(0,1), ylim = c(0,10000), xlab = 'Void fraction', main = 'Primary void fractions') # first histogram
plot( p2, col=rgb(1,0,0,1), xlim=c(0,1), add=T) 
plot( p3, col=rgb(1,0,1,1), xlim=c(0,1), add=T) 
plot( p4, col=rgb(0,1,0,1/3), xlim=c(0,1), add=T) 
plot( p5, col=rgb(3/4,3/4,3/4,1/4), xlim=c(0,1), add=T) 
legend('topright',c('Slug','Stratified','Annular','Plug','Bubble'),
       fill = c(rgb(0,0,1,1), rgb(1,0,0,1),rgb(1,0,1,1),rgb(0,1,0,1/3),rgb(3/4,3/4,3/4,1/4)),
       border = NA,cex = 0.6)

plot( p6, col=rgb(0,0,1,1), xlim=c(0,1), ylim = c(0,10000), xlab = 'Void fraction', main = 'Secondary void fractions')  # first histogram
plot( p2, col=rgb(1,0,0,1), xlim=c(0,1), add=T) 
plot( p8, col=rgb(1,0,1,1), xlim=c(0,1), add=T) 
plot( p4, col=rgb(0,1,0,1/3), xlim=c(0,1), add=T) 
plot( p7, col=rgb(3/4,3/4,3/4,1/4), xlim=c(0,1), add=T) 
legend('topright',c('Slug','Stratified','Transient','Plug','Bubble'),
       fill = c(rgb(0,0,1,1), rgb(1,0,0,1),rgb(1,0,1,1),rgb(0,1,0,1/3),rgb(3/4,3/4,3/4,1/4)),
       border = NA,cex = 0.6)
TransientVar

slgsd = as.numeric(formatC(Slugsd, 2))
strtsd = as.numeric(formatC(Stratsd, 2))
annsd = as.numeric(formatC(Annularsd, 2))
plgsd = as.numeric(formatC(Plugsd, 2))
bubsd = as.numeric(formatC(Bubblesd, 2))
slg2sd  = as.numeric(formatC(Slugsd_wang, 2))
bub2sd = as.numeric(formatC(Bubblesd_wang, 2))
trnsd = as.numeric(formatC(Transientsd, 2))

p1 = hist(slgsd, xlim = c(0,.3))
p2 = hist(strtsd, xlim = c(0,.3))
p3 = hist(annsd, xlim = c(0,.3))
p4 = hist(plgsd, xlim = c(0,.3))
p5 = hist(bubsd, xlim = c(0,.3))
p6 = hist(slg2sd, xlim = c(0,.3))
p7 = hist(bub2sd, xlim = c(0,.3))
p8 = hist(trnsd, xlim = c(0,.3))

plot( p1, col=rgb(0,0,1,1/2), xlim=c(0,.3), ylim = c(0,1000)) # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,.3), add=T) 
plot( p3, col=rgb(1,0,1,1/4), xlim=c(0,.3), add=T) 
plot( p4, col=rgb(0.5,1,0.5,1/4), xlim=c(0,.3), add=T) 
plot( p5, col=rgb(6/7,6/7,6/7,1/4), xlim=c0,.3, add=T) 

plot( p6, col=rgb(0,0,1,1/2), xlim=c(0,.3), ylim = c(0,1000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,.3), add=T) 
plot( p8, col=rgb(1,0,0,1/4), xlim=c(0,.3), add=T) 
plot( p4, col=rgb(0.5,1,0.5,1/4), xlim=c(0,.3), add=T) 
plot( p7, col=rgb(6/7,6/7,6/7,1/4), xlim=c(0,.3), add=T) 

plot(1:length(slug_rw_means),slug_rw_means, main = 'Slug Flow (Primary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(strat_rw_means),strat_rw_means, main = 'Stratified Flow (Primary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(annular_rw_means),annular_rw_means, main = 'Annular Flow (Primary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(plug_rw_means),plug_rw_means, main = 'Plug Flow (Primary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(bubble_rw_means),bubble_rw_means, main = 'Bubble Flow (Primary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(slug_w_rw_means),slug_w_rw_means, main = 'Plug Flow (Secondary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(bubble_w_rw_means),bubble_w_rw_means, main = 'Bubble Flow (Secondary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))
plot(1:length(transient_w_rw_means),transient_w_rw_means, main = 'Transient Flow (Secondary)', xlab = 'Frame', ylab = 'Mean Frame Value', ylim = c(0,1))

PlugMean
plot(1:length(PlugMean),PlugMean, main = 'Plug Flow (Primary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),SlugMean, main = 'Slug Flow (Primary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),SlugMean_wang, main = 'Slug Flow (Secondary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),AnnularMean, main = 'Annular Flow (Primary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),StratifiedMean, main = 'Stratified Flow (Primary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),BubbleMean, main = 'Bubble Flow (Primary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),BubbleMean_wang, main = 'Bubble Flow (Secondary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))
plot(1:length(PlugMean),TransientMean, main = 'Transient Flow (Secondary)', xlab = 'Pixel', ylab = 'Mean Pixel Value', ylim = c(0,1))

pairs(~PlugMean+BubbleMean+SlugMean+StratifiedMean+AnnularMean)
pairs(~PlugMean+BubbleMean_wang+SlugMean_wang+StratifiedMean+TransientMean)
PlugMean
length(BubbleMean)

primary = data.frame('Stratified' = StratifiedMean,'Plug' = PlugMean, 'Bubble' = BubbleMean,'Slug' = SlugMean,'Annular' = AnnularMean)
secondary = data.frame('Stratified' = StratifiedMean, 'Plug' = PlugMean, 'Bubble' = BubbleMean_wang,'Slug' = SlugMean_wang,'Transient' = TransientMean)

lo <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    geom_point()+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits = c(0,1))
}  

di <- function(data,mapping){
  ggplot(data = data, mapping = mapping)+
    scale_y_continuous(limits = c(0,30))
}   

ggpairs(primary, title = 'Primary labels' ,lower = list(continuous = wrap(lo)))
ggpairs(secondary, title = 'Secondary labels' , lower = list(continuous = wrap(lo)))
head(primary,1)
max(primary[1])
max(primary[2])
max(primary[3])
max(primary[4])
max(primary[5])
min(primary[1])
min(primary[2])
min(primary[3])
min(primary[4])
min(primary[5])

head(secondary,1)
max(secondary[1])
max(secondary[2])
max(secondary[3])
max(secondary[4])
max(secondary[5])
min(secondary[1])
min(secondary[2])
min(secondary[3])
min(secondary[4])
min(secondary[5])
plot(1:length(Transient_sd),Transient_sd, main = 'Transient Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Bubble_w_sd),Bubble_w_sd, main = 'Bubble (secondary label) Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Slug_w_sd),Slug_w_sd, main = 'Slug (secondary label) Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Bubble_sd),Bubble_sd, main = 'Bubble Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Plug_sd),Plug_sd, main = 'Plug Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Annular_sd),Annular_sd, main = 'Annular Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Stratified_sd),Stratified_sd, main = 'Stratified Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(Slug_sd),Slug_sd, main = 'Slug Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))

plot(1:length(m_221_mean),m_221_sd, main = 'Meas_221 Standard Deviation', xlab = 'Pixel',ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(m_220_mean),m_220_sd, main = 'Meas_220 Standard Deviation', xlab = 'Pixel', ylab = expression(paste(sigma)), ylim = c(0,1))
plot(1:length(m_222_mean),m_222_sd, main = 'Meas_222 Standard Deviation', xlab = 'Pixel',ylab = expression(paste(sigma)), ylim = c(0,1))

offset = c(7,5,3,2,2,1,1,rep(0,6),1,1,2,2,3,5,7) #Approximation of a circle
start = cumsum(c(1,20-offset[-20]*2))
end   = cumsum(c(20-offset*2))
y = array(NA, dim=c(20,20))
install.packages('fields')
library(fields)
rotate <- function(x) t(apply(x, 2, rev))
for (i in 1:20){
  y[i, (offset[i]+1):(20-offset[i])] = as.numeric(SlugMean[start[i]:end[i]])
}
rotate(y[,])
image.plot(rotate(y[,]), main = "Slug flow (primary)", zlim = c(0,1))

PlugMean+BubbleMean_wang+SlugMean_wang+StratifiedMean+TransientMean
+BubbleMean+SlugMean+AnnularMean
Slug_sd












aframe = data.frame(Annular_tot[,3:634])
aframe_a = aframe[,1:316]
aframe_b = aframe[,317:632]
names(aframe_b) <- names(aframe_a) 
aframe = rbind(aframe_a,aframe_b)

aframe[] <- lapply(aframe, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(aframe,class)

aframe2 = data.frame(tmp2041)
aframe2_a = aframe2[,1:316]
aframe2_b = aframe2[,317:632]
names(aframe2_b) <- names(aframe2_a) 
aframe2 = rbind(aframe2_a,aframe2_b)

aframe2[] <- lapply(aframe2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(aframe2,class)

aframe3 = data.frame(tmp2042)
aframe3_a = aframe3[,1:316]
aframe3_b = aframe3[,317:632]
names(aframe3_b) <- names(aframe3_a) 
aframe3 = rbind(aframe3_a,aframe3_b)

aframe3[] <- lapply(aframe3, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(aframe3,class)

aframe4 = data.frame(tmp2043)
aframe4_a = aframe4[,1:316]
aframe4_b = aframe4[,317:632]
names(aframe4_b) <- names(aframe4_a) 
aframe4 = rbind(aframe4_a,aframe4_b)

aframe4[] <- lapply(aframe4, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(aframe4,class)



det(cov(aframe))

aframe[["class"]] = 1
aframe2[["class"]] = 2
aframe3[["class"]] = 3
aframe4[["class"]] = 4


names(aframe2) <- names(aframe) 
names(aframe3) <- names(aframe) 
names(aframe4) <- names(aframe) 

aframe5 = do.call("rbind", list(aframe, aframe2, aframe3, aframe4))
aframe5['class']

eigen(cov(aframe))

summary(chisq.test(aframe2))

summary(aframe2)


summary(boxM(aframe5[,1:316], group = aframe5[,'class']))
boxM(aframe5[,1:316], group = aframe5[,'class'])

loadingscorep = c( 0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.02481662,  0.02586305,  0.01096546,
                   0.01439292,  0.02065811,  0.02091307,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.01252106,  0.01606984,  0.01018104,  0.00634183,  0.00889506,
                   0.01050204,  0.01062039,  0.0087068 ,  0.00450143,  0.00659065,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466, -0.01374595, -0.00858697,
                   -0.00720013, -0.00456376, -0.00108598,  0.00221566,  0.00461131,
                   0.00565769,  0.00510131,  0.00276705, -0.00100149, -0.00444836,
                   0.0030727 ,  0.00484584,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466, -0.00367849, -0.01518198, -0.01492335,
                   -0.01269079, -0.00982766, -0.00695179, -0.00434479, -0.00242831,
                   -0.0016825 , -0.00239175, -0.00447927, -0.00741873, -0.01041928,
                   -0.01260358, -0.01378154, -0.01437103,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466, -0.01191978, -0.01830228, -0.01829722,
                   -0.01726578, -0.0158389 , -0.01426948, -0.01273793, -0.01155742,
                   -0.01108709, -0.01155889, -0.01287605, -0.01484234, -0.01692834,
                   -0.01906772, -0.02087491, -0.01999645,  0.09193466,  0.09193466,
                   0.09193466, -0.01951744, -0.02341277, -0.02395058, -0.02366136,
                   -0.02324328, -0.02280681, -0.02226189, -0.02162892, -0.02103006,
                   -0.02070812, -0.02082783, -0.02150818, -0.02253504, -0.02399563,
                   -0.02575338, -0.02734452, -0.02780983, -0.02622966,  0.09193466,
                   0.09193466, -0.03124855, -0.03086244, -0.03065527, -0.0304138 ,
                   -0.03030739, -0.03031128, -0.03031106, -0.03014316, -0.02984258,
                   -0.02953043, -0.02945132, -0.02976824, -0.03041207, -0.03105275,
                   -0.031811  , -0.03281921, -0.03381923, -0.03506011,  0.09193466,
                   -0.02465665, -0.0378289 , -0.03727164, -0.03740049, -0.0377082 ,
                   -0.03799262, -0.03821359, -0.03827856, -0.03814932, -0.03783193,
                   -0.03758277, -0.03723892, -0.03682082, -0.03642432, -0.03649003,
                   -0.0373029 , -0.03837363, -0.03993828, -0.04041295, -0.04090414,
                   -0.03200603, -0.04177954, -0.04344701, -0.04391546, -0.04436379,
                   -0.04479718, -0.04507337, -0.04518855, -0.04506189, -0.04435094,
                   -0.04320475, -0.04195405, -0.04074208, -0.04020641, -0.0403968 ,
                   -0.04120299, -0.04226251, -0.0435246 , -0.04441325, -0.04367552,
                   -0.04359287, -0.04604666, -0.04750847, -0.0483986 , -0.0484581 ,
                   -0.04837721, -0.04777353, -0.04759766, -0.04755381, -0.04717589,
                   -0.04575237, -0.04400727, -0.04248414, -0.04189506, -0.04226707,
                   -0.04338385, -0.04468942, -0.04566537, -0.04654804, -0.04749095,
                   -0.04107171, -0.04875862, -0.04939328, -0.04923428, -0.04896449,
                   -0.04864961, -0.04845699, -0.04839442, -0.04836175, -0.04794093,
                   -0.04608999, -0.04433302, -0.04318203, -0.04270749, -0.04316786,
                   -0.04443745, -0.0459893 , -0.04698585, -0.04731288, -0.04332835,
                   -0.03458634, -0.04389317, -0.04506984, -0.04542036, -0.04588098,
                   -0.0476954 , -0.04843002, -0.04825323, -0.04773708, -0.04705295,
                   -0.04509675, -0.0431678 , -0.04166373, -0.03963033, -0.03955996,
                   -0.04092691, -0.04265088, -0.04377151, -0.04322742, -0.03485446,
                   -0.03481755, -0.04288785, -0.04281843, -0.04198574, -0.04096336,
                   -0.04021265, -0.03964581, -0.03910914, -0.03860118, -0.03840689,
                   -0.03725746, -0.03569945, -0.03489687, -0.03500924, -0.03613088,
                   -0.03806268, -0.04045577, -0.04270832, -0.04350652, -0.03427205,
                   0.09193466, -0.04171479, -0.0400483 , -0.03811796, -0.03610747,
                   -0.03446376, -0.0330133 , -0.03165856, -0.03055509, -0.03011422,
                   -0.02995277, -0.02881637, -0.02831227, -0.02874957, -0.03052427,
                   -0.03344813, -0.03649079, -0.039315  , -0.04203385,  0.09193466,
                   0.09193466, -0.02101077, -0.02172912, -0.02163697, -0.02039495,
                   -0.01937043, -0.01911386, -0.01905043, -0.01895593, -0.01901221,
                   -0.01923359, -0.01927456, -0.01899611, -0.01930656, -0.02054915,
                   -0.02175026, -0.02312263, -0.02414883, -0.02384407,  0.09193466,
                   0.09193466,  0.09193466, -0.01874782, -0.01728627, -0.01542821,
                   -0.01411872, -0.01315576, -0.01244044, -0.011805  , -0.01152911,
                   -0.01157336, -0.01342142, -0.01472445, -0.01637388, -0.01751531,
                   -0.01875644, -0.02038673, -0.02159921,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466, -0.01811526, -0.01450537, -0.01182462,
                   -0.01022207, -0.00461345, -0.00366586, -0.00361069, -0.00357645,
                   -0.00345092, -0.00335062, -0.0031824 , -0.00426132, -0.01140142,
                   -0.01546844, -0.01824856, -0.0209349 ,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466, -0.00249023, -0.00245687,
                   -0.00280573, -0.00316648, -0.00333456, -0.00342425, -0.00346111,
                   -0.00333362, -0.00319048, -0.00299367, -0.00296213, -0.00297018,
                   -0.00297359, -0.00311708,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   -0.00229193, -0.00303095, -0.00323292, -0.00334723, -0.0034636 ,
                   -0.00332583, -0.00300881, -0.00291779, -0.00290311, -0.00277206,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466, -0.0027959 , -0.00303045, -0.00349674,
                   -0.00320303, -0.00275473, -0.00273197,  0.09193466,  0.09193466,
                   0.09193466,  0.09193466,  0.09193466,  0.09193466,  0.09193466)

loadingscorep2 = c(1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02, -5.68639049e-02,
                   -6.62102670e-02, -1.00604269e-01, -1.00024743e-01, -1.03841353e-01,
                   -1.05849326e-01,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02, -6.25046509e-02, -7.23848171e-02, -1.03647383e-01,
                   -1.01030893e-01, -1.02256068e-01, -1.02335016e-01, -1.02634280e-01,
                   -1.03193521e-01, -1.02043920e-01, -9.31640041e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02, -7.07298401e-02,
                   -8.21117362e-02, -8.86573660e-02, -9.43965822e-02, -9.83724630e-02,
                   -1.01286329e-01, -1.02769799e-01, -1.03047857e-01, -1.02576028e-01,
                   -1.00985415e-01, -9.75600573e-02, -8.95770886e-02, -9.06061901e-02,
                   -9.07993194e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02, -7.93135898e-02, -7.21318760e-02,
                   -7.73870185e-02, -8.39045559e-02, -8.98111156e-02, -9.45464350e-02,
                   -9.79266856e-02, -9.96204546e-02, -9.96857495e-02, -9.81708275e-02,
                   -9.50657789e-02, -9.04088430e-02, -8.30918185e-02, -7.53162126e-02,
                   -7.22551844e-02, -6.44881123e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02, -6.90411823e-02, -6.67906327e-02,
                   -7.27915432e-02, -7.83025634e-02, -8.33248198e-02, -8.74589592e-02,
                   -9.02058477e-02, -9.14028573e-02, -9.08757994e-02, -8.87555956e-02,
                   -8.52601581e-02, -7.99334329e-02, -7.23212166e-02, -6.46127554e-02,
                   -5.88086189e-02, -5.60702957e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02, -5.76795312e-02, -5.35873341e-02, -5.79779562e-02,
                   -6.51216032e-02, -7.05238167e-02, -7.42937137e-02, -7.69943504e-02,
                   -7.85272415e-02, -7.87892502e-02, -7.77285293e-02, -7.54353945e-02,
                   -7.13433760e-02, -6.62284305e-02, -5.77918239e-02, -5.10643838e-02,
                   -4.61758340e-02, -4.40663494e-02, -4.58706436e-02,  1.82536010e-02,
                   1.82536010e-02, -3.95365893e-02, -4.34410477e-02, -4.82520735e-02,
                   -5.39316046e-02, -5.95130987e-02, -6.30597120e-02, -6.43442861e-02,
                   -6.45185536e-02, -6.37091206e-02, -6.17704306e-02, -5.81691294e-02,
                   -5.26773547e-02, -4.56744244e-02, -4.00337682e-02, -3.69426731e-02,
                   -3.45609869e-02, -3.19420918e-02, -1.60670891e-02,  1.82536010e-02,
                   -1.13733022e-02,  6.37038539e-03, -3.30036078e-02, -3.76468078e-02,
                   -4.13331490e-02, -4.45574711e-02, -4.70702311e-02, -4.82249777e-02,
                   -4.74593596e-02, -4.49759966e-02, -4.02871160e-02, -3.50194907e-02,
                   -2.84867968e-02, -2.30910514e-02, -1.85450218e-02, -3.07399534e-03,
                   -1.26374117e-03,  1.92925929e-03,  1.41812552e-03, -2.22436854e-03,
                   -2.23054391e-04,  1.47729130e-02,  1.21131218e-02, -1.88516985e-02,
                   -2.74858229e-02, -2.88334181e-02, -2.89108020e-02, -2.83218270e-02,
                   -2.59640934e-02, -2.21671032e-02, -1.73653574e-02, -9.74732543e-03,
                   7.96246548e-04,  1.43219451e-02,  1.52732029e-02,  1.55018800e-02,
                   1.40140597e-02,  1.25543966e-02,  9.93403049e-03,  4.34554436e-03,
                   2.02142061e-02,  2.34873942e-02,  2.28260421e-02,  2.25212811e-02,
                   2.23180936e-02,  1.62530736e-02, -1.93064080e-03, -2.94530716e-03,
                   -1.41944941e-03,  1.62540497e-02,  2.44772780e-02,  2.92156272e-02,
                   3.17592569e-02,  3.24629662e-02,  3.13569156e-02,  2.92486061e-02,
                   2.62399752e-02,  2.24253317e-02,  1.86464579e-02,  1.55427443e-02,
                   4.00236480e-02,  3.41862522e-02,  3.51847555e-02,  3.66436163e-02,
                   3.77814395e-02,  3.86169113e-02,  3.90905582e-02,  3.98192569e-02,
                   4.08694787e-02,  4.29646750e-02,  4.74514006e-02,  5.08347648e-02,
                   5.24541909e-02,  5.26346069e-02,  5.08826055e-02,  4.78906797e-02,
                   4.40922322e-02,  4.02200755e-02,  3.81629344e-02,  4.74952892e-02,
                   4.09187148e-02,  5.86676582e-02,  6.05839914e-02,  6.16697675e-02,
                   6.25082230e-02,  5.72327413e-02,  5.57652727e-02,  5.67081931e-02,
                   5.76713686e-02,  5.79929823e-02,  6.03314907e-02,  6.25196401e-02,
                   6.43243484e-02,  6.95883889e-02,  7.05952794e-02,  6.83312618e-02,
                   6.54437301e-02,  6.14607680e-02,  5.78938326e-02,  4.27389245e-02,
                   4.32929236e-02,  6.34034020e-02,  6.63066473e-02,  6.84238715e-02,
                   6.98434926e-02,  7.05372063e-02,  7.08318024e-02,  7.06342328e-02,
                   7.00516394e-02,  6.88614598e-02,  6.88930231e-02,  7.04258033e-02,
                   7.13694046e-02,  7.16834526e-02,  7.13932104e-02,  7.05976111e-02,
                   6.90256233e-02,  6.60143407e-02,  6.23851871e-02,  4.11437950e-02,
                   1.82536010e-02,  6.63202387e-02,  6.81766650e-02,  6.89900037e-02,
                   6.85850087e-02,  6.71472839e-02,  6.50515184e-02,  6.29776132e-02,
                   6.10011965e-02,  5.97966380e-02,  5.89243093e-02,  6.03233800e-02,
                   6.19991978e-02,  6.33564559e-02,  6.41935544e-02,  6.49944845e-02,
                   6.64012546e-02,  6.82344733e-02,  6.74929887e-02,  1.82536010e-02,
                   1.82536010e-02,  3.39133614e-02,  3.79765032e-02,  4.10245717e-02,
                   4.06152661e-02,  3.91737022e-02,  3.91725178e-02,  3.92971046e-02,
                   3.90060616e-02,  3.89342870e-02,  3.92768391e-02,  3.94050835e-02,
                   4.16362399e-02,  4.33608469e-02,  4.41996557e-02,  4.62629227e-02,
                   4.81760991e-02,  4.88462065e-02,  4.75412859e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  3.28254702e-02,  3.30408885e-02,
                   3.04672918e-02,  2.80779053e-02,  2.60467169e-02,  2.42240293e-02,
                   2.23114900e-02,  2.13762890e-02,  2.13847363e-02,  2.62826777e-02,
                   3.07665055e-02,  3.55790563e-02,  3.84392970e-02,  4.11813036e-02,
                   4.40792002e-02,  4.55058355e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  3.18380951e-02,  2.86290111e-02,
                   2.34967492e-02,  1.95000361e-02,  4.36869897e-03,  1.41808473e-03,
                   8.49322660e-04,  5.97561941e-04,  5.17184512e-04,  1.30710278e-03,
                   2.52012806e-03,  5.82064642e-03,  2.44423894e-02,  3.44845631e-02,
                   4.06183673e-02,  4.49995305e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  3.73775410e-04,
                   1.83771424e-04,  4.18972970e-04,  4.60031471e-04,  1.61281445e-04,
                   9.11790089e-06,  2.04332653e-04,  7.03022341e-04,  2.42189260e-03,
                   2.98807332e-03,  3.09415947e-03,  3.19149980e-03,  3.19687040e-03,
                   3.43880320e-03,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02, -5.10870831e-04, -4.15966638e-04, -1.05366776e-03,
                   -1.05463306e-03, -7.35384813e-05,  1.36794488e-03,  3.46392853e-03,
                   3.41416911e-03,  3.42822496e-03,  3.06498638e-03,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02, -3.86363434e-03,
                   -3.59813720e-03, -9.21946273e-04,  3.38467155e-03,  3.47357657e-03,
                   3.29833843e-03,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02,
                   1.82536010e-02,  1.82536010e-02,  1.82536010e-02,  1.82536010e-02)
y = array(NA, dim=c(20,20))
y2 = array(NA, dim=c(20,20))
for (i in 1:400){
  y[i] = as.numeric(loadingscorep[i])
}
for (i in 1:400){
  y2[i] = as.numeric(loadingscorep2[i])
}
image.plot(y[,], main = "Loading scores for PC1", zlim = c(-0.12,0.12))
image.plot(y2[,], main = "Loading scores for PC2", zlim = c(-0.12,0.12))







loadingscore = c( 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.01711331,  0.01612988,  0.00211077,
                 0.00396235,  0.00409152,  0.00626393,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.00493326,  0.0058985 , -0.0019099 , -0.00245211, -0.00081645,
                 -0.0001313 , -0.00064288, -0.0024581 , -0.00840411, -0.00362125,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203, -0.01913265, -0.01609957,
                 -0.01464263, -0.01196843, -0.00909116, -0.00656008, -0.00490025,
                 -0.00441623, -0.00513384, -0.00703855, -0.00987661, -0.01226798,
                 -0.00273243, -0.00465068,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203, -0.01217352, -0.02153426, -0.02120896,
                 -0.01936795, -0.01702802, -0.01465037, -0.01256936, -0.01113892,
                 -0.01067467, -0.01127978, -0.01284559, -0.01496282, -0.0170183 ,
                 -0.01819119, -0.01764337, -0.01875384,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203, -0.01850066, -0.02434299, -0.02468492,
                 -0.02399858, -0.02278111, -0.02137027, -0.01997986, -0.01891035,
                 -0.01843915, -0.01867994, -0.01952928, -0.02089247, -0.02229056,
                 -0.02350191, -0.02376765, -0.02310895,  0.09343203,  0.09343203,
                 0.09343203, -0.02396071, -0.02813057, -0.02927937, -0.02961288,
                 -0.02954184, -0.02911472, -0.02839465, -0.02754066, -0.02673008,
                 -0.0261747 , -0.02599675, -0.02634503, -0.02701039, -0.02798416,
                 -0.02903931, -0.02979563, -0.03038277, -0.02898226,  0.09343203,
                 0.09343203, -0.03364565, -0.03426962, -0.03496703, -0.03539416,
                 -0.03558051, -0.03548318, -0.03510546, -0.03445948, -0.033683  ,
                 -0.03303597, -0.03266219, -0.03270936, -0.03305323, -0.03341655,
                 -0.03405104, -0.03515479, -0.03639469, -0.03611687,  0.09343203,
                 -0.02813202, -0.03841151, -0.03965453, -0.04046013, -0.04118103,
                 -0.04157589, -0.04159704, -0.04113564, -0.04039227, -0.03957722,
                 -0.03892536, -0.03819064, -0.0375209 , -0.0371841 , -0.03745567,
                 -0.03820822, -0.03974015, -0.0427742 , -0.04031845, -0.03930173,
                 -0.03486004, -0.04196183, -0.04400449, -0.04535455, -0.04572948,
                 -0.04616853, -0.04630077, -0.04564335, -0.04466217, -0.04346861,
                 -0.04201915, -0.04069777, -0.03957523, -0.03918638, -0.03974515,
                 -0.04095187, -0.04261   , -0.04445926, -0.04361131, -0.04273283,
                 -0.04316847, -0.04503801, -0.04607009, -0.04696563, -0.04696988,
                 -0.04712701, -0.04699599, -0.04622123, -0.04509447, -0.0438745 ,
                 -0.04222043, -0.04066197, -0.03971299, -0.03954051, -0.04018764,
                 -0.04173957, -0.04354748, -0.04506928, -0.04545154, -0.04643357,
                 -0.03842668, -0.04615234, -0.04659201, -0.04625795, -0.04588743,
                 -0.04549546, -0.04523642, -0.04478515, -0.04348992, -0.04220931,
                 -0.04047283, -0.03885714, -0.03809267, -0.03824308, -0.03916192,
                 -0.04072905, -0.04245485, -0.04341663, -0.04444555, -0.03931632,
                 -0.03295788, -0.03813467, -0.03903076, -0.03930904, -0.0398198 ,
                 -0.04225329, -0.04317545, -0.04285291, -0.04122694, -0.03964063,
                 -0.03796217, -0.03655949, -0.03549714, -0.03321605, -0.03349341,
                 -0.03514245, -0.03657392, -0.03678916, -0.03821201, -0.03492717,
                 -0.03278023, -0.03671186, -0.03654748, -0.03556905, -0.03438506,
                 -0.03350864, -0.03295937, -0.03243708, -0.03113933, -0.03006008,
                 -0.02882829, -0.02774231, -0.02761467, -0.02835372, -0.02996341,
                 -0.03214397, -0.03448414, -0.0356162 , -0.03772486, -0.03435968,
                 0.09343203, -0.03569954, -0.03417351, -0.03220875, -0.03007512,
                 -0.02831541, -0.02670767, -0.02525551, -0.02421324, -0.02294806,
                 -0.02234162, -0.02170163, -0.02174479, -0.02267246, -0.024661  ,
                 -0.0275378 , -0.03053723, -0.03329206, -0.03596956,  0.09343203,
                 0.09343203, -0.01924152, -0.01996586, -0.0195702 , -0.01792847,
                 -0.01667282, -0.01605103, -0.01581022, -0.01556773, -0.01538567,
                 -0.01499719, -0.01497023, -0.01523977, -0.01549943, -0.01661891,
                 -0.01805034, -0.01964273, -0.02076687, -0.02038454,  0.09343203,
                 0.09343203,  0.09343203, -0.01643781, -0.01478638, -0.01278391,
                 -0.01141674, -0.01041204, -0.0097163 , -0.00911006, -0.00878964,
                 -0.00877096, -0.00991102, -0.01089038, -0.01219292, -0.01323219,
                 -0.01463791, -0.01650007, -0.01772462,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203, -0.01565394, -0.0116857 , -0.00900219,
                 -0.00757651, -0.00626434, -0.00520866, -0.00515593, -0.00507993,
                 -0.00490022, -0.00473418, -0.00442605, -0.00553518, -0.00797653,
                 -0.01136658, -0.01411767, -0.01689879,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203, -0.00352679, -0.00349428,
                 -0.0039755 , -0.00452424, -0.00483229, -0.00496148, -0.00493052,
                 -0.00471909, -0.00443501, -0.00411996, -0.00406991, -0.00407233,
                 -0.00406658, -0.00424489,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 -0.00328973, -0.00440114, -0.00484905, -0.0049877 , -0.00496056,
                 -0.00467445, -0.00411439, -0.00399044, -0.00397017, -0.00379905,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203, -0.00489534, -0.00502275, -0.00509125,
                 -0.00439597,  0.00189167,  0.00200482,  0.09343203,  0.09343203,
                 0.09343203,  0.09343203,  0.09343203,  0.09343203,  0.09343203)

loadingscore2 = c(-9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,  3.17698397e-02,
                  3.87575951e-02,  9.40888796e-02,  9.60590570e-02,  1.13088773e-01,
                  1.12236150e-01, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03,  3.62349251e-02,  4.70960793e-02,  9.33199287e-02,
                  9.40553617e-02,  9.64806328e-02,  9.80957030e-02,  9.94443411e-02,
                  1.00048883e-01,  1.02365485e-01,  8.92358977e-02, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,  7.18285608e-02,
                  7.88203441e-02,  8.47250747e-02,  8.84374523e-02,  9.14855275e-02,
                  9.43140467e-02,  9.64502920e-02,  9.75704322e-02,  9.75208048e-02,
                  9.58352186e-02,  9.22552656e-02,  8.30262456e-02,  7.69364826e-02,
                  8.08601632e-02, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03,  8.30658326e-02,  7.36340652e-02,
                  7.71504289e-02,  8.20393886e-02,  8.61949993e-02,  8.95556064e-02,
                  9.21732128e-02,  9.36871101e-02,  9.39732244e-02,  9.28058365e-02,
                  9.01198135e-02,  8.59697262e-02,  7.85291033e-02,  6.97802963e-02,
                  6.62936620e-02,  6.20033798e-02, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03,  7.20190827e-02,  6.88997062e-02,
                  7.47569984e-02,  7.90030148e-02,  8.26530214e-02,  8.55260510e-02,
                  8.73120179e-02,  8.79796038e-02,  8.74161462e-02,  8.55915541e-02,
                  8.26204506e-02,  7.75776506e-02,  6.93884403e-02,  6.07167512e-02,
                  5.36121757e-02,  5.42878828e-02, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03,  6.10242496e-02,  5.54892212e-02,  6.05629314e-02,
                  6.86723054e-02,  7.37788537e-02,  7.64976208e-02,  7.81076866e-02,
                  7.87554530e-02,  7.85158741e-02,  7.73650699e-02,  7.53217993e-02,
                  7.13036380e-02,  6.61535906e-02,  5.60140485e-02,  4.86348564e-02,
                  4.40268596e-02,  4.40931292e-02,  4.78332318e-02, -9.15342762e-03,
                  -9.15342762e-03,  4.27123512e-02,  4.71422400e-02,  5.21791507e-02,
                  5.83548005e-02,  6.45381991e-02,  6.79980655e-02,  6.84472205e-02,
                  6.78838290e-02,  6.66127483e-02,  6.45510893e-02,  6.06036459e-02,
                  5.41760168e-02,  4.57724027e-02,  3.97532270e-02,  3.73337467e-02,
                  3.60052566e-02,  3.39880420e-02,  8.61262975e-03, -9.15342762e-03,
                  -6.14329927e-03, -2.02873289e-02,  3.86284605e-02,  4.36907967e-02,
                  4.74513329e-02,  5.07229137e-02,  5.33644894e-02,  5.44728473e-02,
                  5.33379648e-02,  5.04593468e-02,  4.48637096e-02,  3.90091011e-02,
                  3.13906518e-02,  2.52970146e-02,  1.94499728e-02, -4.05092741e-03,
                  -5.97772675e-03, -7.88416256e-03, -1.16991739e-02, -6.73238400e-03,
                  -1.46622372e-02, -2.61848994e-02, -2.30022180e-02,  2.29337859e-02,
                  3.51222822e-02,  3.73971239e-02,  3.81947953e-02,  3.72557024e-02,
                  3.38763623e-02,  2.95742409e-02,  2.43168873e-02,  1.49587869e-02,
                  6.63729349e-04, -1.95952047e-02, -2.12566955e-02, -2.24480836e-02,
                  -2.07675485e-02, -1.88436396e-02, -1.91136907e-02, -1.10731035e-02,
                  -2.97711301e-02, -3.32061023e-02, -3.30871811e-02, -3.21472961e-02,
                  -3.05283346e-02, -2.02476191e-02,  9.06846859e-03,  1.03629504e-02,
                  7.21506435e-03, -1.90913010e-02, -2.91687786e-02, -3.38385526e-02,
                  -3.56573621e-02, -3.60868463e-02, -3.54186924e-02, -3.41027560e-02,
                  -3.17452075e-02, -2.84140540e-02, -2.60754071e-02, -2.30297622e-02,
                  -5.42242778e-02, -4.29919278e-02, -4.32059790e-02, -4.35517493e-02,
                  -4.38614011e-02, -4.41214792e-02, -4.40645788e-02, -4.47031465e-02,
                  -4.67730228e-02, -4.90531969e-02, -5.18741984e-02, -5.41459601e-02,
                  -5.51231411e-02, -5.48961620e-02, -5.36791741e-02, -5.19668047e-02,
                  -4.98879989e-02, -4.83273371e-02, -4.64258726e-02, -5.91381540e-02,
                  -5.68915544e-02, -6.68764863e-02, -6.84184567e-02, -6.87434771e-02,
                  -6.92353386e-02, -6.12226280e-02, -5.84519862e-02, -5.89181921e-02,
                  -6.12687035e-02, -6.27445305e-02, -6.37589605e-02, -6.46580195e-02,
                  -6.61840269e-02, -7.33008615e-02, -7.52423247e-02, -7.38069732e-02,
                  -7.30916160e-02, -7.22772422e-02, -6.69320493e-02, -5.98726097e-02,
                  -5.84245570e-02, -7.04611606e-02, -7.30719311e-02, -7.44524403e-02,
                  -7.51856613e-02, -7.50796766e-02, -7.45968233e-02, -7.36937202e-02,
                  -7.34041327e-02, -7.30505174e-02, -7.21855311e-02, -7.22833005e-02,
                  -7.25589513e-02, -7.31150864e-02, -7.39111772e-02, -7.45829177e-02,
                  -7.49773486e-02, -7.53185106e-02, -7.09339751e-02, -5.90485012e-02,
                  -9.15342762e-03, -7.32910670e-02, -7.43096964e-02, -7.38351150e-02,
                  -7.18942132e-02, -6.87279854e-02, -6.47654887e-02, -6.11424291e-02,
                  -5.82110450e-02, -5.79579980e-02, -5.77884505e-02, -5.80084956e-02,
                  -5.91383556e-02, -6.05253543e-02, -6.27168951e-02, -6.63984687e-02,
                  -7.04687421e-02, -7.40885145e-02, -7.45282445e-02, -9.15342762e-03,
                  -9.15342762e-03, -3.82901069e-02, -4.27786886e-02, -4.50344476e-02,
                  -4.23763724e-02, -3.89818622e-02, -3.72295637e-02, -3.64865457e-02,
                  -3.54759619e-02, -3.46222931e-02, -3.52049676e-02, -3.56786346e-02,
                  -3.75932076e-02, -4.03064904e-02, -4.28580058e-02, -4.63566590e-02,
                  -4.97905378e-02, -5.14264846e-02, -4.97241801e-02, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -3.47614228e-02, -3.31441768e-02,
                  -2.81054267e-02, -2.41080983e-02, -2.10001255e-02, -1.84907630e-02,
                  -1.60394303e-02, -1.47096160e-02, -1.47898248e-02, -1.94384990e-02,
                  -2.51502762e-02, -3.06527375e-02, -3.41643556e-02, -3.82308962e-02,
                  -4.29135791e-02, -4.53661634e-02, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -3.31983825e-02, -2.57984914e-02,
                  -1.80516555e-02, -1.32278204e-02, -7.99087608e-03, -3.61761099e-03,
                  -2.78479774e-03, -2.45452635e-03, -2.45768396e-03, -3.92642008e-03,
                  -6.02354257e-03, -1.06204692e-02, -1.89507579e-02, -2.96581646e-02,
                  -3.75325945e-02, -4.42083463e-02, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -1.49846811e-03,
                  -1.17311657e-03, -1.69652130e-03, -1.95720422e-03, -1.60015672e-03,
                  -1.44496138e-03, -1.83406522e-03, -2.61572235e-03, -5.43220724e-03,
                  -6.47239238e-03, -6.70122561e-03, -6.91686145e-03, -6.94082507e-03,
                  -7.36045919e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -4.53249140e-06, -5.70292425e-04,  2.81225307e-04,
                  2.00797640e-04, -1.40969051e-03, -3.65090357e-03, -7.00869645e-03,
                  -6.95549430e-03, -7.03144449e-03, -6.58757668e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,  4.55313767e-03,
                  4.07679864e-03, -1.18318155e-04, -6.83722294e-03, -1.60287461e-02,
                  -1.58851480e-02, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03,
                  -9.15342762e-03, -9.15342762e-03, -9.15342762e-03, -9.15342762e-03)
y = array(NA, dim=c(20,20))
y2 = array(NA, dim=c(20,20))
for (i in 1:400){
  y[i] = as.numeric(loadingscore[i])
}
for (i in 1:400){
  y2[i] = as.numeric(loadingscore2[i])
}

image.plot(y[,], main = "Loading scores for PC1", zlim = c(-0.12,0.12))
image.plot(y2[,], main = "Loading scores for PC2", zlim = c(-0.12,0.12))

variableimp = c(1.14142238e-01, 1.12623769e-01, 5.49383101e-03, 4.09806655e-02,
                6.30329756e-02, 8.65221949e-02, 1.29160649e-01, 1.42641184e-01,
                5.92198625e-02, 8.53254085e-03, 1.12852044e-02, 6.11792407e-03,
                3.82840313e-02, 1.45210659e-02, 5.58475023e-02, 1.40714941e-01,
                6.89249841e-02, 1.18379832e-01, 5.47051394e-02, 1.16614830e-02,
                3.34182708e-03, 4.80991327e-03, 2.57732463e-03, 8.51664805e-03,
                3.42127945e-03, 1.48323369e-02, 1.17411410e-03, 7.13903216e-02,
                5.06758017e-02, 8.79523748e-02, 3.69224986e-01, 2.05635497e-02,
                4.85393564e-03, 5.84225463e-03, 4.33346025e-03, 4.11260506e-03,
                4.79732102e-03, 3.02852612e-03, 1.55359960e-02, 1.01309182e-02,
                8.01070498e-03, 1.62037885e-02, 3.78032098e-03, 2.42905661e-02,
                5.25424259e-02, 1.64693541e-01, 5.46458534e-02, 1.59276682e-02,
                2.11733387e-03, 3.12169382e-03, 0.00000000e+00, 1.75802987e-01,
                4.54328877e-03, 1.78523446e-01, 8.85777208e-03, 7.04113362e-02,
                6.45997368e-03, 2.29340283e-03, 8.79366909e-03, 9.53708209e-03,
                2.11567633e-02, 1.54735731e-01, 1.13925442e-01, 1.74520271e-02,
                4.09107795e-03, 0.00000000e+00, 4.74757727e-02, 4.52228523e-03,
                0.00000000e+00, 2.03288693e-03, 1.26452329e-02, 4.19999144e-03,
                1.81903611e-03, 1.30418575e-03, 4.88805904e-03, 4.64798956e-03,
                1.34453448e-03, 9.90492516e-04, 1.25395698e-01, 1.27408345e-01,
                9.79636830e-03, 2.13382434e-02, 5.37883666e-03, 4.37282967e-03,
                4.14750608e-03, 2.81985250e-01, 7.01885274e-02, 2.61293847e-01,
                3.29584451e-01, 1.97481145e-01, 1.45279236e-03, 0.00000000e+00,
                6.21460240e-03, 6.27096250e-03, 5.14341325e-03, 5.09761320e-03,
                7.62685112e-03, 5.93562897e-02, 1.84629120e-01, 6.04216958e-03,
                1.80442079e-01, 2.16883141e-02, 6.15190505e-03, 3.89022113e-01,
                1.80421631e-02, 2.57534829e-01, 6.39491110e-01, 4.52067881e-01,
                2.22617349e-01, 1.33871257e-03, 1.58978314e-02, 2.23254724e-03,
                0.00000000e+00, 1.39483976e-03, 1.45121562e-02, 4.04953760e-02,
                1.18213029e-01, 9.39337861e-02, 8.47561214e-02, 1.43896328e-02,
                1.77003918e-01, 1.92050424e-01, 1.00000000e+00, 2.78915181e-01,
                5.29206288e-01, 7.07562199e-01, 4.96604310e-02, 3.78329630e-01,
                6.06733791e-03, 4.82369332e-03, 6.15642970e-03, 1.03722325e-02,
                2.99538263e-03, 3.88263982e-03, 1.73249628e-02, 2.45603105e-02,
                6.28895165e-02, 1.18532492e-01, 1.35902740e-01, 1.07011369e-01,
                4.73930853e-03, 2.11807401e-03, 2.36954653e-01, 5.09393727e-01,
                4.68202348e-01, 2.52690221e-01, 5.91327685e-02, 1.01593021e-03,
                0.00000000e+00, 5.17369270e-03, 2.23786805e-02, 4.26708687e-03,
                1.44741449e-02, 1.01461872e-02, 5.62573274e-03, 4.64518009e-03,
                1.31427488e-01, 3.03717931e-02, 3.97409207e-01, 9.09411328e-03,
                1.06602046e-02, 1.38664066e-02, 3.48157674e-02, 1.72058590e-02,
                2.15918114e-02, 8.82668285e-03, 1.05957228e-02, 4.69816243e-03,
                5.77786769e-03, 2.21890321e-02, 1.01943214e-02, 1.19289624e-02,
                1.09388455e-02, 3.20357932e-02, 2.22350390e-02, 6.79114242e-02,
                1.88649821e-01, 4.40420203e-01, 3.86218288e-01, 4.83987047e-01,
                5.89804386e-01, 2.30395885e-01, 5.29212635e-02, 3.16132836e-02,
                1.83946534e-02, 1.58639916e-02, 2.22639343e-02, 6.99648933e-02,
                4.10775131e-02, 2.86721552e-02, 3.10820527e-02, 5.53304806e-03,
                1.51485632e-02, 6.98123475e-02, 1.41350394e-01, 9.96501006e-02,
                4.40198043e-01, 2.78496266e-01, 1.77724997e-01, 7.66554998e-01,
                3.37555914e-01, 4.70090874e-01, 3.55269052e-01, 6.92705453e-02,
                2.56548016e-01, 2.23783646e-01, 1.69602905e-01, 9.64281894e-02,
                9.39862898e-02, 4.04020646e-02, 3.87776352e-02, 3.46120030e-02,
                4.21585722e-01, 9.39453954e-02, 9.08175470e-02, 5.08500913e-01,
                4.39519990e-01, 4.06258131e-01, 5.24018162e-01, 2.33635662e-01,
                4.47428776e-02, 8.33128597e-02, 1.36711235e-01, 5.37448700e-02,
                7.12093666e-02, 4.72986597e-02, 8.80623772e-02, 6.49463272e-02,
                1.14123114e-01, 1.23650534e-01, 5.73508376e-02, 1.13396878e-01,
                6.37248055e-02, 5.33109542e-02, 1.20809537e-01, 1.38684459e-01,
                6.25275679e-02, 4.45752196e-02, 8.57712226e-02, 6.30868661e-02,
                6.00616272e-02, 1.25601267e-01, 5.43530380e-02, 3.60756608e-02,
                4.99009428e-02, 4.11379194e-02, 3.34070125e-02, 2.69833647e-02,
                2.67728615e-02, 3.32037924e-02, 1.52493065e-02, 3.28998050e-02,
                4.65471831e-02, 4.19564264e-02, 5.40989107e-02, 4.96171432e-02,
                8.09813115e-02, 4.85254140e-02, 4.94104875e-02, 2.77905956e-02,
                9.97225456e-03, 2.79334532e-02, 2.37807868e-02, 5.06954000e-02,
                6.09564362e-02, 3.94247243e-02, 5.37903011e-03, 5.03659983e-02,
                3.81930213e-02, 2.24784908e-02, 6.43245832e-02, 2.34470875e-02,
                2.18320695e-02, 1.73038218e-02, 5.44208162e-02, 2.06299638e-02,
                1.30158850e-01, 7.92974602e-03, 8.14371014e-02, 1.86639973e-01,
                1.75451807e-02, 5.61635765e-02, 5.56687226e-02, 5.35209514e-02,
                4.20744450e-02, 1.75918606e-02, 6.68472493e-02, 1.46762957e-02,
                3.49763389e-02, 3.62382138e-02, 1.25934001e-01, 7.09681485e-02,
                8.20469917e-02, 5.38595793e-02, 8.34674792e-02, 1.28661359e-02,
                3.41317413e-01, 3.37185886e-02, 1.88965979e-01, 4.46506756e-02,
                1.18303060e-02, 1.10059993e-01, 6.67218293e-02, 8.96937742e-02,
                1.98734385e-02, 9.12313035e-02, 1.12364561e-01, 1.59367299e-01,
                3.66832254e-02, 6.59377879e-02, 9.00971590e-02, 3.80036976e-02,
                5.19787534e-02, 1.58138343e-01, 5.22208160e-02, 9.78580860e-02)
y = array(NA, dim=c(20,20))

for (i in 1:20){
  y[i, (offset[i]+1):(20-offset[i])] = as.numeric(variableimp[start[i]:end[i]])
}
print(y[,])
var_imp = rotate(y[,])
image.plot(var_imp, main = 'Random Forest Variable Importance'); box()

