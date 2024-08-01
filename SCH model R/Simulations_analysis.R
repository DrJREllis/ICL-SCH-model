
rm(list=ls())
require("scales")


outFileStub <- "test"

source("ResultsProcessingFunctions.R")

data_path <-"outputs/"

load(paste0(data_path,outFileStub,".results.RData"))

## pre-process data...
hostDataRaw <- extractHostData2(foreachResults)  
currentK_epg<-0.87
ageBand<-c(5,15)
currentLambda<-hostDataRaw[[1]]$params$lambda
Unfertilized<-FALSE
villageSampleSize <-100 
nSamples<-2
CountThreshold<-50

timeIndices <- 1:length(hostDataRaw[[1]]$timePoints)

results<-sapply(timeIndices,FUN=function(x){return(getSampledDetectedPrevByVillage(hostDataRaw, x, nSamples,Unfertilized,villageSampleSize,ageBand))})
resultsFinal<-colSums(results)/hostDataRaw[[1]]$params$repNum


plot(hostDataRaw[[1]]$timePoints,resultsFinal*100,'l',xlab='Time (Years)',ylab='Prevalence (%)',ylim = c(0,80),main="",col="red",lwd=3,cex.lab=1.5,cex.axis=1.5, xaxt = "n")
axis(1, at=c(50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100), labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50),cex.axis=1.5) #re-label x-axis

CD.sorted <- apply(results,2,sort,decreasing=F)
polygon(c(hostDataRaw[[1]]$timePoints,rev(hostDataRaw[[1]]$timePoints)),c(CD.sorted[0.05*hostDataRaw[[1]]$params$repNum,]*100,rev(CD.sorted[0.95*hostDataRaw[[1]]$params$repNum,]*100)),col=scales::alpha('red',.2),border="white")
#lines(hostDataRaw[[1]]$timePoints,resultsFinal*100,'l',xlab='Time (Years)',ylab='Prevalence (%)',ylim = c(0,100),xlim = c(10,25),col="black",lwd=2)
ageBand<-c(16,80)
resultsadult<-sapply(timeIndices,FUN=function(x){return(getSampledDetectedPrevByVillage(hostDataRaw, x, nSamples,Unfertilized,villageSampleSize,ageBand))})
resultsFinaladult<-colSums(resultsadult)/hostDataRaw[[1]]$params$repNum
print(resultsFinaladult)
lines(hostDataRaw[[1]]$timePoints,resultsFinaladult*100,col="blue",lwd=3)
CD.sorted1 <- apply(resultsadult,2,sort,decreasing=F)
lines(hostDataRaw[[1]]$timePoints,CD.sorted1[0.05*hostDataRaw[[1]]$params$repNum,]*100,col="light blue",lwd=2)#5%
lines(hostDataRaw[[1]]$timePoints,CD.sorted1[0.95*hostDataRaw[[1]]$params$repNum,]*100,col="light blue",lwd=2)#95%
polygon(c(hostDataRaw[[1]]$timePoints,rev(hostDataRaw[[1]]$timePoints)),c(CD.sorted1[0.05*hostDataRaw[[1]]$params$repNum,]*100,rev(CD.sorted1[0.95*hostDataRaw[[1]]$params$repNum,]*100)),col=scales::alpha('blue',.2),border="white")
legend('topright',c('SAC prevalence','Adult prevalence'),col=c('red','blue'),lty=c(1,1),lwd=c(2,2), cex=1,bty = "n")
#legend('topleft',c('B'),col=c('black'), cex=1.2,bty = "n")
abline(h=1,lty=2,col="gray47",lwd=2) #add horizontal line at 1% prev
abline(h=5,lty=2,col="gray47",lwd=2) #add horizontal line at 5% prev
#dev.off()
#legend('topleft',c('D'),col=c('black'), cex=1.2,bty = "n")
################################################################
#######################################
ageBand<-c(5,15)
resultsHeavy<-sapply(timeIndices,FUN=function(x){return(getSampledDetectedPrevHeavyBurdenByVillage(hostDataRaw, x, nSamples,Unfertilized,villageSampleSize,ageBand))})
resultsFinalHeavy<-colSums(resultsHeavy)/hostDataRaw[[1]]$params$repNum
#tiff("R:/SCH_vaccine/KKvacc_code/Cluster_run/Mass_vaccination/low_settings_0_75_SAC_high_intensity.tif", res=500, compression = "lzw", height=13.94, width=14.63, units="cm")
plot(hostDataRaw[[1]]$timePoints,resultsFinalHeavy*100,'l',xlab='Time (Years)',ylab='Heavy intensity prevalence (%)',ylim = c(0,50),col="red",lty=1,lwd=3,cex.lab=1.5,cex.axis=1.5, xaxt = "n")
axis(1, at=c(50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100), labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50),cex.axis=1.5) #re-label x-axis
CD.sortedSAC <- apply(resultsHeavy,2,sort,decreasing=F)
polygon(c(hostDataRaw[[1]]$timePoints,rev(hostDataRaw[[1]]$timePoints)),c(CD.sortedSAC[0.05*hostDataRaw[[1]]$params$repNum,]*100,rev(CD.sortedSAC[0.95*hostDataRaw[[1]]$params$repNum,]*100)),col=scales::alpha('red',.2),border="white")
ageBand<-c(16,80)
resultsadultHeavy<-sapply(timeIndices,FUN=function(x){return(getSampledDetectedPrevHeavyBurdenByVillage(hostDataRaw, x, nSamples,Unfertilized,villageSampleSize,ageBand))})
resultsFinaladultHeavy<-colSums(resultsadultHeavy)/hostDataRaw[[1]]$params$repNum
lines(hostDataRaw[[1]]$timePoints,resultsFinaladultHeavy*100,'l',xlab='Time (Years)',ylab='Prevalence (%)',ylim = c(0,100),xlim = c(10,25),col="blue",lty=1,lwd=3, xaxt = "n")

CD.sortedAdult <- apply(resultsadultHeavy,2,sort,decreasing=F)

polygon(c(hostDataRaw[[1]]$timePoints,rev(hostDataRaw[[1]]$timePoints)),c(CD.sortedAdult[0.05*hostDataRaw[[1]]$params$repNum,]*100,rev(CD.sortedAdult[0.95*hostDataRaw[[1]]$params$repNum,]*100)),col=scales::alpha('blue',.2),border="white")


abline(h=1,lty=2,col="gray47",lwd=2) #add horizontal line at 1% prev
abline(h=5,lty=2,col="gray47",lwd=2) #add horizontal line at 5% prev
legend('topright',c('SAC heavy intensity prevalence','Adult heavy intensity prevalence','1% and 5% WHO goal'),col=c('red','blue','gray47'),lty=c(1,1,2),lwd=c(2,2,2), cex=1,bty = "n")
#legend('topleft',c('B'),col=c('black'), cex=1.2,bty = "n")
#dev.off()
############
