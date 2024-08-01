#For testing:
#setwd("R:/ModellingOutput/2015.06.22_Stoch_R_GoldStandardParams-AdjustCoverageAndEfficacy/")
#source("Q:/Modelling/Projects/StochasticRmodel-currentworkinprogress/helsim_JY_FUNC_JT_b.R")
#source("Q:/Modelling/General-purpose functions and scripts/ResultsProcessingFunctionsV2.R")
#ageBreaks = readParam("treatmentBreaks", "Q:/Modelling/Projects/StochasticRmodel-currentworkinprogress/AscarisParameters.txt")




# General function to get relevant host data out of the foreachResults into a form more practical for plotting etc
extractHostData = function(foreachResults)
{  
  nrReps = length(foreachResults)
  nrTimepoints = length(foreachResults[[1]]$results)
  nrHosts = length(foreachResults[[1]]$results[[1]]$worms$total)
  
  # worms (total or female) over time (wot or femwot) will be across all reps, the total pool of individuals across time
  wot = array(dim=c(nrReps,nrTimepoints,nrHosts)) # an array is just a multidimensional matrix. obviously. [shakes fist at R].
  femwot = array(dim=c(nrReps,nrTimepoints,nrHosts))
  
  # host ages, in the same format as wot
  ages = array(dim=c(nrReps,nrTimepoints,nrHosts))
  
  # adherences factors, ditto (using on adherenceFactorAtChemo currently)
  # NOTE: these are no longer useful for identifying systematic noncompliers. Use the below instead
  adherenceFactors = array(dim=c(nrReps,nrTimepoints,nrHosts))
  
  # Keep track of who's a complier (or systematic noncomplier)
  compliers = array(dim=c(nrReps,nrTimepoints,nrHosts))
  
  # final values of the free living populatation (used for determining elimination)
  finalFreeLiving = c()
  
  # attendance record and ages at attendance
  attendanceRecord = list()
  ageAtChemo = list()
  adherenceFactorAtChemo = list()
  
  # The output times for each rep (though will generally be the same, may not be in some circumstances)
  times = list()
  
  # The parameters used in that iteration/village
  params = list()
  
  
  
  # ideally there would be a list-based way of slicing this... pull the relevant data out
  for (rep in 1:nrReps)
  {
    attendanceRecord[[rep]] = foreachResults[[rep]]$attendanceRecord
    ageAtChemo[[rep]] = foreachResults[[rep]]$ageAtChemo
    finalFreeLiving[rep] = foreachResults[[rep]]$results[[nrTimepoints]]$freeLiving   
    adherenceFactorAtChemo[[rep]] = foreachResults[[rep]]$adherenceFactorAtChemo
    times[[rep]] = foreachResults[[rep]]$params$outTimings
    params[[rep]] = foreachResults[[rep]]$params
    
    for (timepoint in 1:nrTimepoints)
    {
      wot[rep, timepoint, ] = foreachResults[[rep]]$results[[timepoint]]$worms$total
      femwot[rep, timepoint, ] = foreachResults[[rep]]$results[[timepoint]]$worms$female
      ages[rep, timepoint, ] = foreachResults[[rep]]$results[[timepoint]]$time - foreachResults[[rep]]$results[[timepoint]]$hosts$birthDate
      adherenceFactors[rep, timepoint, ] = foreachResults[[rep]]$results[[timepoint]]$adherenceFactors
      compliers[rep, timepoint, ] = foreachResults[[rep]]$results[[timepoint]]$compliers
    }
  }
  
  return(list(wormsOverTime=wot, femaleWormsOverTime=femwot, ages=ages, finalFreeLiving=finalFreeLiving, adherenceFactors=adherenceFactors, adherenceFactorAtChemo=adherenceFactorAtChemo,
              attendanceRecord=attendanceRecord, ageAtChemo=ageAtChemo, compliers=compliers, times = times, params=params))  
}

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# General function to get relevant host data out of the foreachResults into a form more practical for plotting etc
extractHostData2 = function(foreachResults)
{  
  nrReps = length(foreachResults)
  nrTimepoints = length(foreachResults[[1]]$results)
  ##nrHosts = length(foreachResults[[1]]$results[[1]]$worms$total) ## Not needed. 
  
  
  # ideally there would be a list-based way of slicing this... pull the relevant data out
  output = list()  ## put the villages as list members in here...
  for (rep in 1:nrReps)
  {
    currentOutputVillage <- list()
    currentDataVillage <- foreachResults[[rep]]
    
    currentVillageResults <- currentDataVillage$results
    currentOutputVillage$wormsOverTime <- sapply(currentVillageResults,FUN=function(x) {x$worms$total})
    currentOutputVillage$femaleWormsOverTime <- sapply(currentVillageResults,FUN=function(x) {x$worms$female})
    currentOutputVillage$VaccinationState <- lapply(currentVillageResults,FUN=function(x) {x$vaccState})  #KK not sure check
    currentOutputVillage$freeLiving <- lapply(currentVillageResults,FUN=function(x) {x$freeLiving})
    currentOutputVillage$ages <- sapply(currentVillageResults,FUN=function(x) {x$time - x$hosts$birthDate})
    currentOutputVillage$adherenceFactors <- sapply(currentVillageResults,FUN=function(x) {x$adherenceFactors})
    currentOutputVillage$compliers <- sapply(currentVillageResults,FUN=function(x) {x$compliers})
    currentOutputVillage$totalPop <- nrow(currentVillageResults[[1]]$worms)
    currentOutputVillage$timePoints <- sapply(currentVillageResults,FUN=function(x) {x$time}) ## JET: record the time as well...
    currentOutputVillage$attendanceRecord <- currentDataVillage$attendanceRecord
    currentOutputVillage$ageAtChemo <- currentDataVillage$ageAtChemo
    currentOutputVillage$ageAtVaccine <- currentDataVillage$ageAtVaccine #KK added not sure check
    currentOutputVillage$finalFreeLiving <- currentVillageResults[[nrTimepoints]]$freeLiving
    currentOutputVillage$adherenceFactorAtChemo <- currentDataVillage$adherenceFactorAtChemo
    currentOutputVillage$params <- currentDataVillage$params
    currentOutputVillage$times <- currentDataVillage$params$outTimings
    output[[rep]] <- currentOutputVillage
  }
  
  return(output)  
}


## Return a set of readings of egg counts from a vector of individuals, according to their repro biology. 
## Takes a vector of total worms and female worms and a flag for whether unfertilized worms generate eggs and param list. 
## Returns a random set of egg count readings from a single sample. 
# getSetOfEggCounts <- function(total,female,Unfertilized,p)
# {
# 	eggProducers <- female
# 	if(!Unfertilized)
# 	{
# 		## only fetilized worms generate eggs.
# 		fert <- total!=female
# 		eggProducers[!fert] <- 0
# 	}
# 	else {
# 	  male = currentOutputVillage$wormsOverTime - currentOutputVillage$femaleWormsOverTime
# 	  eggProducers = pmin(male, currentOutputVillage$femaleWormsOverTime)
# 
# 	}
# 
# 	meanCount <- eggProducers*p$lambda*p$z^eggProducers
# 
# 	readings <- rnbinom(length(meanCount),mu=meanCount,size=p$k_epg)
# 	return(readings)
# }
# getSetOfEggCounts <- function(total,female,Unfertilized,p)
# {
#   eggProducers <- female
# 
#   if(!Unfertilized)
#   {
#     ## only fetilized worms generate eggs.
#     fert <- total!=female
#     eggProducers[!fert] <- 0
#   }
#   if (p$reproFuncName == "epgMonog")
#   {
#     male = total - female
#     eggProducers = pmin(male, female)
#   }
# 
#   meanCount <- eggProducers*p$lambda*p$z^eggProducers
# 
#   readings <- rnbinom(length(meanCount),mu=meanCount,size=p$k_epg)
#   return(readings)
# }
###this is to match Ben's results
getSetOfEggCounts <- function(total,female,Unfertilized,p,vaccMod)
{
  newLambda <- p$lambda*vaccMod
  eggProducers <- female
  meanCount <- eggProducers*newLambda*p$z^eggProducers
  
  if(!Unfertilized)
  {
    ## only fetilized worms generate eggs.
    fert <- total!=female
    eggProducers[!fert] <- 0  
    meanCount <- eggProducers*newLambda*p$z^eggProducers
  }
  if (p$reproFuncName == "epgMonog")
  {
    male = total - female
    eggProducers = pmin(male, female)
    meanCount <- eggProducers*newLambda*p$z^total
  }
  
  
  readings <- rnbinom(length(meanCount),mu=meanCount,size=p$k_epg)
  return(readings)
}

## Takes: village list object, timeIndex, nSamples=1, Unfertilized=TRUE
## Returns: Mean egg count across readings by host. 
getVillageMeanCountsByHost <- function(villageList, timeIndex, nSamples=1, Unfertilized=FALSE)
{
  villageSize <- nrow(villageList$wormsOverTime)
  meanEggsByHost <- rep(0,villageSize)
  
  ## Calculate vaccine-modification of egg output. 
  vaccMod <- villageList$params$v2[villageList$VaccinationState[[timeIndex]]]
  
  for(i in 1:nSamples) ## calculate mean egg count. 
  {
    meanEggsByHost <- meanEggsByHost + getSetOfEggCounts(villageList$wormsOverTime[,timeIndex],villageList$femaleWormsOverTime[,timeIndex],Unfertilized,villageList$params,vaccMod)
  }
  return(meanEggsByHost/nSamples)
}

## Takes: villageData, timeIndex, nSamples=1, Unfertilized=TRUE,villageSampleSize,ageGroup=c(-1,120)
## Returns: sampled, age-cat prevalence. 
## This function needs global variables currentK_epg and currentLambda to function!!!
getAgeCatSampledPrevByVillage <- function(villageData, timeIndex, nSamples=2, Unfertilized=FALSE,villageSampleSize,ageBand) #ageBand=c(-1,120) for the whole population KK
{
  ## DEBUG LINES BECAUSE K-EPG AND LAMBDA MIGHT BE ABSENT/WRONG IN THE PARAM LISTS. 
  villageData$params$k_epg <- currentK_epg
  villageData$params$lambda <- currentLambda
  ## THESE SHOULD BE IN THE PARAMETER FILES, BUT WE MAY NEED TO ADJUST THEM AFTER THE RUN. 
  
  ## get readings from the hosts. 
  meanEggCounts <- getVillageMeanCountsByHost(villageData, timeIndex, nSamples, Unfertilized)
  
  ## get ages, filter age group.
  ageBreaks <- c(-10,ageBand,150)  ## any reasonable age group will be labelled 2. 
  ageGroups <- cut(villageData$ages[,timeIndex],breaks=ageBreaks,labels=1:3) 
  currentAgeGroupMeanEggCounts <- meanEggCounts[ageGroups==2]
  
  ## do sampling. Don't sample with replacement more than you have.  
  ageGroupSize <- length(currentAgeGroupMeanEggCounts) 
  #hostSampleSize <- floor(ageGroupSize*hostSampleSizeFrac)
  if(villageSampleSize > ageGroupSize) 
  {
    stop("Village smaller than sample size")    ## Don't sample no people...
  }
  
  mySample <- sample(x=currentAgeGroupMeanEggCounts,size=villageSampleSize)
  return(sum(nSamples*mySample > 0.9)/villageSampleSize) ## multiply the mean count to get total, which needs to be 1 or more. 
}

getAgeCatSampledPrevHeavyBurdenByVillage <- function(villageData, timeIndex, nSamples=2, Unfertilized=FALSE,villageSampleSize,ageBand) #ageBand=c(-1,120) for the whole population KK
{
  ## DEBUG LINES BECAUSE K-EPG AND LAMBDA MIGHT BE ABSENT/WRONG IN THE PARAM LISTS. 
  villageData$params$k_epg <- currentK_epg
  villageData$params$lambda <- currentLambda
  ## THESE SHOULD BE IN THE PARAMETER FILES, BUT WE MAY NEED TO ADJUST THEM AFTER THE RUN. 
  
  ## get readings from the hosts. 
  meanEggCounts <- getVillageMeanCountsByHost(villageData, timeIndex, nSamples, Unfertilized)
  
  ## get ages, filter age group.
  ageBreaks <- c(-10,ageBand,150)  ## any reasonable age group will be labelled 2. 
  ageGroups <- cut(villageData$ages[,timeIndex],breaks=ageBreaks,labels=1:3) 
  currentAgeGroupMeanEggCounts <- meanEggCounts[ageGroups==2]
  
  ## do sampling. Don't sample with replacement more than you have.  
  ageGroupSize <- length(currentAgeGroupMeanEggCounts) 
  #hostSampleSize <- floor(ageGroupSize*hostSampleSizeFrac)
  if(villageSampleSize > ageGroupSize) 
  {
    stop("Village smaller than sample size")    ## Don't sample no people...
  }
  
  mySample <- sample(x=currentAgeGroupMeanEggCounts,size=villageSampleSize)
  
  return(sum(mySample >= CountThreshold)/villageSampleSize)
  #return(sum(nSamples*mySample > 0.9)/villageSampleSize) ## multiply the mean count to get total, which needs to be 1 or more. 
}

##Takes: hostData, timeIndex, nSamples=1,Unfertilized=TRUE,villageSampleSize,ageGroup=c(-1,120)
## Returns: array of detected prevalences by village. 
getSampledDetectedPrevByVillage <- function(hostData, timeIndex, nSamples=2,Unfertilized=FALSE,villageSampleSize,ageBand)
{
  ## call function to age-categorize and sample on each village. 
  sampledVillagePrevs <- sapply(hostData,FUN=getAgeCatSampledPrevByVillage,timeIndex=timeIndex, nSamples=nSamples, Unfertilized=Unfertilized,villageSampleSize=villageSampleSize,ageBand=ageBand)
  return(sampledVillagePrevs)
}
getSampledDetectedPrevHeavyBurdenByVillage <- function(hostData, timeIndex, nSamples=2,Unfertilized=FALSE,villageSampleSize,ageBand)
{
  ## call function to age-categorize and sample on each village. 
  sampledVillagePrevs <- sapply(hostData,FUN=getAgeCatSampledPrevHeavyBurdenByVillage,timeIndex=timeIndex, nSamples=nSamples, Unfertilized=Unfertilized,villageSampleSize=villageSampleSize,ageBand=ageBand)
  return(sampledVillagePrevs)
}





## Returns: array of ultimate elimination by village. Elimination = TRUE. 
getEliminationByVillage <- function(hostData)
{
  eliminated <- sapply(hostData,function(x) {x$finalFreeLiving < x$params$equiData$L_breakpoint})
  #if(anyNA(eliminated)) #anyNA not in MW (22 November 2016) base functions so changed it to any(is.na(x))
  if(any(is.na(eliminated)))  
  {
    eliminated <- sapply(hostData,function(x) {x$finalFreeLiving < 2})  ## default guess value in case getEquilibrium(...) outputs NA. 
  }
  return(eliminated)
}

## A function to repeatedly sample (with replacement) N villages from selection with required prevalence range.
## Calculate frac villages that threshold says should go to elim and compare to frac that really do.   
## Takes: hostData structure, N (#villages to sample), index of Study Start, village sample size (samples in village), prevalence range, number of times to bootstrap-sample NN villages, 
## Takes: deltaPrev
calculatePrecision <- function(hostData, NN, indexStudyStart,villageSampleSize,prevRange, nVillageBootstrapReps, deltaPrevs)
{
  ## what kind of individual testing? 
  nReadings <- 2
  Unfertilized <- FALSE
  ageRange <- c(5,15) 
  
  ## Timing indices of the final study end and follow-up. SHOULD BE ARGUMENTS!!!
  index_t0 <- 6
  index_t1 <- 7
  
  villagePrevalences <- getSampledDetectedPrevByVillage(hostData,indexStudyStart,nSamples=nReadings,Unfertilized=Unfertilized,villageSampleSize=villageSampleSize,ageBand=ageRange)
  
  prevBreaks <- c(-0.1,prevRange,1.1)
  inRangeIndices <- cut(villagePrevalences,breaks=prevBreaks,labels=1:3)
  
  validIndices <- which(inRangeIndices==2)
  
  statValue <- NA
  statStatus <- NA
  fracDetectedElims=rep(0,nVillageBootstrapReps)
  fracRealElims=rep(0,nVillageBootstrapReps)
  for(i in 1:nVillageBootstrapReps)
  {
    currentVillageSampleIndices <- sample(validIndices,size=NN,replace=TRUE)
    
    currentHostData <- hostData[currentVillageSampleIndices]
    
    prevs_t0 <- getSampledDetectedPrevByVillage(currentHostData,index_t0,nSamples=nReadings,Unfertilized=Unfertilized,villageSampleSize=villageSampleSize,ageBand=ageRange)
    prevs_t1 <- getSampledDetectedPrevByVillage(currentHostData,index_t1,nSamples=nReadings,Unfertilized=Unfertilized,villageSampleSize=villageSampleSize,ageBand=ageRange)
    
    #currentStat <- prevs_t1 - prevs_t0  ## prevalence diff. 
    #currentStat <- prevs_t1  ## pure prevalence.
    currentStat <- prevs_t0  ## pure prevalence AT END OF STUDY. 
    detectedElims <- currentStat < deltaPrevs*0.6  ## 0.6 is roughly where it falls for low prevs (see excel file SampleSize.xlsx)
    fracDetectedElims[i] <- sum(detectedElims)/length(detectedElims)
    
    realElims <- getEliminationByVillage(currentHostData)
    fracRealElims[i] <- sum(realElims)/length(realElims)
    
    ## collect stat distributions 
    if(length(statValue)<3000)
    {
      statValue <- c(statValue,currentStat)
      statStatus <- c(statStatus,realElims)
    }
  }
  
  output <- list(fracDetectedElims=fracDetectedElims,fracRealElims=fracRealElims,statValue=statValue[-1],statStatus=statStatus[-1]) ## remove first NA. 
  return(output)
}

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################



# TODO:   replace on the other end with extractHostData then x$wormsOverTime[rep,,]. might need to adapt code.
#getWormsOverTime = function(foreachResults)

# hostData$wormsOverTime[elim,,]
# sum(elim)
# sum(!elim)
# for elim, call functions with elimHostData = list(wormsOverTime=hostData$wormsOverTime[eliminated,,], ages=hostData$ages[eliminated,,])  or !eliminated


# Get a boolean vector representing the reps in which we see elimination
getEliminated = function(ffl, threshold, thresholdInvalidBounds){
  thresholdInvalid = sum( (ffl > thresholdInvalidBounds[1])  &  (ffl < (thresholdInvalidBounds[2])) )
  print(paste0("Nr of reps with ambiguous final states: ", thresholdInvalid))
  return(ffl < threshold )
}





# Get summarised data across treatments on what proportion of repeats have led to elimination,
# as well as data on finalFreeLiving so we can check whether all reps have reached equilibrium
# If a file doesn't exist, substitute a NA for that result.
# This function will break if the first run specified doesn't exist (even if subsequent ones do), and do something unspecified
# (probably bad) if the runs specified don't all have the same number of hosts.
getEliminated_MultipleTreatments = function(dataLocation, treatmentsToCover, eliminationThreshold, thresholdInvalidBounds){
  
  time = proc.time()
  
  propElim = data.frame()
  finalFreeLiving = c()
  thresholdInvalid = c()
  propElim = c()
  thresholdInvalid = c()
  for (trt in treatmentsToCover){
    
    t = round((proc.time() - time)[["elapsed"]]) # round() rounds to the nearest even number, so down for 0.5 but up for 1.5. Becoming increasingly certain that R has been sent to test me.
    cat(paste0(t, " secs: processing ", trt, ".results.RData\n"))
    
    
    f = paste0(dataLocation, trt, ".results.RData")
    if (file.exists(f)){
      load(f)
      hostData = extractHostData(foreachResults) # Pull out host data; worm burdens, ages and final value of free living population
      eliminated = getEliminated(hostData$finalFreeLiving, eliminationThreshold, thresholdInvalidBounds) # Logical "this rep eliminated/not" vector
      # Is finalFreeLiving above zero but not much larger than the elimination threshold in any reps? Indicates simulation not settled after treatment.
      # More than a very small fraction of these likely to skew results.
      nearThreshold = (hostData$finalFreeLiving > thresholdInvalidBounds[1])  &  (hostData$finalFreeLiving < thresholdInvalidBounds[2])
      
      propElim = c(propElim, (sum(eliminated) / length(eliminated))) # proportion eliminated under these settings
      finalFreeLiving = c(finalFreeLiving, hostData$finalFreeLiving) # the final value of the free living population
      thresholdInvalid = c(thresholdInvalid, sum(nearThreshold)) # number of reps in which threshold is invalid
    }
    else{ # if we can't find the file, substitute a NA 
      nrHosts = length(hostData$finalFreeLiving) # lazily use the previous run to check how long finalFreeLiving should be
      propElim = c(propElim, NA) # proportion eliminated under these settings
      thresholdInvalid = c(thresholdInvalid, NA)
      # just ignore finalFreeLiving rather than inserting NAs; it'll be shorter than expected but that's ok
      
    }
  }
  
  names(propElim) = treatmentsToCover
  names(thresholdInvalid) = treatmentsToCover
  
  cat(paste0("Complete after ", (proc.time() - time)[["elapsed"]], " secs.\n"))
  
  return(list(propElim=propElim, finalFreeLiving=finalFreeLiving, thresholdInvalid=thresholdInvalid))
}






# Get summarised data across parameter sets and treatments on what proportion of repeats have led to elimination,
# as well as data on finalFreeLiving so we can check whether all reps have reached equilibrium
# If a file doesn't exist, substitute a NA for that result.
# This function will break if the first run specified doesn't exist (even if subsequent ones do), and do something unspecified
# (probably bad) if the runs specified don't all have the same number of hosts.
getEliminated_MultipleParamSets = function(dataLocation, paramsetsToCover, treatmentsToCover, eliminationThreshold, thresholdInvalidBounds){
  
  time = proc.time()
  
  propElim = data.frame()
  finalFreeLiving = c()
  thresholdInvalid = c()
  for (par in paramsetsToCover){
    
    prop = c()
    thresh = c()
    for (trt in treatmentsToCover){
      
      t = round((proc.time() - time)[["elapsed"]]) # round() rounds to the nearest even number, so down for 0.5 but up for 1.5. Becoming increasingly certain that R has been sent to test me.
      cat(paste0(t, " secs: processing ", par, ".", trt, ".results.RData\n"))
      
      
      f = paste0(dataLocation, par, ".", trt, ".results.RData")
      if (file.exists(f)){
        load(f)
        hostData = extractHostData(foreachResults) # Pull out host data; worm burdens, ages and final value of free living population
        eliminated = getEliminated(hostData$finalFreeLiving, eliminationThreshold) # Logical "this rep eliminated/not" vector
        # Is finalFreeLiving above zero but not much larger than the elimination threshold in any reps? Indicates simulation not settled after treatment.
        # More than a very small fraction of these likely to skew results.
        nearThreshold = (hostData$finalFreeLiving > thresholdInvalidBounds[1])  &  (hostData$finalFreeLiving < thresholdInvalidBounds[2])
        
        prop = c(prop, (sum(eliminated) / length(eliminated))) # proportion eliminated under these settings
        finalFreeLiving = c(finalFreeLiving, hostData$finalFreeLiving) # the final value of the free living population
        thresh = c(thresh, sum(nearThreshold)) # number of reps in which threshold is invalid
      }
      else{ # if we can't find the file, substitute a NA 
        nrHosts = length(hostData$finalFreeLiving) # lazily use the previous run to check how long finalFreeLiving should be
        prop = c(prop, NA) # proportion eliminated under these settings
        thresh = c(thresholdInvalid, NA)
        # just ignore finalFreeLiving rather than inserting NAs; it'll be shorter than expected but that's ok
        
      }
    }
    
    propElim = rbind(propElim, par=prop)
    thresholdInvalid = rbind(thresholdInvalid, par=thresh)
  }
  
  row.names(propElim) = paramsetsToCover
  colnames(propElim) = treatmentsToCover
  row.names(thresholdInvalid) = paramsetsToCover
  colnames(thresholdInvalid) = treatmentsToCover
  
  
  cat(paste0("Complete after ", (proc.time() - time)[["elapsed"]], " secs.\n"))
  
  return(list(propElim=propElim, finalFreeLiving=finalFreeLiving, thresholdInvalid=thresholdInvalid))
}


# Get population mean worm burden over time for each rep
getMeanWormsOverTime = function(wot) { apply(wot, c(1,2), mean, na.rm=TRUE) }  # apply the mean across the third dimension (i.e margin=c(1,2)), ignoring NAs


# Get mean of population mean worm burdens over time, i.e. over *all* reps
getGrandMeanWormsOverTime = function(wot) { apply(wot, 2, mean, na.rm=TRUE) }  # apply the mean across the first and third dimension (i.e margin=2), ignoring NAs



# Get the host data, as above, but split into age categories
getWormsOverTime_AgeSplit = function(hd, breaks)
{
  
  breaks = as.numeric(breaks) # without this R will quite happily let you use a string in place of a number, with undetermined results. Daft.
  
  # Split eliminated into age categories. Returns NA where that host is not in the specified age group.
  wot_as = list()
  for (b in 1:(length(breaks)-1)){
    keep = hd$ages > breaks[b] & hd$ages < breaks[b+1]
    w = hd$wormsOverTime
    w[!keep] = NA
    
    wot_as[[b]] = w
    names(wot_as)[b] <- paste0("ages", breaks[b], "_", breaks[b+1])
  }
  
  return(wot_as)
}




# Ditto but for female worms
getFemaleWormsOverTime_AgeSplit = function(hd, breaks)
{
  breaks = as.numeric(breaks) # without this R will quite happily let you use a string in place of a number, with undetermined results. Daft.
  
  # Split eliminated into age categories. Returns NA where that host is not in the specified age group.
  wot_as = list()
  for (b in 1:(length(breaks)-1)){
    keep = hd$ages > breaks[b] & hd$ages < breaks[b+1]
    w = hd$femaleWormsOverTime
    w[!keep] = NA
    
    wot_as[[b]] = w
    names(wot_as)[b] <- paste0("ages", breaks[b], "_", breaks[b+1])
  }
  
  return(wot_as)
}




# Get mean population worm burden over time for each rep, as above, but split into age categories
# Pass in data from getWormsOverTime_AgeSplit(), similar to how the normal (non-age-split) mean worm functions work
getMeanWormsOverTime_AgeSplit = function(wot_as){
  
  for (i in 1:length(wot_as)){
    wot_as[[i]] = apply(wot_as[[i]], c(1,2), mean, na.rm=TRUE) # apply the mean across the third dimension (i.e margin=c(1,2)), ignoring NAs
  }
  return(wot_as)
}




# Get the grand mean, split into age categories
# Can't use the getMeanWormsOverTime_AgeSplit() function here because you might get varying numbers of individuals in the different
# categories over time. So to get the grand mean for each age category, get the worm burden per individual across all the reps
# (at each timepoint), then calculate the mean of that (at each timepoint).
# I.e. **this treats all the individuals within all the reps as one big population**.
getGrandMeanWormsOverTime_AgeSplit = function(hd, breaks){
  
  breaks = as.numeric(breaks) # without this R will quite happily let you use a string in place of a number, with undetermined results. Daft.
  
  # Split eliminated into age categories
  gmwot_as = list()
  for (b in 1:(length(breaks)-1)){
    keep = hd$ages > breaks[b] & hd$ages < breaks[b+1]
    w = hd$wormsOverTime
    w[!keep] = NA
    
    gmwot_as[[b]] = apply(w, 2, mean, na.rm=TRUE) # apply the mean across the first and third dimension (i.e margin=2), ignoring NAs
    names(gmwot_as)[b] <- paste0("ages", breaks[b], "_", breaks[b+1])
  }
  
  return(gmwot_as)
}




# Return prevalence data over time for all reps
getPrevalences = function(wormsOverTime, prevalenceThreshold){
  prevalences = (apply(wormsOverTime > prevalenceThreshold, MARGIN=c(1, 2), sum) / dim(wormsOverTime)[3])
  return(prevalences)
}




# Take timecourse data and split it into reps that eventually eliminate vs don't eliminate.
# Should in theory work on any of my data structures that are in the same format as hostData$wormsOverTime.
splitByElim = function(timecourseData, finalFreeLiving, eliminationThreshold, thresholdInvalidBounds){  
  elim = getEliminated(finalFreeLiving, eliminationThreshold, thresholdInvalidBounds)
  thresholdInvalid = sum( (finalFreeLiving > thresholdInvalidBounds[1])  &  (finalFreeLiving < (thresholdInvalidBounds[2])) )
  print(paste0("Nr of reps with ambiguous final states: ", thresholdInvalid))
  
  if (is.null(dim(timecourseData)) ) {  # it's probably a vector
    l = list(elim=timecourseData[elim], not_elim=timecourseData[!elim])
  }
  else{  # it's probably multidimensional
    l = list(elim=timecourseData[elim,], not_elim=timecourseData[!elim,])
  }
  
  return(l)
}




# Produce histogram data for one or more sets of input data (all with the same bins though).
# Then write out as a table for export into Excel.
produceHistograms = function(data, lower, upper, size){  # assumes you're inputting data with dim = <observations>, <different histograms>
  
  bins = seq(lower, upper, size)  # values falling on lower bound are included if include.lowest=TRUE (is by default)
  mids = seq(lower+size/2, upper-size/2, size)
  
  print("Using bins: ")
  print(bins)
  
  all_counts = c()
  for(i in 1:dim(data)[2]){
    h = hist(data[,i], bins)
    all_counts = rbind(all_counts, h$counts)
  }
  
  rownames(all_counts) = colnames(data)
  
  print("Mids: ")
  write.table(t(mids),sep="\t",col.names=FALSE,row.names=FALSE)
  
  print("Histogram data: ")
  write.table(all_counts,sep="\t",col.names=FALSE,row.names=TRUE)
}




# As above but output as data structure instead of printing
produceHistograms2 = function(data, lower, upper, size){  # assumes you're inputting data with dim = <observations>, <different histograms>
  
  bins = seq(lower, upper, size)  # values falling on lower bound are included if include.lowest=TRUE (is by default)
  mids = seq(lower+size/2, upper-size/2, size)
  
  #print("Using bins: ")
  #print(bins)
  
  all_counts = c()
  for(i in 1:dim(data)[2]){
    h = hist(data[,i], bins)
    all_counts = rbind(all_counts, h$counts)
  }
  
  rownames(all_counts) = colnames(data)
  
  return(list(mids=mids, counts=all_counts))
  
}









# Get mean population worm burden over time for each rep, as above, but split into age categories
#getMeanWormsOverTime_AgeSplit = function(hd, breaks){
#  
#  breaks = as.numeric(breaks) # without this R will quite happily let you use a string in place of a number, with undetermined results. Daft.
#  
#  # Split eliminated into age categories
#  mwot_as = list()
#  for (b in 1:(length(breaks)-1)){
#    keep = hd$ages > breaks[b] & hd$ages < breaks[b+1]
#    w = hd$wormsOverTime
#    w[!keep] = NA
#    
#    mwot_as[[b]] = apply(w, c(1,2), mean, na.rm=TRUE) # apply the mean across the third dimension (i.e margin=c(1,2)), ignoring NAs
#    names(mwot_as)[b] <- paste0("split", breaks[b], "_", breaks[b+1])
#  }
#  
#  return(mwot_as)
#}


