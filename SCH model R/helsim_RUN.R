# helsim01 - R version of the individual-based model in Medley 1989 thesis, and Anderson&Medley 1985
# Original version Graham Medley, July 2013

## Results format. 
## Results format is a list containing the parameters ($params) and results from individual realisations (list elements 1,2,3...)

## The results from each individual realization is a list. 
## It's elements are:
## lists for each time point specified by the outTimings parameter. 
## $repNo - the realization number (don't have a use for this at the moment). 

## The list for each time point in each realization contains: 
## a data.frame of total and female worms at that time for each host.  


###  to run on own machine (not cluster) change debugging=TRUE + change dataInputPath 


## Specify a single realisation/rep as a function

doRealization <- function(repNo,params, debugging)
{
  
  tryCatch({
    
    
    # Set up variables to keep track of for debugging/runtime profiling
    #if (debugging==TRUE){
    #  repTotalTime = proc.time()
    #  repSetupTime = proc.time()
    #  RepTime = proc.time()
    #  calcRatesTotalTime = 0
    #  sumRatesTotalTime = 0
    #  calcTstepTotalTime = 0
    #  doEventTotalTime = 0
    #  preDetEventTotalTime = 0
    #}
    
    
    # Setup the simulation
    simData <- setupSD ( params )
    time <- 0
    FLlast <- time
    outTimes <- params$outTimings
    
    nextOutIndex <- which.min(outTimes)
    nextOutTime <- outTimes[nextOutIndex]
    ageingInt <- 1/52     # Check ages every week
    #ageingInt <- 1/4     # Check ages every quarter
    currentchemoTiming1<-params$chemoTimings1
    currentchemoTiming2<-params$chemoTimings2
    currentVaccineTimings<-params$VaccineTimings
    nextAgeTime <- ageingInt
    nextChemoIndex1 <- which.min(currentchemoTiming1)
    nextChemoIndex2 <- which.min(currentchemoTiming2)
    nextChemoTime1 <- currentchemoTiming1[nextChemoIndex1]
    nextChemoTime2 <- currentchemoTiming2[nextChemoIndex2]
    nextVaccineIndex <- which.min(currentVaccineTimings)     ##KK
    nextVaccineTime <- currentVaccineTimings[nextVaccineIndex]##KK
    nextStep <- min ( nextOutTime, time+maxStep, nextChemoTime1, nextChemoTime2, nextAgeTime , nextVaccineTime) ##KK added nextVaccineTime
    #nextStep <- min ( nextOutTime, time+maxStep, nextChemoTime1, nextChemoTime2, nextAgeTime)
    elimTime <- NA ## default elimination time. 
    
    results <- list() ## 
    
    outCount <- 1
    
    #if (debugging==TRUE)  repSetupTime = (proc.time() - repSetupTime)[["elapsed"]]
    
    while ( (time<params$maxTime) ) ## && (outCount<=length(outTimes)) there are other recording events besides the out events (e.g. elimTimes).   
    {
      #cat(file=logFile,"Current time: ",time,"\n",append=TRUE) #KK debug
      #if (debugging==TRUE)  currentTime = proc.time()
      #rates <- calcRates ( params, simData, time )  #JT 	calcRates for no vaccine
      rates <- calcRates2 ( params, simData, time )  #JT 	calcRates2 for vaccine
      #if (debugging==TRUE)  calcRatesTotalTime = calcRatesTotalTime + (proc.time() - currentTime)[["elapsed"]]
      
      #if(is.na(sum(rates)))  ### DEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUGDEBUG
      #{
      #	cat(file=logFile,"Error!: ",time,"\n")
      
      #	cat(file=logFile,"Rates: ",rates,"\n")
      #		cat(file=logFile,"Ages: ",simData$contactAgeGroupIndices,"\n")
      #	}
      
      #if (debugging==TRUE)  currentTime = proc.time()
      sumRates <- sum(rates)
      #if (debugging==TRUE)  sumRatesTotalTime = sumRatesTotalTime + (proc.time() - currentTime)[["elapsed"]]
      
      ## if(sumRates < 1) elimTime <- time  ## next worm event is about a year away -> elimination? Didn't work. 
      #if((sum(simData$worms$total)<0.5) & is.na(elimTime))  # (elimTime < 0)
      #{
      #	elimTime <- time
      #cat(file=logFile,"Total worms: ",simData$worms$total,"\n") 
      #cat(file=logFile,"elimTime: ",elimTime,"\n")
      #}
      
      
      # If the rate is such that nothing's likely to happen in the next 10,000 years, just fix the next timestep to 10,000.
      # This is to avoid occasional situations where the rate gets tiny (>1e-300) with the result that R can't calculate the
      # correct values (due to data type limits) and returns NaN.
      if (sumRates < 1e-4){
        tstep = 10000
      }
      # Otherwise sample the next timestep from the exponential distribution
      else{
        tstep = rexp(1,sumRates)
      }
      
      
      if ((time+tstep)<nextStep) 
      {
        time <- time+tstep
        #if (debugging==TRUE)  currentTime = proc.time()
        #simData <- doEvent ( rates, params, simData, time ) #JT no vaccine
        simData <- doEvent2 ( rates, params, simData, time ) #JT vaccine
        #if (debugging==TRUE)  doEventTotalTime = doEventTotalTime + (proc.time() - currentTime)[["elapsed"]]
        #cat(file=logFile,",")
      } 
      else 
      { 
        #if (debugging==TRUE)  currentTime = proc.time()
        
        ## Predetermined event block.
        simData <- doFreeLive ( params, simData, nextStep-FLlast )
        FLlast <- nextStep
        timeBarrier <- nextStep
        
        #timeBarrier <- time+tstep 	## This should probably be nextStep + 0.001. Otherwise, when rates are low, time+tstep could get very big...
        #timeBarrier <- nextStep + 0.001  -> doing a rates check now instead
        
        # Ageing and death 
        if(timeBarrier>=nextAgeTime)
        { 
          simData <- doDeath (params, simData, time) 
          nextAgeTime <- nextAgeTime + ageingInt
        }
        
        # Chemotherapy - now split into ChemoTime1 and ChemoTime2 to allow for two separate treatment programmes. Note I don't know what happens if two treatments are simultaneous.
        # Better not do that then.
        if (timeBarrier>=nextChemoTime1) 
        {
          simData <- doDeath ( params, simData, time ) # update age groups/deaths just before chemo
          simData <- doChemo ( params, simData, time, params$coverage1 )
          currentchemoTiming1[nextChemoIndex1] <- params$maxTime+10 # The +10 is to make sure chemo isn't been done at maxTime
          nextChemoIndex1 <- which.min(currentchemoTiming1)
          nextChemoTime1 <- currentchemoTiming1[nextChemoIndex1]
          #cat(file=logFile,time,"\n",append=TRUE,sep="\t")
          
        }
        if (timeBarrier>=nextChemoTime2) 
        {
          simData <- doDeath ( params, simData, time ) # update age groups/deaths just before chemo
          simData <- doChemo ( params, simData, time, params$coverage2 )
          currentchemoTiming2[nextChemoIndex2] <- params$maxTime+10 # The +10 is to make sure chemo isn't been done at maxTime
          nextChemoIndex2 <- which.min(currentchemoTiming2)
          nextChemoTime2 <- currentchemoTiming2[nextChemoIndex2]
        }
        ##KK
        #			if(timeBarrier>=nextVaccineTime)
        #			{
        #			  simData <- doDeath ( params, simData, time ) # update age groups/deaths just before vaccine KK
        #			  simData <- doVaccine ( params, simData, time, params$VaccCoverage )#debug KK
        #			  currentVaccineTimings[nextVaccineIndex] <- params$maxTime+10 # The +10 is to make sure chemo isn't been done at maxTime
        #			  nextVaccineIndex <- which.min(currentVaccineTimings)
        #			  nextVaccineTime <- currentVaccineTimings[nextVaccineIndex]
        #			}
        # Record
        if (timeBarrier>=nextOutTime )  
        { 	
          currentRes <- list()
          currentRes$worms <- simData$worms
          currentRes$hosts <- simData$demography
          currentRes$vaccState <- simData$sv ##KK
          currentRes$freeLiving <- simData$freeLiving
          currentRes$time <- time
          currentRes$adherenceFactors = simData$adherenceFactors
          currentRes$compliers = simData$compliers
          # save sv array JT for output
          results[[outCount]] <- currentRes
          
          outCount <- outCount+1
          
          outTimes[nextOutIndex] <- params$maxTime+10
          nextOutIndex <- which.min(outTimes)
          nextOutTime <- outTimes[nextOutIndex]
          #cat(file=logFile, ".\n")
          #cat("\n	RECORD") # Flag for testing
        }		
        time <- nextStep
        nextStep <- min ( nextOutTime, time+maxStep, nextChemoTime1, nextChemoTime2, nextVaccineTime, nextAgeTime )
        #nextStep <- min ( nextOutTime, time+maxStep, nextChemoTime1, nextChemoTime2, nextAgeTime )
        #if (debugging==TRUE)  preDetEventTotalTime = preDetEventTotalTime + (proc.time() - currentTime)[["elapsed"]]
      } ## end of predetermined event block.
    } ## end of while loop. 
    #cat(file=logFile,"Called doVaccine: time = ",time,"\n",append=TRUE)
    
    finalResults = list(results=results, repNo=repNo, elimTime=elimTime, attendanceRecord=unname(simData$attendanceRecord), ageAtChemo=simData$ageAtChemo, adherenceFactorAtChemo=simData$adherenceFactorAtChemo, params=params)
    
    #if (debugging==TRUE){
    #  results$repTotalTime = (proc.time() - repTotalTime)[["elapsed"]]
    #  results$repSetupTime = repSetupTime
    #  results$calcRatesTotalTime = calcRatesTotalTime
    #  results$calcTstepTotalTime = calcTstepTotalTime
    #  results$sumRatesTotalTime = sumRatesTotalTime
    #  results$doEventTotalTime = doEventTotalTime
    #  results$preDetEventTotalTime = preDetEventTotalTime
    #}
    
    cat(file=logFile,"Rep ",repNo," finished.\n",append=TRUE)
    
    return(finalResults)
    
  },
  # if there are any warnings or errors, this will write them to 'outfile="DoParallel_logging.txt"'. Normally DoParallel suppresses them. 
  warning = function(war){print(paste0("WARNING: ", war, " at ", Sys.time()))},  
  error = function(err){print(paste0("ERROR: ", err, " at ", Sys.time()))})
}
