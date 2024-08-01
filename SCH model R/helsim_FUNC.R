# Function file for helsim_JY_RUN.R

## Get parameter value for the name param in the given file. 
## Format of the file:
## paramName (tab) value1 value2 value3...valueN (tab) Comments. 
## returns a vector of strings to be converted if necessary. 

## JT = changes made to add in vaccination: v1sigma, v2lambda, v3beta, vaccEff, vaccCoverage, vaccTreatmentBreaks

readParam <- function(paramName,fullFilePath)
{	
	con <- file(fullFilePath,open="r")
	
	value <- NA
	found = FALSE
	while(length(line <- readLines(con, n = 1, warn = FALSE)) > 0 & !found)
	{
		qq <- strsplit(line,"\t")
		tokens <- qq[[1]]
		if(length(tokens)>0)
		{
			if(tokens[1]==paramName)
			{
				#value <- as.numeric(tokens[2])
				value <- strsplit(tokens[2]," ")
				found=TRUE
			}
		}
	}
	
	close(con)
	if(!found) return(NA) 
	return(value[[1]])
}

readParams <- function (fileName,demogName="Default") 
{
  repNum <- as.integer(readParam("repNum",fileName)) # Number of repetitions
  maxTime <- as.numeric(readParam("nYears",fileName)) # Maximum number of years to run
	# THE STUDY SIM SHOULD IGNORE nYearsPostTreat AS IT MIGHT RESULT IN DIFFERENT NR YEARS FOR DIFFERENT INPUTS (EG NR TREATMENTs)
  #nYearsPostTreat <- as.numeric(readParam("nYearsPostTreat",fileName)) # The number of years to continue running after treatment (will override nYears if specified)
	N <- as.integer(readParam("nHosts",fileName)) # Host population size
	R0 <- as.numeric(readParam("R0",fileName)) # Basic reproductive number
	lambda <- as.numeric(readParam("lambda",fileName)) # Eggs per gram
	v2 <- as.numeric(readParam("v2lambda",fileName)) # impact of vaccine on eggs per gram  JT Fraction of eggs produced when vaccinated. JET
	gamma <- as.numeric(readParam("gamma",fileName)) # Exponential density dependence of parasite adult stage
	k <-as.numeric(readParam("k",fileName)) # Shape parameter of assumed negative binomial distribution of worms amongst host
	sigma <- as.numeric(readParam("sigma",fileName)) # Worm death rate
	v1 <- as.numeric(readParam("v1sigma",fileName)) # impact of vaccine on worm death rate JT. Assume worm death rate is v1*sigma. JET
	LDecayRate <- as.numeric(readParam("ReservoirDecayRate",fileName)) # Decay rate of the infectious material in the environment (reservoir decay rate)
	ContactAGB <- as.numeric(readParam("contactAgeBreaks",fileName)) # Contact age group breaks
	beta <- as.numeric(readParam("betaValues",fileName)) # Contact rates
	v3 <- as.numeric(readParam("v3betaValues",fileName)) # impact of vaccine on contact rates  JT. Assume contact rate under vaccination is times v3. JET 
	rho <- as.numeric(readParam("rhoValues",fileName))	# rho, contribution values. 
	treatmentBreaks <- as.numeric(readParam("treatmentBreaks",fileName)) # Minimum ages of the treatment groups
	VaccTreatmentBreaks <- as.numeric(readParam("VaccTreatmentBreaks",fileName)) # Minimum ages of the treatment groups JT
	coverage <- as.numeric(readParam("coverage",fileName)) # Coverage of the age groups
	coverage1 <- as.numeric(readParam("coverage1",fileName)) # Coverage of the age groups
	coverage2 <- as.numeric(readParam("coverage2",fileName)) # Coverage of the age groups
	VaccCoverage <- as.numeric(readParam("VaccCoverage",fileName)) # Vaccine coverage of the age groups JT
	DrugEfficacy <- as.numeric(readParam("drugEff",fileName)) # Efficacy of drugs
	#VaccEfficacy <- as.numeric(readParam("vaccEff",fileName)) # Efficacy of vaccine  JT
	#chemoTimings <- as.numeric(readParam("treatFreq",fileName)) # Timings of the treatment
	outTimings <- as.numeric(readParam("outputEvents",fileName)) # times for survey-style outputs. 
	outputFrequency <- as.numeric(readParam("outputFrequency",fileName))
	outputOffset <- as.numeric(readParam("outputOffset",fileName))
	adherenceSetting <- readParam("adherenceSetting",fileName)
	propNeverCompliers <- as.numeric(readParam("propNeverCompliers",fileName))
	highBurdenBreaks <- as.numeric(readParam("highBurdenBreaks",fileName))
	highBurdenValues <- as.numeric(readParam("highBurdenValues",fileName))
	#add number of vaccinated states, default=2 JT. Not sure this is necessary, actually. JET. 
	vaccDecayRate <- as.numeric(readParam("vaccDecayRate",fileName)) # vacc decay rate. rate of vaccine decay = 1/duration of vaccine  JT A vector with value 0 in state 1 and the vacc decay rate for state 2. JET. 
	
	## sort out times for treatments. 
	#treatStart <- as.numeric(readParam("treatStart",fileName))
	nRounds <- as.numeric(readParam("nRounds",fileName)) 
	treatInterval <- as.numeric(readParam("treatInterval",fileName)) 
	#chemoTimings <- seq(from=treatStart,by=treatInterval,length.out=nRounds)
	## sort out times for chemo1 treatments. ###KK
	treatStart1 <- as.numeric(readParam("treatStart1",fileName))
	nRounds1 <- as.numeric(readParam("nRounds1",fileName)) 
	treatInterval1 <- as.numeric(readParam("treatInterval1",fileName)) 
	chemoTimings1 <- seq(from=treatStart1,by=treatInterval1,length.out=nRounds1)
	## sort out times for chemo2 treatments. ###KK
	treatStart2 <- as.numeric(readParam("treatStart2",fileName))
	nRounds2 <- as.numeric(readParam("nRounds2",fileName)) 
	treatInterval2 <- as.numeric(readParam("treatInterval2",fileName)) 
	chemoTimings2 <- seq(from=treatStart2,by=treatInterval2,length.out=nRounds2)
	## sort out times for vaccine. ###########KK
	VaccTreatStart <- as.numeric(readParam("VaccTreatStart",fileName))
	nRoundsVacc <- as.numeric(readParam("nRoundsVacc",fileName)) 
	treatIntervalVacc <- as.numeric(readParam("treatIntervalVacc",fileName)) 
	VaccineTimings <- seq(from=VaccTreatStart,by=treatIntervalVacc,length.out=nRoundsVacc)
	
	pars <- list (	repNum=repNum, 
					maxTime=maxTime, 
					#nYearsPostTreat = nYearsPostTreat,
					N=N,
					R0=R0,
					lambda=lambda,
					v2=v2, #JT
					gamma=gamma,
					k=k,
					sigma=sigma,
					v1=v1, #JT
					LDecayRate=LDecayRate,
					DrugEfficacy=DrugEfficacy,
					#VaccEfficacy=VaccEfficacy, #JT 
					#chemoTimings=chemoTimings,
					contactAgeBreaks=ContactAGB,
					nRounds=nRounds,
					contactRates=beta,
					v3=v3, #JT
					rho=rho,
					treatmentBreaks=treatmentBreaks,
					VaccTreatmentBreaks=VaccTreatmentBreaks,   #JT
					coverage=coverage,
					coverage1=coverage1,
					coverage2=coverage2,
					VaccCoverage=VaccCoverage, #JT
					vaccDecayRate=vaccDecayRate, #JET
					treatInterval=treatInterval,
					#treatStart=treatStart,
					VaccTreatStart=VaccTreatStart,#KK
					nRoundsVacc=nRoundsVacc, #KK
					treatIntervalVacc=treatIntervalVacc, #KK
					treatInterval1=treatInterval1,##KK
					treatInterval2=treatInterval2, ##KK
					treatStart1=treatStart1, ##KK
					treatStart2=treatStart2, ##KK
					nRounds1=nRounds1, ##KK
					nRounds2=nRounds2, ##KK
					chemoTimings1=chemoTimings1, ##KK
					chemoTimings2=chemoTimings2, ##KK
					VaccineTimings=VaccineTimings, #KK
					outTimings=outTimings,
					outputFrequency=outputFrequency,
					outputOffset=outputOffset,
					adherenceSetting=adherenceSetting,
					propNeverCompliers=propNeverCompliers,
					highBurdenBreaks=highBurdenBreaks,
					highBurdenValues=highBurdenValues)

	##################################################################################################
	## construct the path for the demography file from the param file path. 
	g <- gregexpr("[/\\]",fileName)  ## Using regular expressions to capture the position of slashes in both directions. 
	stub <- substr(fileName,1,max(g[[1]]))
	demogPath <- paste0(stub,"Demographies.txt")
	
	## record the demography used. 
	pars$demogType <- demogName 
	
	## construct the parameter names...
	muName <- paste0(demogName,"_hostMuData")
	boundsName <- paste0(demogName,"_upperBoundData")
			
	#### host survival curve.
	## Read in the data. 
	pars$hostMuData <- as.numeric(readParam(muName,demogPath)) 
	pars$muBreaks <- c(0,as.numeric(readParam(boundsName,demogPath)))
	
	## turn SR on or off. 
	pars$SR <- TRUE
	if(readParam("StochSR",fileName)=="FALSE")
	{
		pars$SR <- FALSE
	}
	
	## read in function name for deterministic repro function. Needed for calculation of equilibrium. 
	pars$reproFuncName <- readParam("reproFuncName",fileName)
	
	# Parasite survival curve stuff
	pars$psi <- 1.0 # Dummy value prior to R0 calculation
		
	return(pars)
}




# For batches of runs with varying parameters, provide a parameter overrides file and use paramOverrideNr to
# specify which row of parameters to use. Any parameters named will be overwritten with those values
# - or added to the params if they' didn't already exist.
# TODO: this is messy - given we might also need to do similar preprocessing on other parameters a better overall solution
#       would be to have a separate R file that takes the basic parameter .txt and the overrides and writes new a .txt file
#       for each override. then the main run files don't even have to know about the parameter variations, and there's no need
#       to update this function in parallel, plus it'd be easy to rerun individual sets of parameters.
# overrideParams = function(params, overridesFile, paramOverrideNr)
# {
# 	load(overridesFile)
# 
#   # For each element of the specified row, read its name and insert/override the specified parameter with that value
#   n = names(paramOverrides[1,])
# 	for(i in 1:length(paramOverrides[1,])){
# 		params[n[i]] = paramOverrides[paramOverrideNr,i]
# 	}
# 	
#   # Update some other parameters indirectly
# 	params$contactRates = c(paramOverrides[[paramOverrideNr,"Beta Infant"]], paramOverrides[[paramOverrideNr,"Beta Pre-SAC"]], 1, paramOverrides[[paramOverrideNr,"Beta Adult"]])
# 	params$rho = params$contactRates#KK do we need any v3 here?no
# 	params$z <- exp(-params$gamma)
# 
# 	# Coverage 1 and 2 etc are for two different treatment programmes (probably non-overlapping, not sure what happens if two treatments occur at the same time)
# 	params$coverage1 = c(paramOverrides[[paramOverrideNr,"InfantCoverage1"]], paramOverrides[[paramOverrideNr,"PreSacCoverage1"]],paramOverrides[[paramOverrideNr,"SacCoverage1"]], paramOverrides[[paramOverrideNr,"AdultCoverage1"]])
# 	params$coverage2 = c(paramOverrides[[paramOverrideNr,"InfantCoverage2"]], paramOverrides[[paramOverrideNr,"PreSacCoverage2"]],paramOverrides[[paramOverrideNr,"SacCoverage2"]], paramOverrides[[paramOverrideNr,"AdultCoverage2"]])	
# 	params$treatStart1 = paramOverrides[[paramOverrideNr,"treatStart1"]]
# 	params$treatStart2 = paramOverrides[[paramOverrideNr,"treatStart2"]]
# 	params$nRounds1 = paramOverrides[[paramOverrideNr,"nRounds1"]]
# 	params$nRounds2 = paramOverrides[[paramOverrideNr,"nRounds2"]]
# 	params$treatInterval1 = paramOverrides[[paramOverrideNr,"treatInterval1"]]
# 	params$treatInterval2 = paramOverrides[[paramOverrideNr,"treatInterval2"]]
# 	params$N = paramOverrides[[paramOverrideNr,"N"]]
# 	
# 	# Just to make sure we don't use the usual ones by accident
# 	params$coverage = NULL
# 	params$treatStart = NULL
# 	params$nRounds = NULL
# 	params$treatInterval = NULL
# 	params$VaccCoverage =NULL  ##KK
# 	return(params)
# }








	##### BREAK HERE!!!

configure <- function(pars)
{
  
  ## this was in the read in function...
  pars$z <- exp(-pars$gamma) # Fecundity parameter (z)
  
  ## look up the name of the reproduction function. 
  pars$reproFunc <- match.fun(params$reproFuncName) ## carry the reproduction function with the parameters. 
  
	dT <- 0.1 ## level of descretization for the drawing of lifespans. 
	pars$maxHostAge <- min(max(pars$muBreaks),max(pars$contactAgeBreaks))
	
	#pars$muAges <- seq(0,pars$maxHostAge-dT, by=dT)+0.5*dT
	pars$muAges <- seq(0,max(pars$muBreaks)-dT, by=dT) + 0.5*dT   ### the whole range of ages. Concatenate later. 
	muIndices <- cut(pars$muAges,breaks=pars$muBreaks,labels=1:length(pars$hostMuData)) 
	pars$hostMu <- pars$hostMuData[muIndices] 
	
	## first element gives probability of surviving to end of first year. 
	pars$hostSurvivalCurve <- exp(-cumsum(pars$hostMu)*dT)
	
	## the index for the last age group before the cutoff in this descretization. 
	maxAgeIndex <- which(pars$muAges > pars$maxHostAge)[1] - 1
	
	## the cumulative probability of dying in the ith year.  
	fullHostAgeCumulDistr <- cumsum(dT*pars$hostMu*c(1,pars$hostSurvivalCurve[1:(length(pars$hostSurvivalCurve)-1)]))  ## prob of dying in each age group. 
	#pars$hostAgeCumulDistr <- cumsum(dT*pars$hostMu*c(1,hostSurvivalCurve[1:(length(hostSurvivalCurve)-1)]))
	pars$hostAgeCumulDistr <- c(fullHostAgeCumulDistr[1:(maxAgeIndex-1)],1) ## cumulative probability with cutoff at max age. 	

	# Contact stuff
	pars$contactAgeGroupBreaks <- c(pars$contactAgeBreaks[1:(length(pars$contactAgeBreaks)-1)], pars$maxHostAge)  ##  + dT used to have this to avoid going outside bounds of breaks. Not necessary, I think.    
	
	# Treatment stuff
	pars$treatmentAgeGroupBreaks <- c(pars$treatmentBreaks[1:(length(pars$treatmentBreaks)-1)], pars$maxHostAge + dT) 
	
	# Vaccination stuff. 
	## Breaks in the param file are the lower bounds of ranges with width 1. JET 
	## Now we construct the real breaks from this. 
	constructedVaccBreaks <- sort( c(pars$VaccTreatmentBreaks,pars$VaccTreatmentBreaks+1) )  
	pars$VaccTreatmentAgeGroupBreaks <- c(-dT, constructedVaccBreaks, pars$maxHostAge + dT) 
	#pars$VaccTreatmentAgeGroupBreaks <- c(-dT, pars$VaccTreatmentBreaks, pars$maxHostAge + dT) #
	#pars$VaccTreatmentAgeGroupBreaks <- c(-dT, pars$VaccTreatmentBreaks[1:(length(pars$VaccTreatmentBreaks)-1)], pars$maxHostAge + dT)

	
	## Timings stuff
	#if (!is.na(pars$nYearsPostTreat)){ # if nYearsPostTreat is specified, update maxTime to the year required
	  #treatmentProgramme1Final = pars$treatStart1 + (pars$nRounds1 * pars$treatInterval1)
	  #treatmentProgramme2Final = pars$treatStart2 + (pars$nRounds2 * pars$treatInterval2)
	  # use the later finish of the two programmes
	  #if (treatmentProgramme1Final > treatmentProgramme1Final){  pars$maxTime = treatmentProgramme1Final + pars$nYearsPostTreat  }
	  #else{  pars$maxTime = treatmentProgramme2Final + pars$nYearsPostTreat  }
	#}
	if(!is.na(pars$outputFrequency)){
		timings = seq(pars$outputOffset, pars$maxTime, 1/pars$outputFrequency) # if outputFrequency is specified, update the output times accordingly
		timings[timings < 0] = 0 # if there are any times smaller than zero (eg from a negative offset) then reset those to zero
		pars$outTimings = timings
		if(pars$outTimings[length(pars$outTimings)] < pars$maxTime) pars$outTimings = c(pars$outTimings, pars$maxTime) # always output at the end of simulation ##KK we put this inside if loop and made it less. It adds the nYears in the end
	}  
	#if(pars$outTimings[length(pars$outTimings)] != pars$maxTime) pars$outTimings = c(pars$outTimings, pars$maxTime) # always output at the end of simulation
	
	## if propNeverCompliers is not defined, set to zero. 
	if(is.na(pars$propNeverCompliers))  pars$propNeverCompliers <- 0 
	
	
	## Set up params specific to monogramous reproduction
  if (pars$reproFuncName == "epgMonog")	 pars$monogParams = monogFertilityConfig(pars)
	
  return (pars)
}



# Sets up the simulation to initial conditions based on analytical equilibria
setupSD <-  function ( pars ) 
{
    si <- rgamma(pars$N,scale=1/pars$k,shape=pars$k)
    #sv <- (pars$N)  #JT FIX: 1=unvacc, 2=vacc, VaccCoverage
	sv <- rep(1,pars$N)  ## JET: everyone starts as unvaccinated. #Kk change to 2 to see the decay debug
  
#  	# Pickup function
#	pickUpCDF <- NULL # Dummy
#	if ( pars$pickUp[1]==1 ) 
#	{
#		pickUpCDF <- calcLoCDF ( 500, pars$pickUp[2] ) # Logarithmic distribution
#		# age: we are not going to use this method of generating the negative binmoial , this parameter will always be zero
#	}
	
	# Worm populations - total and female worms for each individual 
	# Worms <- data.frame(total=rep(7,pars$N),female=rep(3,pars$N))
	#wTotal <- rpois(pars$N,lambda=2*11.5*si) # These values chosen as initial conditions (initial conditions should be the equilibrium)
	#worms <- data.frame(total=wTotal,female=rbinom(pars$N,size=wTotal,prob=0.5))
	
	# Freeliving populations: uninfective and infective
	#freeLiving <- 4   # Don't know what this number should be. 
	
	# Distribute birth dates such that at time zero have the right distribution of ages
	# Give a sample of ages at the start of the simulation
	# For an exponential distribution, the survival function S(t)=exp(-t/mu)
	# So to sample from this distribution, generate a random number on 0 1 and then invert this function
	
	lifeSpans <- getLifeSpans(pars$N,pars)
	trialBirthDates <- -lifeSpans*runif(pars$N)
	trialDeathDates <- trialBirthDates + lifeSpans 
	
	## Equilibrate the population over 1000 years, in the absence of understanding how to generate it in the first place. 
	communityBurnIn <- 1000
	while(min(trialDeathDates)<communityBurnIn)
	{
		earlyDeath <- which(trialDeathDates < communityBurnIn)
		trialBirthDates[earlyDeath] <- trialDeathDates[earlyDeath]
		trialDeathDates[earlyDeath] <- trialDeathDates[earlyDeath] + getLifeSpans(length(earlyDeath),pars) 
	}
	
	demography <- data.frame(birthDate=trialBirthDates-communityBurnIn,deathDate=trialDeathDates-communityBurnIn)
  
 	# Contactparam index for each host
	contactAgeGroupIndices <- cut(-demography$birthDate,pars$contactAgeGroupBreaks,1:(length(pars$contactAgeGroupBreaks)-1))
	
	
	### calculate the IC worm burdens here... 
	#indexInB <- as.integer((-demography$birthDate)/calcs$deltaT) + 1    ## lowest index is 1. THE OLD WAY.  
	#equiInfRates <- equiRes*si*calcs$B[indexInB]
	#stableProfile<-rep(pars$k*(pars$R0^(1/(pars$k+1))-1)/(1-exp(-pars$gamma)),pars$N)
	meanBurdenIndex <- cut(-demography$birthDate,breaks=c(0,pars$equiData$ageValues),labels=1:length(pars$equiData$ageValues))
	means <- si*pars$equiData$stableProfile[meanBurdenIndex]*2  ## factor of 2 because the profile is for female worms in the det system. 
  #means <- si*stableProfile*2##DEBUG 
	wTotal <- rpois(pars$N,lambda=means) 
	worms <- data.frame(total=wTotal,female=rbinom(pars$N,size=wTotal,prob=0.5))
	stableFreeLiving <- pars$equiData$L_stable*2  ## 2 because all eggs not just female ones.  
	SD <- list (	ID=1:pars$N, 
	             si=si, 
	             sv=sv,   #JT FIX ABOVE
	             worms=worms, 
	             freeLiving=pars$equiData$L_stable*2, 
	             demography=demography,
	             contactAgeGroupIndices=contactAgeGroupIndices,
	             vaccCount=0)

 	
 	### Adherence stuff
 	
 	# Coverage level index for each host
 	SD$treatmentAgeGroupIndices <- cut(-demography$birthDate,pars$treatmentAgeGroupBreaks,1:(length(pars$treatmentAgeGroupBreaks)-1)) 
 	SD$VaccTreatmentAgeGroupIndices <- cut(-demography$birthDate,pars$VaccTreatmentAgeGroupBreaks,1:(length(pars$VaccTreatmentAgeGroupBreaks)-1)) #JT
 	
 	# Random adherence factors from 0 to 1 (not inclusive). This'll be overwritten at each round if the compliance settings is random
	SD$adherenceFactors = runif(pars$N)
	# Random vaccination factors from 1 to 2 (not inclusive). This'll be overwritten at each round if the vaccine setting is random
	SD$vaccinatedFactors = runif(pars$N,min=1,max=2)  #JT
	
	
	## flag those that are treatment compliers. This state lasts a lifetime. 
	SD$compliers <- !(runif(pars$N) < pars$propNeverCompliers)
	
 	# Right at the start pick all individuals who won't adhere to treatment, based on those adherenceFactors and the coverage for each group.
 	# This'll be overwritten in all compliance settings except systematic. Note that if coverage changes over time that *will not* affect
    # attendance in a systematic setting; it's based on initial coverage. This may need to change in future.
 	#SD$attendance = getAttendance(SD$adherenceFactors, pars$coverage, SD$treatmentAgeGroupIndices)

	# We'll keep track of actual attendance at each round
	SD$attendanceRecord = c()
	SD$ageAtChemo = c()
	SD$adherenceFactorAtChemo = c()
	return(SD)
}




# Calculate probability of attending for each individual, as Plaisier et al 2000; p = adherenceFactors^((1-c)/c),
# where c is the coverage for that age group.
# This doesn't worry about whether adherence is random or systematic, it just returns percentage likelihoods
# for each individual of attending any particular round of treatment. For systematic we'll just do this once.
getAttendance = function(adherenceFactors, coverage, treatmentAgeGroupIndices){
  c = coverage[treatmentAgeGroupIndices]
  p = adherenceFactors^((1-c)/c) 
  attendance = runif(length(p)) < p # get a random number for each host and they attend if that's lower than their probability.
	
  return(attendance)
}


#JT FIX ignore
# JT added function - probability for getting vaccinated for each individual where vc=vacc coverage for that age group
#KK we dont need the following function.
#getVaccinated = function(vaccinatedFactors, VaccCoverage, VaccTreatmentAgeGroupIndices){
#  vc = VaccCoverage[VaccTreatmentAgeGroupIndices]
 # p = vc vaccinatedFactors ############## FIX
 # vaccinated = runif(length(p),min=1,max=2) < p # get a random number for each host and they attend if that's lower than their probability.
 # return(vaccinated)
#}






# Output the simulation
outRes <- function ( f, r, t, SD ) 
{
	cat ( file=f, r, t, mean(SD$worms$female),SD$worms$female,SD$freeLiving, "\n", sep="\t") # Adults only
}

# Output the results from multiple runs
outResMulti <- function (f,SD) 
{
	cat ( file=f, mean(SD$worms$female),"\t") # Adults only
}

# Calculate the event rates 
#Event order: host infection rates, host vacc recovery rates, total worm death rates. JET. 
calcRates <- function ( pars, SD, t=0 ) 
{
	# Only three events: worm death new worms and vacc decay. 
	# Worm total death rate
	#deathRate <- pars$sigma*sum(SD$worms$total)
	deathRate <- pars$sigma*sum(SD$worms$total*pars$v1[SD$sv]) ## Each worm total is X by relative vaccinated death rate. JET
	## Perhaps cumsum(SD$worms$total*pars$v1[SD$sv]) should be stored to prevent unnecessary recalc? JET. 
	
	hostInfRates <- SD$freeLiving*SD$si*pars$contactRates[SD$contactAgeGroupIndices] #KK do we need any v3 here? No we don't

	hostVaccDecayRates <- pars$vaccDecayRate[SD$sv]  ## This should have zeros for unvacc people and decay rate for others. JET. 
	
	rates <- c (hostInfRates,hostVaccDecayRates,deathRate)  #Event order: host infection rates, host vacc recovery rates, total worm death rates. JET. 
	return ( rates )
}

# Calculate the event rates: version 2. 
#Event order: host infection rates, host vacc recovery rates, worm death rates. JET. 
# Each of the these types of events happen to individual hosts. 
calcRates2 <- function ( pars, SD, t=0 ) 
{
	# Worm total death rate
	#deathRate <- pars$sigma*sum(SD$worms$total)
	deathRates <- pars$sigma*SD$worms$total*pars$v1[SD$sv] ## Each worm total is X by relative vaccinated death rate. JET 
	
	hostInfRates <- SD$freeLiving*SD$si*pars$contactRates[SD$contactAgeGroupIndices] #KK do we need any v3 here? No

	hostVaccDecayRates <- pars$vaccDecayRate[SD$sv]  ## This should have zeros for unvacc people and decay rate for others. JET. 
	
	rates <- c (hostInfRates,hostVaccDecayRates,deathRates)  #Event order: host infection rates, host vacc recovery rates, total worm death rates. JET. 
	return ( rates )
}

# Enact an event
doEvent <- function ( rates, pars, SD, t=0 ) 
{
	# Determine which event   # vaccination recovery event to be added - v1-v3 additions? JT
	# If it's 1 to N, it's a new worm otherwise, it's a worm death
	event <- which((runif(1)*sum(rates))<cumsum(rates))[1]
  
	if(event==length(rates))
	{
		# It's a worm death... 
#		deathIndex <- which((runif(1)*sum(SD$worms$total))<cumsum(SD$worms$total))[1]
		deathIndex <- which((runif(1)*sum( SD$worms$total*pars$v1[SD$sv] ))<cumsum( SD$worms$total*pars$v1[SD$sv] ))[1]  ## We've used it twice more here (see previous function) JET. ##KK times relative vaccinated rate. v1-->(1/1-v1)?
		if(runif(1)<SD$worms$female[deathIndex]/SD$worms$total[deathIndex]) 
		    SD$worms$female[deathIndex] <- SD$worms$female[deathIndex] - 1
		SD$worms$total[deathIndex] <- SD$worms$total[deathIndex] - 1
	} 
	
	if(event <= pars$N) ## It's a potential worm acquisition event. 
	{
		if(runif(1) < pars$v3[ SD$sv[event] ]) ## If true, this worm establishes. This is a rejection algorithm approach. JET. 
		{
			# It's a new worm...
			SD$worms$total[event] <- SD$worms$total[event] + 1
			if(runif(1)<0.5) 
				SD$worms$female[event] <- SD$worms$female[event] + 1
		}
	} else if(event <= 2*pars$N) ## it's a vaccine recovery event...
	{
		hostIndex <- event - pars$N
		SD$sv[hostIndex] <- 1   ## set to unvaccinated. If it was already 1, something is wrong. JET. 
	}
	
	return( SD )  
} 

# Enact an event: version 2. 
doEvent2 <- function ( rates, pars, SD, t=0 ) 
{
	# Determine which event  
	# There are three types of event and n possible targets for each type. 
	event <- which((runif(1)*sum(rates))<cumsum(rates))[1]
  
	eventType <- ((event-1) %/% pars$N) + 1  ## 1 = potential host inf, 2 = vacc decay, 3 = worm death. 
	hostIndex <- ((event-1) %% pars$N) + 1
  
	 if(eventType==1) ## potential worm acquisition. #KK comment this out to see vaccination decay###DEBUG DEBUG
	{
	 	if(runif(1) < pars$v3[ SD$sv[hostIndex] ]) ## If true, this worm establishes. This is a rejection algorithm approach. JET. 
		{
	#		# It's a new worm...
			SD$worms$total[hostIndex] <- SD$worms$total[hostIndex] + 1
	 		if(runif(1)<0.5) 
				SD$worms$female[hostIndex] <- SD$worms$female[hostIndex] + 1
	 	}		
	 }

	if(eventType==2) ## vaccination decay event. 
	{
			SD$sv[hostIndex] <- 1   ## set to unvaccinated. If it was already 1, something is wrong. JET. 
			# debugCounter <<- debugCounter + 1
			## output to log time and counter... DEBUG. 
			# cat(file=logFile,t,"\t",debugCounter,"\n",append=TRUE)
			# cat(file=logFile,eventType,hostIndex,"\n",append=TRUE,sep="\t")
			# for(i in 1:500)
			# {
			  # cat(file=logFile,i,rates[i],rates[i+500],rates[i+1000],"\n",sep="\t",append=TRUE)
			# }
			#stop()
	}
	
	if(eventType==3) ## worm death event. 
	{
		# Is it female? 
		if(runif(1)<SD$worms$female[hostIndex]/SD$worms$total[hostIndex]) 
		    SD$worms$female[hostIndex] <- SD$worms$female[hostIndex] - 1
		SD$worms$total[hostIndex] <- SD$worms$total[hostIndex] - 1		
	}
		
	return( SD )  
} 


# Run processes that need to occur regularly, i.e reincarnating whichever hosts have died recently and
# updating the free living worm population
doRegular <- function ( pars, SD, ts )
{
  SD <- doDeath(pars, SD, t)
  SD <-  doFreeLive( pars, SD, ts )
  return (SD)
}


# Update the freeliving populations deterministically
doFreeLive <- function ( pars, SD, ts ) 
{
  
  # Polygamous reproduction - female worms produce fertilised eggs only if there's at least one male worm around  
  if (pars$reproFuncName == "epgFertility"){
    noMales <- SD$worms$total==SD$worms$female
    productivefemaleworms <- SD$worms$female
    if(pars$SR)
    {
      productivefemaleworms[noMales] <- 0		## Commenting out this line would remove sexual reproduction, I think. 
    }    
  }
  
  # Monogamous reproduction - only pairs of worms produce eggs
  else if (pars$reproFuncName == "epgMonog"){
    male = SD$worms$total - SD$worms$female
    productivefemaleworms = pmin(male, SD$worms$female)
    
  }
  
  
  #eggOutputPerHost <- pars$lambda*productivefemaleworms*exp(-productivefemaleworms*pars$gamma)   #will change when vaccinated JT
  #eggOutputPerHost <- pars$lambda*productivefemaleworms*exp(-productivefemaleworms*pars$gamma)*pars$v2[SD$sv]#KK vaccine related fecundity## KK debug to match Ben
  
  eggOutputPerHost <- pars$lambda*productivefemaleworms*exp(-SD$worms$total*pars$gamma)*pars$v2[SD$sv]#KK vaccine related fecundity

  ## Factor of 2 because psi is rate of prod of female eggs. We want total fertilized eggs for total worms. 
  eggsProdRate <- 2*pars$psi*sum(eggOutputPerHost*pars$rho[SD$contactAgeGroupIndices])/pars$N		
  
  ## dL/dt = K - mu*L has soln: L(0)exp(-mu t) + K*(1-exp(-mu t))/mu.  This is exact if rate of egg production is constant in the timestep.
  expo <- exp(-pars$LDecayRate*ts)
  SD$freeLiving <- SD$freeLiving*expo + eggsProdRate*(1-expo)/pars$LDecayRate
  return ( SD )
}



# Note that despite the name, this is run periodically to check whether there have been any deaths but also to update 
# the age group the hosts fall into. 
doDeath<-function(pars, SD, t)  ## , logFile
{
  # Identify the indices of the dead 
  theDead <- which(SD$demography$deathDate < t) 
  if(length(theDead!=0))
  { # note that prior to fix, if there were no dead, this would return to the caller and not update age categories	
    # Set the birth date to now
    SD$demography$birthDate[theDead] <- t - 0.001   ## put the birth slightly in the past to ensure age is just positive for categorization. 
    
    # Calculate death dates
    newDeathDays <- t + getLifeSpans(length(theDead),pars)
    SD$demography$deathDate[theDead] <- newDeathDays
    
    # They also need new force of infections (FOIs)
    SD$si[theDead] <- rgamma(length(theDead),scale=1/pars$k,shape=pars$k) 
    SD$sv[theDead] <- 1 ## JET: Removed only for debugging!!!!
    
    # Kill all their worms 
    SD$worms$total[theDead] <- 0
    SD$worms$female[theDead] <- 0  #1/2 born at 1 for vaccination state JT ##KK Removed for DEBUGGING  
    
    ## Redo attendance
    
    # Regenerate adherenceFactors
    SD$adherenceFactors[theDead] = runif(length(theDead))
    
    ## assign the newly-born to either comply or not. Equally, you could just allow this type of compliance to be inherited/reincarnated. i.e. just comment out this line.  
    SD$compliers[theDead] <- runif(length(theDead)) > pars$propNeverCompliers
    
    # Then redo their attendance. Don't need to distinguish between adherence settings; need this for systematic but if adherence is random  
    # or semi-systematic then adherence factors and attendance will be recalculated at the appropriate times (before chemo) anyway
    #SD$attendance[theDead] = getAttendance(SD$adherenceFactors[theDead], pars$coverage, SD$treatmentAgeGroupIndices[theDead])
    
  }
  
  
  # Redo the contact age categories
  SD$contactAgeGroupIndices <- cut(t-SD$demography$birthDate,pars$contactAgeGroupBreaks,1:(length(pars$contactAgeGroupBreaks)-1))
  
  # Work out which age category each host falls into  
  treatmentAgeGroupIndices_new <- cut(t-SD$demography$birthDate,pars$treatmentAgeGroupBreaks,1:(length(pars$treatmentAgeGroupBreaks)-1))
  VaccTreatmentAgeGroupIndices_new <- cut(t-SD$demography$birthDate,pars$VaccTreatmentAgeGroupBreaks,1:(length(pars$VaccTreatmentAgeGroupBreaks)-1)) #KK
  
  # Work out which hosts have moved into an older age category. Then - for systematic attendance - decide whether those hosts will ever attend
  # while they're in this age category. Note they are not allocated a new adherence factor as that's a lifelong property.
  # For the other settings this will all be handled organically by calling getAttendance() during chemo.
  #if (pars$adherenceSetting == "systematic"){
  #  hostsInNewAgeGroups = which(as.numeric(as.character(SD$treatmentAgeGroupIndices)) < as.numeric(as.character(treatmentAgeGroupIndices_new))) # have to convert R's factor datatype to numeric via character (?!?!)
  #  SD$attendance[hostsInNewAgeGroups] = getAttendance(SD$adherenceFactors[hostsInNewAgeGroups], pars$coverage, treatmentAgeGroupIndices_new[hostsInNewAgeGroups])
  #}
  
  # Update the age categories
  SD$treatmentAgeGroupIndices = treatmentAgeGroupIndices_new
  SD$VaccTreatmentAgeGroupIndices = VaccTreatmentAgeGroupIndices_new #KK
  
  return(SD)
}





# Chemotherapy
#
# How attendance works:
#   You have a boolean attendance vector describing whether each host will attend treatment at any particular point. You also have an
#   adherenceFactor for each host describing their propensity to attend treatment.
#   * For the systematic adherence setting the adherenceFactor, probability of attending and attendance vector are all generated only
#     once, at the start. Note that if coverage changes over time that *will not* affect attendance in this setting; it's based on
#     initial coverage. Might want to change that in future.
#   * For the semi-systematic adherence setting the adherenceFactor is only generated once but probability of attending and the attendance
#     vector are recalculated every time the getAttendance() function is called - so at each treatment. Note that will handle changing coverage.
#   * For random adherence, to keep the approach consistent with the other settings all the above is regenerated at every treatment, including
#     the adherenceFactor.
doChemo <- function (pars, SD, t, coverage) 
{  
  # Decide which individuals are treated; if this is a systematic non-compliance setting then use the pre-calculated attendance, for a
  # semi-systematic setting reset which are treated by re-randomising over fixed probabilities of attending*, or for a random setting also
  # re-randomise probabilities of attending.
  # * "fixed" assuming constant coverage
  #if (pars$adherenceSetting == "systematic")
  #{
  #  attendance = SD$attendance
  #} else if (pars$adherenceSetting == "semi-systematic")  
  #{
  #  attendance = getAttendance(SD$adherenceFactors, pars$coverage, SD$treatmentAgeGroupIndices)
  #} else if (pars$adherenceSetting == "random")
  #{
  #  attendance <- runif(pars$N) < pars$coverage[SD$treatmentAgeGroupIndices]
  #} else stop("Adherence setting not properly specified in parameters")
  
  attendance <- runif(pars$N) < coverage[SD$treatmentAgeGroupIndices]
  
  toTreatNow <- attendance & SD$compliers  ## they're compliers and it's their turn. 
  n2Treat <- sum(toTreatNow)
  
  # How many worms to die? 
  maleWorms <- SD$worms$total[toTreatNow] - SD$worms$female[toTreatNow]
  maleToDie <- rbinom(n2Treat,maleWorms,pars$DrugEfficacy) 
  femaleToDie <- rbinom(n2Treat,SD$worms$female[toTreatNow],pars$DrugEfficacy) 
  
  SD$worms$total[toTreatNow] <- SD$worms$total[toTreatNow] - maleToDie - femaleToDie
  SD$worms$female[toTreatNow] <- SD$worms$female[toTreatNow] - femaleToDie
  
  # Save actual attendance record and the age of each host when treated
  SD$attendanceRecord = rbind(SD$attendanceRecord, toTreatNow)
  SD$ageAtChemo =  rbind(SD$ageAtChemo, t-SD$demography$birthDate)
  SD$adherenceFactorAtChemo = rbind(SD$adherenceFactorAtChemo, SD$adherenceFactors)
  
  return ( SD )  
} 
doVaccine <- function (pars, SD, t, VaccCoverage) #KK
{  
  # Decide which individuals are treated; if this is a systematic non-compliance setting then use the pre-calculated attendance, for a
  # semi-systematic setting reset which are treated by re-randomising over fixed probabilities of attending*, or for a random setting also
  # re-randomise probabilities of attending.
  # * "fixed" assuming constant coverage
  #if (pars$adherenceSetting == "systematic")
  #{
  #  attendance = SD$attendance
  #} else if (pars$adherenceSetting == "semi-systematic")  
  #{
  #  attendance = getAttendance(SD$adherenceFactors, pars$coverage, SD$treatmentAgeGroupIndices)
  #} else if (pars$adherenceSetting == "random")
  #{
  #  attendance <- runif(pars$N) < pars$coverage[SD$treatmentAgeGroupIndices]
  #} else stop("Adherence setting not properly specified in parameters")
  
  #vaccinate <- runif(pars$N) < VaccCoverage ##KK commented out for debug
  #vaccinate <- runif(pars$N) < VaccCoverage[SD$VaccTreatmentAgeGroupIndices]  ###KK debug
  temp <- (as.numeric(SD$VaccTreatmentAgeGroupIndices)+1)%/%2
  vaccinate <- runif(pars$N) < VaccCoverage[temp]  ###KK debug
  #SD$sv[SD$VaccTreatmentAgeGroupIndices==2 & vaccinate] <- 2

  indicesToVaccinate <- (1:length(pars$VaccTreatmentBreaks)) * 2  ## JET: should refer to the hosts in vacc age groups. 
  Hosts4Vaccination <- SD$VaccTreatmentAgeGroupIndices %in% indicesToVaccinate
  SD$sv[Hosts4Vaccination & vaccinate] <- 2
  SD$vaccCount <- SD$vaccCount + sum(Hosts4Vaccination & vaccinate)
  
  # cat(file=logFile,"From doVaccine\n",append=TRUE)
  # 
  # for(i in 1:pars$N)
  # {
  #   cat(file=logFile,t - SD$demography$birthDate[i],"\t",SD$VaccTreatmentAgeGroupIndices[i],"\t",vaccinate[i],"\t",SD$sv[i],"\n",append=TRUE)
  # }

  return ( SD )  
} 

## If you uncomment this be careful about which demog value is called as the previous demog entries found in paramsTEST.txt were changed in context
# Return death dates for nNewborn individuals. 
#lifeSpans <- function(nNewborn,currentParams)
#{
#	lifeSpans <- -currentParams$demog[2]*log(runif(currentParams$N)) 
#	return(lifeSpans)
#}

# Return the value of psi, given the parameters and R0 
getPsiOLD <- function(pars)
{
	qVector <- pars$hostMu + pars$sigma + 1  # The 1 is for movement from one class to the next  #KK do we need any v1 here?ignore
	preSurvivalLog <- c(0,-log(1+pars$hostMu+pars$sigma)) #KK do we need any v1 here?no
	wormSurvivalLog <- cumsum(preSurvivalLog) 

	# Construct the matrix and sum it
	summationMatrix <- matrix(0,pars$NAG,pars$NAG)
	ageIndices <- cut(1:pars$NAG-0.1,pars$contactAgeGroupBreaks,1:(length(pars$contactAgeGroupBreaks)-1))
	betaAge <- pars$contactRates[ageIndices]#KK do we need any v3 here?ignore
	for(i in 1:pars$NAG)
	{
		for(j in i:pars$NAG)
		{
			summationMatrix[i,j] <- betaAge[i]*pars$hostSurvivalCurve[j]*exp(wormSurvivalLog[j] - wormSurvivalLog[i])/qVector[j]
		}
	}
	
	summation <- pars$lambda*pars$z*sum(summationMatrix)/(pars$hostSurvivalTotal*pars$LDecayRate) #KK do we need any v2 here?ignore
	
	summation <- summation * 0.5   # Only half the worms are female and the model is in terms of female worms
	
	return(pars$R0/summation)
}

## Calculate and return the correct psi to give the required R0. 
## New calculation of R0 with higher resolution too. 
getPsi_b <- function(pCurrent)#KK not using this function
{
	## higher resolution. 
	deltaT <- 0.1
	
	## inteval-centered ages for the age intervals. 
	## 0 to maxHostAge, mid points. 
	modelAges <- seq(0,pCurrent$maxHostAge-deltaT,by=deltaT) + 0.5*deltaT ## each age group in the actual model is annual characterised by mid-value (0.5 is [0,1), etc.)
	
	## hostMu for the new age intervals. 
	hostMuGroupIndex <- cut(modelAges,breaks=pCurrent$muBreaks,labels=1:length(pCurrent$hostMuData)) 
	hostMu <- pCurrent$hostMuData[hostMuGroupIndex] 
	
	meanDeaths <- hostMu*deltaT
	hostSurvivalCurve <- exp(-cumsum(meanDeaths))
	
	hostSurvivalTotal <- sum(hostSurvivalCurve[1:length(modelAges)])*deltaT  ## This should be the integral under the survival curve UP TO THE TOP AGE LIMIT. 
	
	## calculate the cumulative sum of host and worm death rates from which to calculate worm survival. 
	intMeanWormDeathEvents <- cumsum(hostMu+pCurrent$sigma)*deltaT #KK do we need any v1 here?
	
	## need rho and beta at this age resolution as well. 
	ageIndices <- 1:(length(pCurrent$contactAgeGroupBreaks)-1) 
	modelAgeGroupCatIndex <- cut(modelAges,breaks=pCurrent$contactAgeGroupBreaks,labels=ageIndices) 
	betaAge <- pCurrent$contactRates[modelAgeGroupCatIndex]#KK do we need any v3 here?
	rhoAge <- pCurrent$rho[modelAgeGroupCatIndex] 
	
	
	## calculate the infectiousness bit first. 
	nMax <- length(hostMu)
	K <- rep(0,nMax)
	
	for(i in 1:nMax)
	{
		a <- i:nMax
		currentMeanDeath <- intMeanWormDeathEvents[a] - intMeanWormDeathEvents[i]
		K[i] <- deltaT*sum(rhoAge[a]*exp(-currentMeanDeath))
	}
	
	summation <- sum(betaAge*hostSurvivalCurve*K)*deltaT
	
	psi <- pCurrent$R0*hostSurvivalTotal*pCurrent$LDecayRate/(pCurrent$lambda*pCurrent$z*summation) #KK do we need any v2 here?no
	
	return(psi*2) ## This two because it's infection with both male and female worms and only half of these are going to be female. 
}

## new psi calculation. 
## Results are returned as a list because we want deltaT and B for IC calculation. 
getPsi <- function(pCurrent)
{
	## higher resolution. 
	deltaT <- 0.1
	
	## inteval-centered ages for the age intervals. 
	## 0 to maxHostAge, mid points. 
	modelAges <- seq(0,pCurrent$maxHostAge-deltaT,by=deltaT) + 0.5*deltaT ## each age group in the actual model is annual characterised by mid-value (0.5 is [0,1), etc.)
	
	## hostMu for the new age intervals. 
	hostMuGroupIndex <- cut(modelAges,breaks=pCurrent$muBreaks,labels=1:length(pCurrent$hostMuData)) 
	hostMu <- pCurrent$hostMuData[hostMuGroupIndex] 
	
	meanDeaths <- hostMu*deltaT
	hostSurvivalCurve <- exp(-cumsum(meanDeaths))
	
	MeanLifespan <- sum(hostSurvivalCurve[1:length(modelAges)])*deltaT  ## This should be the integral under the survival curve UP TO THE TOP AGE LIMIT. 
	
	## calculate the cumulative sum of host and worm death rates from which to calculate worm survival. 
	intMeanWormDeathEvents <- cumsum(hostMu+pCurrent$sigma)*deltaT #KK do we need any v1 here?no
	
	## need rho and beta at this age resolution as well. 
	ageIndices <- 1:(length(pCurrent$contactAgeGroupBreaks)-1) 
	modelAgeGroupCatIndex <- cut(modelAges,breaks=pCurrent$contactAgeGroupBreaks,labels=ageIndices) 
	betaAge <- pCurrent$contactRates[modelAgeGroupCatIndex]#KK do we need any v3 here? no
	rhoAge <- pCurrent$rho[modelAgeGroupCatIndex] 
	
	
	wSurvival <- exp(-pCurrent$sigma*modelAges)#KK do we need any v1 here? no
	
	## calculate the infectiousness bit first. 
	nMax <- length(hostMu)
	B <- rep(0,nMax)
	
	for(i in 1:nMax)
	{
		B[i] <- sum(betaAge[1:i]*wSurvival[i:1])*deltaT
	}
	
	summation <- sum(rhoAge*hostSurvivalCurve*B)*deltaT
	
#	meanBeta <- sum(betaAge*hostSurvivalCurve)*deltaT/MeanLifespan
	
	psi <- pCurrent$R0*MeanLifespan*pCurrent$LDecayRate/(pCurrent$lambda*pCurrent$z*summation)#KK do we need any v2 here?no
		
	## returning three bits of info. 
	#out <- list(psiThing=psi*2, deltaT=deltaT,B=B)
	return(psi)  
}


# Draw a lifespan from the population survival curve. 
getLifeSpans <- function(nSpans,pars)
{
	spans <- rep(0,nSpans)
	for(i in 1:nSpans)
	{
		spans[i] <- which(   runif(1)  *  max(pars$hostAgeCumulDistr)<pars$hostAgeCumulDistr   )[1]
	}
	return(pars$muAges[spans])
}


############################################## DETERMINISTIC FUNCTIONS FOR EQUILIBRIUM CALC ##########################################


## takes the parameter structure. 
## returns a structure containing the equilibrium worm burden with age and the reservoir value. 
## Also the breakpoint reservoir value and other things. 
## N.B. - psi should already be calculated. 
getEquilibrium <- function(pCurrent)
{
  ## higher resolution. 
  deltaT <- 0.1
  
  ## inteval-centered ages for the age intervals. 
  modelAges <- seq(0,pCurrent$maxHostAge-deltaT,by=deltaT) + 0.5*deltaT ## each age group in the actual model is annual characterised by mid-value (0.5 is [0,1), etc.)
  
  ## hostMu for the new age intervals. 
  hostMuGroupIndex <- cut(modelAges,breaks=pCurrent$muBreaks,labels=1:length(pCurrent$hostMuData)) 
  hostMu <- pCurrent$hostMuData[hostMuGroupIndex] 
  
  meanDeaths <- hostMu*deltaT
  hostSurvivalCurve <- exp(-cumsum(meanDeaths))
  
  MeanLifespan <- sum(hostSurvivalCurve[1:length(modelAges)])*deltaT  ## This should be the integral under the survival curve UP TO THE TOP AGE LIMIT. 
  
  ## need rho and beta at this age resolution as well. 
  ageIndices <- 1:(length(pCurrent$contactAgeBreaks)-1) 
  modelAgeGroupCatIndex <- cut(modelAges,breaks=pCurrent$contactAgeBreaks,labels=ageIndices) 
  betaAge <- pCurrent$contactRates[modelAgeGroupCatIndex]#KK do we need any v3 here?no
  rhoAge <- pCurrent$rho[modelAgeGroupCatIndex] 
  
  wSurvival <- exp(-pCurrent$sigma*modelAges) #KK do we need any v1 here?no
  
  nMax <- length(hostMu)
  Q <- rep(0,nMax)  ## This variable times L is the equilibrium worm burden. 
  
  for(i in 1:nMax)
  {
    Q[i] <- sum(betaAge[1:i]*wSurvival[i:1])*deltaT
  }
  
  ## converts L values into mean force of infection. 
  FOIMultiplier <- sum(betaAge*hostSurvivalCurve)*deltaT/MeanLifespan
  
  ## upper bound on L. 
  SRhoT <- sum(hostSurvivalCurve*rhoAge)*deltaT
  R_power <- 1/(pCurrent$k+1)
  L_hat <- pCurrent$z*pCurrent$lambda*pCurrent$psi*SRhoT*pCurrent$k*(pCurrent$R0^R_power - 1)/(pCurrent$R0*MeanLifespan*pCurrent$LDecayRate*(1-pCurrent$z)) #KK do we need any v2 here?no
  
  ## now check the value of the K function across a series of L values.
  ## find point near breakpoint. L_minus is the value that gives an age-averaged worm burden of 1. Negative growth should exist somewhere below this. 
  L_minus <- MeanLifespan/sum(Q*hostSurvivalCurve*deltaT)
  test_L <- c(seq(0,L_minus,length.out=10),seq(L_minus,L_hat,length.out=20))
  
  K_valueFunc <- function(currentL,pCurrent)
  {
    answer <- pCurrent$psi*sum(pCurrent$reproFunc(currentL*Q,pCurrent)*rhoAge*hostSurvivalCurve*deltaT)/(MeanLifespan*pCurrent$LDecayRate) - currentL
    return(answer)
  }
  
  K_values <- sapply(test_L, FUN=K_valueFunc,pCurrent=pCurrent)
  
  ## now find the maximum of K_values and use bisection to find critical Ls.
  iMax <- which.max(K_values)
  
  mid_L <- test_L[iMax]
  
  ## is mid_l < 0? 
  ## 20/5/16: why not just check the K value at this point? This also guarrantees that the uniroot call below has interval ends of opposite sign.  
  if(K_values[iMax] < 0) # (iMax == 1) OLD
  {
    solutions <- list(stableProfile=0*Q,ageValues=modelAges,L_stable=0,L_breakpoint=NA,K_values=K_values,L_values=test_L,FOIMultiplier=FOIMultiplier)
    return(solutions)
  }
  
  ## find the top L...
  L_stable <- uniroot(K_valueFunc,interval=c(mid_L,L_hat),pCurrent=pCurrent)$root
  
  ## find the unstable L... Start at 1 in from the zero at the bottom. 
  #L_break <- NA  ## sometimes the first point is not within the negative range. Need to use worms 0 to 2 as defined on the L scale.  
  L_break <- test_L[2]/50 
  if(K_valueFunc(L_break,pCurrent)<0)  ## if it is less than zero at this point, find the zero. 
  {
    L_break <- uniroot(K_valueFunc,interval=c(L_break,mid_L),pCurrent=pCurrent)$root
  }
  
  
  stableProfile <- L_stable*Q 
  
  solutions <- list(stableProfile=stableProfile,ageValues=modelAges,hostSurvival=hostSurvivalCurve,L_stable=L_stable,L_breakpoint=L_break,K_values=K_values,L_values=test_L,FOIMultiplier=FOIMultiplier)
  return(solutions)
}

