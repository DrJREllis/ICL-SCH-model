

# Remove objects
rm(list=ls())
#memory.limit(40950)
closeAllConnections()
runtime = proc.time()



##### SET UP

outFileStub <- "test"
parameterPath <- "data/Haematobium.txt"
  
currentDemogName <- "UgandaRural"


## Other parameters
outPath <- "outputs/"
nNodes <- 8 # think this is the number of cores on the machine, though I've not ever changed it and it seems to work fine - SF #JT can change to 2 to test
maxStep = 1/52 # Time steps never exceed this for deterministic update of freeliving populations
##set.seed(1) # Set random number generator seed to be reproducible 

# Define the functions
source ("helsim_FUNC.R")
source ("helsim_RUN.R")
source ("ParallelFuncs.R")

# Set up the output and recording
logFile <- paste0(outPath,outFileStub,"_log.txt")
cat(file=logFile,"Log file is open!\n")


# Read in parameters
testExist <- file(parameterPath,"r")
if(!isOpen(testExist)){
  cat(file=logFile,"Parameter file not found: ",parameterPath,"\n",append=TRUE)
  stop("Parameter file not found.")
} else {
  cat(file=logFile,"Parameter file found: ",parameterPath,"\n",append=TRUE)
}
close(testExist)
params <- readParams (parameterPath,demogName=currentDemogName)

params <- configure(params)
params$psi <- getPsi(params)
params$equiData <- getEquilibrium(params)
params$chemoTimings1 <- seq(from=params$treatStart1,by=params$treatInterval1,length.out=params$nRounds1)
params$chemoTimings2 <- seq(from=params$treatStart2,by=params$treatInterval2,length.out=params$nRounds2)
params$VaccineTimings <- seq(from=params$VaccTreatStart,by=params$treatIntervalVacc,length.out=params$nRoundsVacc)



cat(file=logFile,"Setup complete after ", (proc.time() - runtime)[["elapsed"]], " secs.\n",append=TRUE)





##### Run multiple instances of doRealization() in parallel

cat(file=logFile,"Setting up parallel stuff.\n",append=TRUE)

require("foreach")
require("doParallel",quietly=TRUE)

# output from inside the loop gets written to the outfile. Note it's appended to any existing outfile
cl <- makeCluster(nNodes, outfile="outputs/DoParallel_logging.txt") 
registerDoParallel(cl)

cat(file=logFile,"Starting parallel stuff.\n",append=TRUE)


foreachResults <- foreach(paramOverrideNr=1:params$repNum) %do% doRealization(paramOverrideNr,params,TRUE)




cat(file=logFile,"Total elapsed time = ",(proc.time() - runtime)[["elapsed"]],"\n",append=TRUE)

resultsFile <- paste0(outPath,outFileStub,".results.RData")

save(file=resultsFile,results=foreachResults)

############################################################################################

cat(file=logFile,"Log file closing...\n",append=TRUE)




