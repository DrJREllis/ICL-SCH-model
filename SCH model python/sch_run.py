
from sch_simulation.helsim_RUN_KK import SCH_Simulation
from sch_simulation.helsim_FUNC_KK import extractHostData, getPrevalence, getIndividualResults, getTargetThresholdProbabilities
#import pandas as pd
import json
import numpy as np

numReps=50


demogName='UgandaRural' #'KenyaKDHS'

paramFileName='Haematobium.txt'

#Name to use for saving the outputs
outputFileName = 'Test'

#Optional override to parameter values
paramsOverride=dict()

[results, params] = SCH_Simulation(paramFileName=paramFileName, demogName=demogName, paramsOverride=paramsOverride,numReps=numReps)

np.save('outputs/' + outputFileName + '_params',np.array(params))


# process the output         
output = extractHostData(results)

#Save everything - not recommended as the files can be several GBs
# np.save('outputs/' + outputFileName + '_allResults',results)

# transform the output to data frame of prevalences over time based on egg counts
prevalence = getPrevalence(output, params, numReps)
prevalence.to_json('outputs/' + outputFileName + '_prevalences.json')

# Generate egg and worm counts for each individual - these generate large files so should only be saved as outputs if detailed analysis is necessary
individualResults = getIndividualResults(output,params)
# np.save('outputs/' + outputFileName + '_eggCounts',np.array(individualResults['egg_results']))
# np.save('outputs/' + outputFileName + '_wormCounts',np.array(individualResults['worm_results']))
# np.save('outputs/' + outputFileName + '_ages',np.array(individualResults['ages']))

# Calculate the probability of reaching prevalence thresholds over time
targetProb = getTargetThresholdProbabilities(individualResults,params)
targetProb.to_json('outputs/' + outputFileName + '_targetProbabilities.json')


