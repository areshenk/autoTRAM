options(stringsAsFactors = F)
source('import_functions.R')
source('analysis_functions.R')
source('batch_functions.R')


autoTRAM = function(){
	# AUTOTRAM loads all of the log files contained in the folder
	# 'log_files' and exports .csv files of trial data to the
	# batch_data folder

	pList = batch_import()

	batch_trialLatency(pList)
	batch_pathLength(pList)
	batch_tenPercTime(pList)
	batch_dwellTimes(pList)
	batch_platformDev(pList)
	batch_pathCurvature(pList)
	print(paste('Exported', length(pList), 'summary files.'))
}