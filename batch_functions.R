batch_import = function(){
	options(warn = -1)
	filelist = getLogList()
	datalist = list()
	for(p in 1:length(filelist)){
		datalist[[p]] = 
		piecewiseLog(
			assembleLog(
				splitLog(
					importLog(filelist[p]))))
	}
	names(datalist) = filelist
	print(paste('Imported', length(datalist), 'log files.'))
	return(datalist)
}

batch_trialLatency = function(logs){
	# Exports a .csv file of trial latencies for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
	frame = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	names(frame) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			frame[n,t] = trialLatency(logs[[n]][[t]])
		}
	}

	write.csv(frame, file = 'batch_data/trial_latency.csv')
}

batch_pathLength = function(logs, normalize = F){
	# Exports a .csv file of path lengths for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
	frame = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	names(frame) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			frame[n,t] = pathLength(logs[[n]][[t]], normalize)
		}
	}

	write.csv(frame, file = 'batch_data/path_length.csv')
}

batch_tenPercTime = function(logs){
	# Exports a .csv file of ten-percent-path times for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
	frame = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	names(frame) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			frame[n,t] = tenPercTime(logs[[n]][[t]])
		}
	}

	write.csv(frame, file = 'batch_data/ten_perc_time.csv')
}

batch_dwellTimes = function(logs){
	# Exports .csv files of quadrant dwell times for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frames
	N = length(logs)
	T = length(logs[[1]])
	upright = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	upleft = upright
	downleft = upright
	downright = upright
	names(upright) = names(logs[[1]])
	names(upleft) = names(logs[[1]])
	names(downleft) = names(logs[[1]])
	names(downright) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			dwelltimes = quadrantDwell(logs[[n]][[t]])
			upright[n,t] = dwelltimes[1]
			upleft[n,t] = dwelltimes[2]
			downleft[n,t] = dwelltimes[3]
			downright[n,t] = dwelltimes[4]
		}
	}

	write.csv(upright, file = 'batch_data/dwelltime_upright.csv')
	write.csv(upleft, file = 'batch_data/dwelltime_upleft.csv')
	write.csv(downleft, file = 'batch_data/dwelltime_downleft.csv')
	write.csv(downright, file = 'batch_data/dwelltime_downright.csv')
}

batch_platformDev = function(logs){
	# Exports a .csv file of ten-percent-path times for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Import platform coordinates
	coords = read.table('support_files/platform_coords.txt',
		sep = ' ')

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
	frame = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	names(frame) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			frame[n,t] = platformDev(logs[[n]][[t]], coords)
		}
	}

	write.csv(frame, file = 'batch_data/platform_deviation.csv')
}

batch_pathCurvature = function(logs){
	# Exports a .csv file of total curvatures for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
	frame = data.frame(
		matrix(NA, nrow = N, ncol = T),
		row.names = names(logs),
		check.names = F)
	names(frame) = names(logs[[1]])

	# Populate storage frame
	for(n in 1:N){
		for(t in 1:T){
			frame[n,t] = pathCurvature(logs[[n]][[t]])
		}
	}

	write.csv(frame, file = 'batch_data/path_curvature.csv')
}