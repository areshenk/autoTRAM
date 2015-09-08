getLogList = function(){
	# Calls bash for a list of the log_file directory
	return(list.files(path = 'log_files/'))
}

importLog = function(x){
	# Imports the log file with filename "x". 
	# Outputs a list with elements corresponding to the time, location and
	# rotation vectors, and trial name markers

	raw_log = readLines(paste('log_files/', x, sep = ''))

	return(list(time = raw_log[sapply(raw_log, function(x) grepl('^.*ScriptLog.*$', x)) &
				  sapply(raw_log, function(x) grepl('^.*Time.*$', x))],
		    location = raw_log[sapply(raw_log, function(x) grepl('^.*ScriptLog.*$', x)) &
				       sapply(raw_log, function(x) grepl('^.*Location.*$', x))],
		    rotation = raw_log[sapply(raw_log, function(x) grepl('^.*ScriptLog.*$', x)) &
				       sapply(raw_log, function(x) grepl('^.*Rotation.*$', x))],
		    markers = raw_log[sapply(raw_log, function(x) grepl('^.*Kismet.*$', x))]))
}

splitLog = function(x){
	# Accepts a list from importLog and splits it in order to
	# produce a dataframe useable by the data analysis functions.

	time_frame = data.frame(matrix(unlist(sapply(x$time, function(x) strsplit(x, "\\[|\\]|,| "))), 
				nrow=length(x$time), 
				byrow = T))
	rotation_frame = data.frame(matrix(unlist(sapply(x$rotation, function(x) strsplit(x, "\\[|\\]|,| "))), 
				    nrow=length(x$rotation), 
				    byrow = T))
	location_frame = data.frame(matrix(unlist(sapply(x$location, function(x) strsplit(x, "\\[|\\]|,| "))), 
				    nrow=length(x$location), 
				    byrow = T))
	marker_frame = data.frame(matrix(unlist(sapply(x$marker, function(x) strsplit(x, "\\[|\\]|,| "))), 
				  nrow=length(x$marker), 
				  byrow = T))

	return(list(time = time_frame,
		    rotation = rotation_frame,
		    location = location_frame,
		    markers = marker_frame))
}

assembleLog = function(x){
	# Creates the master dataframe used by the analysis functions
	len = dim(x$time)[1]
	master = data.frame(index = as.numeric(x$location[,2]),
			    time = as.numeric(x$time[,6]),
			    x = as.numeric(x$location[,6]),
			    y = as.numeric(x$location[,7]),
			    z = as.numeric(x$location[,8]),
			    pitch = as.numeric(x$rotation[,6]),
			    roll = as.numeric(x$rotation[,7]),
			    yaw = as.numeric(x$rotation[,8]))
	
	# Assemble marker frame
	marker_frame = data.frame(Trial = x$markers[,7],
				  Start = rep(NA, length(x$markers[,7])),
				  End = rep(NA, length(x$markers[,7])))
	for(i in 1:(length(x$markers[,7])-1)){
		marker_frame$Start[i] = as.numeric(x$markers[i,2])
		marker_frame$End[i] = as.numeric(x$markers[i+1,2])
	}
	
	return(list(performance_frame = master,
		    trial_frame = marker_frame))

}

piecewiseLog = function(x){
	# Creates a list of dataframes, each element of which which contains
	# data for a single trial. Accepts output from assembleLog
	trialnames = x$trial_frame[,1]
	n = length(trialnames)
	y = list()
	eNorm = function(u,v) abs(u - v) 

	for(t in 1:(n-1)){
		start = which.min(eNorm(x$performance_frame[,1], x$trial_frame[t,2]))
		end = which.min(eNorm(x$performance_frame[,1], x$trial_frame[t,3]))
		y[[t]] = x$performance_frame[start:end,]
	}
	names(y) = x$trial_frame[1:(n-1),1]
	return(y)
}
