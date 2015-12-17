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

batch_finalCoords = function(logs){
	# Exports a .csv file of ten-percent-path times for log files
	# imported using BATCH_IMPORT. Each row is a participant,
	# and each column a trial.

	# Create storage frame
	N = length(logs)
	T = length(logs[[1]])
    
    trial_names = names(logs[[1]])
    new_trial_names = c(trial_names[1], "")
    for(i in 2:T){
        new_trial_names = c(new_trial_names, trial_names[i], "")
    }
    
	frame = data.frame(
		matrix(NA, nrow = N, ncol = 2*T),
		row.names = names(logs),
		check.names = F)
    names(frame) = new_trial_names
    
	# Populate storage frame
	for(n in 1:N){
        trial = c()
		for(t in 1:T){
            trial = c(trial, finalCoords(logs[[n]][[t]]))
		}
        frame[n,] = trial
	}
    write.csv(frame, file = 'batch_data/final_coords.csv')
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

batch_pathPlot = function(logs){
    
    # Read coordinates
    coords = read.table('support_files/plot_coords.txt',
                        sep = ',')
    
    # Coordinates
    cue = as.numeric(coords[1,])
    place = as.numeric(coords[2,])
    allo = as.numeric(coords[3,])
    ego = as.numeric(coords[4,])
    center = as.numeric(coords[5,])
    radius = as.numeric(coords[6,1])
    
    # Dimensions
    N = length(logs)
    T = length(logs[[1]])
    theta = seq(from = 0, to = 6.5, length.out = 50)
    
    # Names
    pID = names(logs)
    pID = gsub(pattern = '.log', x = pID, replacement = '')
    
    # Loop over participants
    for(n in 1:N){
        
        pdf( paste('plots/', pID[n], '_path_plots.pdf', sep = '') )
        
        # Plot individual trials
        for(t in 1:T){
            
            # Trial data
            x = logs[[n]][[t]]$x
            y = logs[[n]][[t]]$y
            N = length(x)
            plot(x, y,
                 xlim = c(center[1] - radius, center[2] + radius),
                 ylim = c(center[2] - radius, center[2] + radius),
                 lwd = 2, col = 'black', type = 'l',
                 main = names(pList[[n]])[t])
            grid(nx = NULL, ny = NULL)
            
            points(cue[1], cue[2], cex = 7.5, col = 'red', pch = 16)
            points(place[1], place[2], cex = 7.5, col = 'blue', pch = 16)
            points(ego[1], ego[2], cex = 7.5, col = 'green', pch = 16)
            points(allo[1], allo[2], cex = 7.5, col = 'yellow', pch = 16)
            lines(radius*cos(theta), radius*sin(theta), lwd = 2)
            
            legend(x = -1700, y = 1700,
                   pch = c(16,16,16,16),
                   col = c('red', 'blue', 'green', 'yellow'),
                   legend = c('Cue', 'Place', 'Ego', 'Allo'),
                   bg = 'white')
        }
        dev.off()
    }
    
}
