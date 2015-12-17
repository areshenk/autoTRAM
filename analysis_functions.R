closest = function(data, match_value){
	# Returns the index of the value in data which is closest to match_value
	return(which.min(abs(data-match_value)))
}

trialLatency <- function(data){
	# Computes latency trial latency (defined as the elapsed time,
	# in seconds, from the start until the end of the trial)

	n = dim(data)[1]
	return(data$time[n] - data$time[1])
}

pathLength = function(data, normalize = F){
	# Computes the total distance traveled in a trial
	# If normalize = T, path length is divided by the distance between
	# the starting and ending points, giving the proportion of "excess travel"

	x = data$x
	y = data$y
	n = length(x)

	len = 0;
	len = sum(sqrt(diff(x)^2 + diff(y)^2))
	
	if(normalize){
		total = sqrt((x[n] - x[1])^2 + (y[n] - y[1])^2)
		return(len / total)
	} else {
		return(len)
	}
}

tenPercTime = function(data){
	# Returns the time taken to reach a distance to the platform
	# of 10% of the total distance from the start to the platform.

	x = data$x
	y = data$y
	n = length(x)

	total = sqrt((x[n] - x[1])^2 + (y[n] - y[1])^2)
	dist_to_goal = sqrt((x - x[n])^2 + (y - y[n])^2)
	return(data$time[closest(dist_to_goal, .9 * total)] - data$time[1])
}

quadrantDwell = function(data){
	# Returns a vector of dwell proportions for each of the 4 quadrants.
	# Quadrants are ordered counter-clockwise from the top-right.

	x = data$x
	y = data$y
	n = length(x)
  	t <- atan(y/x) + pi/2
	quadrant = .bincode(t, breaks = c(0, pi/2, pi, 3*pi/2, 2*pi))

	dwell_prop = c(sum(1*(quadrant == 1)) / n,
		sum(1*(quadrant == 2)) / n,
		sum(1*(quadrant == 3)) / n,
		sum(1*(quadrant == 4)) / n)

	return(dwell_prop)
}

platformDev = function(data, p_loc){
	# Computes the Euclidean distance between the participant's 
	# final location in a trial, and the true location of the platform
	# given by the vector p_loc.

	x = data$x
	y = data$y
	n = length(x)

	return(sqrt((x[n] - p_loc[1])^2 + (y[n] - p_loc[2])^2))
}

finalCoords = function(data){
	# Returns the coordinates of the final location attained by a player
    # in a trial.

	x = data$x
	y = data$y
	n = length(x)

	return(c(x[n], y[n]))
}

pathCurvature = function(data){
	# Computes the total unsigned curvature of the navigation path

	x = data$x
	y = data$y
	N = length(x)

	# Dwell points may produce vanishing derivative, so we
	# construct a velocity-homogeneous representation of the
	# path taken during a trial.
	thresh = 5
	newx = x[1]
	newy = y[1]
	for(n in 2:N){
		newn = length(newx)
		edist = sqrt((newx[newn] - x[n])^2 + (newy[newn] - y[n])^2)
		if(edist > thresh){
			newx = c(newx, x[n])
			newy = c(newy, y[n])
		}
	}

	# Now, estimate derivatives needed to compute curvature
	x_p = diff(newx)
	y_p = diff(newy)
	x_pp = diff(x_p)
	y_pp = diff(y_p)

	# Compute curvature
	k = abs(x_p * y_pp - y_p * x_pp) / 
		(x_p^2 + y_p^2)^(3/2)

	return(sum(k))

}
