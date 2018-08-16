svg(filename="sealed.svg", width=8, height=5, pointsize=10, bg="black")

par(bg = "black")
par(fg="#999999")
par(mar=c(1, 3, 2, 11))

data <- read.csv("sealed.csv", head=FALSE)

player1 = as.vector(t(data[1]))
player2 = as.vector(t(data[3]))
result = as.vector(t(data[2]))

numEntries = length(result)

# Remember then remove asterisks on player names
markerIndices = c()
markerNames = c()
for (i in 1:(numEntries)){
    len = nchar(player1[i])
    if (substr(player1[i], len, len) == "*"){
        player1[i] = substr(player1[i], 1, len-1)
        
        markerIndices = c(markerIndices, i)
        markerNames = c(markerNames, player1[i])
    }
        
    len = nchar(player2[i])
    if (substr(player2[i], len, len) == "*"){
        player2[i] = substr(player2[i], 1, len-1)
        
        markerIndices = c(markerIndices, i)
        markerNames = c(markerNames, player2[i])
    }
}

players = sort(union(player1, player2))
numPlayers = length(players)

template = rep.int(1600, numEntries+1)
scores = data.frame(template)
for (name in players){
	scores[,name] <- template
}

K = 32

for (i in 2:(numEntries+1)){
	p1 = player1[i-1]
	r1 = 10 ^ (scores[i-1, p1] / 400)
	
	p2 = player2[i-1]
	r2 = 10 ^ (scores[i-1, p2] / 400)
	
	# Expected scores
	e1 = r1 / (r1 + r2)
	e2 = r2 / (r1 + r2)
	
	if (result[i-1] == "win"){
		s1 = 1
		s2 = 0
	} else {
		s1 = 0.5
		s2 = 0.5
	}
	
	for (j in i:(numEntries+1)){
		scores[j, p1] = scores[j, p1] + K * (s1 - e1)
		scores[j, p2] = scores[j, p2] + K * (s2 - e2)
	}
}

# Remove leading 1600s from new players
for (player in players){
    for (i in 1:(numEntries-1)){
        if (scores[i, player] == 1600 && scores[i+1, player] == 1600)
            scores[i,player] = NA
    }
}

lowest = 1600
highest = 1600
for (name in players) {
        tempScores = scores[name]
        tempScores[is.na(tempScores)] = 1600
        lowest = min(lowest, min(tempScores[name]))
        highest = max(highest, max(tempScores[name]))
}

plot(
	NULL,
	xlim=c(1, numEntries+1),
	ylim=c(lowest, highest),
	ylab="",
	xlab="",
	col.lab="#999999", col.main="#999999", col.axis="#999999",
	xaxt='n'
)

# From: http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
# x: the vector
# n: the number of samples
# centered: if FALSE, then average current sample and previous (n-1) samples
#           if TRUE, then average symmetrically in past and future. (If n is even, use one more sample from future.)
movingAverage <- function(x, n=1, centered=FALSE) {
    
    if (centered) {
        before <- floor  ((n-1)/2)
        after  <- ceiling((n-1)/2)
    } else {
        before <- n-1
        after  <- 0
    }

    # Track the sum and count of number of non-NA items
    s     <- rep(0, length(x))
    count <- rep(0, length(x))
    
    # Add the centered data 
    new <- x
    # Add to count list wherever there isn't a 
    count <- count + !is.na(new)
    # Now replace NA_s with 0_s and add to total
    new[is.na(new)] <- 0
    s <- s + new
    
    # Add the data from before
    i <- 1
    while (i <= before) {
        # This is the vector with offset values to add
        new   <- c(rep(NA, i), x[1:(length(x)-i)])

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }

    # Add the data from after
    i <- 1
    while (i <= after) {
        # This is the vector with offset values to add
        new   <- c(x[(i+1):length(x)], rep(NA, i))
       
        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new
        
        i <- i+1
    }
    
    # return sum divided by count
    s/count
}

# Segment width determined by markers:
# If last marker was this name, make it thick.
# If last marker was something else, or no marker, then make it thin.
getSegmentWidth <- function(name, i){
    # Loop through markers, until one higher than i is reached, then backtrack.  This will tell us the currently active marker.
    numMarkers = length(markerIndices)
    for (j in 1:(numMarkers+1)){
        if (j <= numMarkers && markerIndices[j] <= i)
            next

        currentMarker = markerNames[j-1]
        if (j == 1 || currentMarker != name)
            return(0.5)
        else
            return(1.5)
    }
    return(0.3)
}
        
xFrom = 1:numEntries
xTo = 2:(numEntries+1)

for (name in players){

    abline(v=markerIndices, lwd=0.2)
    
    SOM="#AAAAAA"
    
    THS="#13B521"
    BNG="#EA4B27"
    JOU="#8A54B7"
    
    SOI="#241B8E"
    EMN="#481F6B"
    
    AKH="#E5C849"
    
    XLN="#277A3D"
    RIX="#EFC428"

    colors = c("#000000")
    if (name == "Garruk")               colors = c("#333333")
    if (name == "Ixalan/Rivals")        colors = c(XLN,RIX)
    if (name == "Shadows/Eldritch")     colors = c(SOI,EMN)
    if (name == "Theros/Gods/Nyx")      colors = c(THS,BNG,JOU)
    if (name == "Amonkhet")             colors = c(AKH)
    if (name == "Scars of Mirrodin")    colors = c(SOM)
    
    yAll = t(scores[name])    
    #yAll = movingAverage(yAll, 10, TRUE) # This optional line changes the data to a moving average
    
    yFrom = yAll[1:numEntries]
    yTo = yAll[2:(numEntries+1)]
    
	for (i in 1:(numEntries)){
        if (is.na(yFrom[i])) next
        if (is.na(yTo[i])) next
        if (yFrom[i] == yTo[i]) next # This optional line hides horizontal segments

        segmentWidth = getSegmentWidth(name, i)
    
        colorIndex = i %% length(colors) + 1
        segments(xFrom[i], yFrom[i], xTo[i], yTo[i],
            col=colors[colorIndex],
            lwd=segmentWidth)
    }
	
	#xData = 1:(numEntries+1)
	#yData = (apply(scores[name], 1, function(x) x))
	#lo = loess(yData~xData)
	#xl <- seq(1, numEntries+1, 0.1)
	#lines(xl, predict(lo, xl), col=colors)
	
	currentScore = scores[numEntries+1, name]
	roundedScore = round(currentScore, 0)
	label = paste(roundedScore, " ", name)
	axis(
		4,
		at=c(currentScore),
		labels=c(label),
		col=colors[1], col.ticks=colors[1], col.axis = colors[1],
		las=2
	)
}

smallestGap = 10000
s1 = ""
s2 = ""
for (name1 in players){
    for (name2 in players){
        if (name1 >= name2) next
        score1 = scores[numEntries+1, name1]
        score2 = scores[numEntries+1, name2]
        diff = abs(score1 - score2)
        if (diff < smallestGap){
            smallestGap = diff
            s1 = name1
            s2 = name2
        }
    }
}

paste("Next game should be between ", s1, " and ", s2, sep="")

