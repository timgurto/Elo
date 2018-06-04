svg(filename="mtg.svg", width=8, height=5, pointsize=10, bg="black")

par(bg = "black")
par(fg="#999999")
par(mar=c(1, 3, 2, 8))

data <- read.csv("mtg.csv", head=FALSE)

player1 = as.vector(t(data[1]))
player2 = as.vector(t(data[3]))
result = as.vector(t(data[2]))

numEntries = length(result)

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

xFrom = 1:numEntries
xTo = 2:(numEntries+1)

i = 0
for (name in players){

    colors = c("#000000")
    if (name == "Werewolves")
        colors = c("#E41A1C", "#4DAF4A")
    if (name == "Zombies")
        colors = c("#377EB8", "#333333")
    if (name == "Proliferate")
        colors = c("#888888", "#E41A1C", "#377EB8")
    if (name == "Mill")
        colors = c("#377EB8")
    if (name == "Humans")
        colors = c("#FFFF99")
    if (name == "Elfasaurus")
        colors = c("#4DAF4A")
    
	i = i + 1
    
    yAll = t(scores[name])
    yFrom = yAll[1:numEntries]
    yTo = yAll[2:(numEntries+1)]
    
    segments(
        xFrom, yFrom, xTo, yTo,
        col=colors,
        lwd=2
    )
	
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
        if (name1 == "Mill" & name2 == "Zombies") next
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

