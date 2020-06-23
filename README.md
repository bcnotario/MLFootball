# Football Route Classifier & Evaluation
## Route Classification

### route.class
Route segments, angles, and error by play
```
route.class(play.j,catch.j,players.j,pass.j,snap.j,track.file)
```
### route.pre
Route characteristics calculated for all plays (using route.class)
```
cluster.dat <- route.pre()
```

## Route Assignments & Classification Results
### cluster.assign
Route assignments based on segments lengths and angle
```
cluster.rt <- cluster.assign(cluster.dat)
```
### cluster.plot
Overlay of receiver paths by route
```
cluster.path <- cluster.plot(cluster.rt)
```

## Defensive Distance Predictors of Play Results
### dist.defendA
Minimum distances from defenders when the ball reaches the intended target
```
dist.result <- dist.defendA()
```
### dist.defendB
Cumulative distances from defenders until the ball reaches the intended target
```
dist.result <- dist.defendB()
```
### dist.summary
Aggregation of defensive distances by game and play
```
dist.play <- dist.summary(dist.result)
```

## Route Statistics by Play (after Cluster Assignments)
### comp.pred
Completion & Yardage GLMs based on play statistics and defense metrics
```
play.mod <- comp.pred(cluster.rt,dist.play)
summary(play.mod$complet.mod)
summary(play.mod$yardage.mod)
```
### play.resultWR
For all WR plays, calculate completion probability, expected yardage, and average route stastistics by play
```
play.summaryWR <- play.resultWR(play.mod)
```
### play.mapWR
Print 5 examples of route combination and route results
```
play.mapWR('DHI',play.summaryWR$rplay,5)
```
