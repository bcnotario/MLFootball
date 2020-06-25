#Route Statistics (after Cluster Assignments)

play.resultWR <- function(play.update){
  
  #Read Plays File
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  
  #Load Play Data & Models
  rplay <- play.update$rplay
  complet.mod <- play.update$complet.mod
  yardage.mod <- play.update$yardage.mod
  
  rplay$cmpl.pred <- 0; rplay$yard.pred <- 0
  
  for(r in 1:nrow(rplay)){
    
    #Predictors (& Replace NA|NaN w/0)
    predictor <- rplay[r,c('b.dist','min.bdist','backf.dist','min.backf','PassLength',
                           'wr.cdist','te.cdist','rb.cdist','all.cdist','side.cdist',
                           'offenseFormation','defendersInTheBox','numberOfPassRushers')]
    predictor[is.na(predictor)] <- 0
    
    #Completion Probability
    rplay[r,41] <- round(predict(complet.mod,newdata=data.frame(predictor[c(1,3:4,6:8)]),type='response'),5)
    
    #Yardage Prediction
    rplay[r,42] <- round(predict(yardage.mod,newdata=data.frame(predictor[c(1:8)])),5)
  }
  
  #Add Offense & Defense Formations
  rplay <- merge(x=rplay,y=plays[,c('gameId','playId')],by=c('gameId','playId'),all.x=T)
  rplay$alpha.excl <- gsub('K|M',replacement='',rplay$alpha.all)
  
  #WR Routes
  route.stat <- data.frame(route=sort(unique(rplay$alpha.wr)),ATT=0,CMP=0,P.CMP=0,YDS=0,E.YDS=0,
                           b.dist=0,min.def=0,backf.dist=0,min.backf=0,pass.dist=0,
                           wr.dist=0,te.dist=0,rb.dist=0,all.dist=0,side.dist=0,
                           V.CMP=0,V.TYDS=0,V.YDS=0,MIN.YDS=0,MAX.YDS=0)
  for(j in 1:nrow(route.stat)){
    
    #Play Subset
    route.temp <- rplay[rplay$alpha.wr==route.stat[j,1]&!is.na(rplay$alpha.wr),]
    
    #Attempts, Completion, & Yardage
    route.stat[j,2] <- nrow(route.temp)
    route.stat[j,3] <- sum(as.numeric(route.temp$PassResult=='C'))/nrow(route.temp)
    route.stat[j,4] <- mean(na.omit(route.temp$cmpl.pred))
    route.stat[j,5] <- mean(na.omit(route.temp$PlayResult))
    route.stat[j,6] <- mean(na.omit(route.temp[!is.infinite(route.temp$min.bdist),'yard.pred']))
    
    #Distances
    route.stat[j,7] <- mean(na.omit(route.temp$b.dist))
    route.stat[j,8] <- mean(na.omit(route.temp[!is.infinite(route.temp$min.bdist),'min.bdist']))
    route.stat[j,9] <- mean(na.omit(route.temp$backf.dist))
    route.stat[j,10] <- mean(na.omit(route.temp$min.backf))
    route.stat[j,11] <- mean(na.omit(route.temp$PassLength))
    route.stat[j,12] <- mean(na.omit(route.temp$wr.cdist))
    route.stat[j,13] <- mean(na.omit(route.temp$te.cdist))
    route.stat[j,14] <- mean(na.omit(route.temp$rb.cdist))
    route.stat[j,15] <- mean(na.omit(route.temp$all.cdist))
    route.stat[j,16] <- mean(na.omit(route.temp$side.cdist))
    
    #Range
    route.stat[j,17] <- sd(na.omit(route.temp$cmpl.pred))
    route.stat[j,18] <- sd(na.omit(route.temp[!is.infinite(route.temp$min.bdist),'yard.pred']))
    route.stat[j,19] <- sd(na.omit(route.temp$PassLength))
    route.stat[j,20] <- min(na.omit(route.temp$PassLength))
    route.stat[j,21] <- max(na.omit(route.temp$PassLength))
  }
  
  #Output
  return(list(rplay=rplay,route.stat=route.stat))
}
