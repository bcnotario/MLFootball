#Play Summary Table

dist.summary <- function(dist.table){
  
  #Defense Table Aggregation by Game/Play
  defend <- dist.table$defend
  defend.mu <- aggregate(list(defend$b.dist,defend$wr.cdist,defend$te.cdist,defend$rb.cdist,defend$all.cdist),
                         by=list(defend$gameId,defend$playId),FUN=mean,na.rm=T)[-1,]
  colnames(defend.mu) <- c('gameId','playId','b.dist','wr.cdist','te.cdist','rb.cdist','all.cdist')
  defend.mu <- data.frame(defend.mu[order(defend.mu$gameId,defend.mu$playId),],min.bdist=0,backf.dist=0,min.backf=0)
  
  for(d in 1:nrow(defend.mu)){
    defend.mu[d,8] <- min(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2],'b.dist'],na.rm=T)
    defend.mu[d,9] <- ifelse(nrow(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                         &defend$backfield==1,])==0,
                             -sum(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                         &defend$backfield==0,'b.dist'],na.rm=T),
                             sum(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                       &defend$backfield==1,'b.dist'],na.rm=T))
    defend.mu[d,10] <- ifelse(nrow(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                          &defend$backfield==1,])==0,
                              -min(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                         &defend$backfield==0,'b.dist'],na.rm=T),
                              min(defend[defend$gameId==defend.mu[d,1]&defend$playId==defend.mu[d,2]
                                  &defend$backfield==1,'b.dist'],na.rm=T))
    }
  
  #Receiver Table Aggregation by Player
  receiv <- dist.table$receiv
  
  #Receiver Speed
  receiv.spd <- aggregate(receiv$speed,by=list(receiv$displayName,receiv$jerseyNumber),FUN=mean,na.rm=T)[-1,]
  colnames(receiv.spd) <- c('displayName','jerseyNumber','speed')
  
  #Receiver Stats as Intended Receiver
  receiv.int <- aggregate(list(receiv$b.dist,receiv$min.dist,receiv$all.cdist),
                          by=list(receiv$displayName,receiv$jerseyNumber,receiv$intend),FUN=mean,na.rm=T)[-1,]
  colnames(receiv.int) <- c('displayName','jerseyNumber','intend','b.dist','min.dist','all.cdist')
  
  #Receiver Target
  receiv.tgt <- aggregate(list(receiv$all.cdist),
                          by=list(receiv$displayName,receiv$jerseyNumber,receiv$intend),FUN=NROW)[-1,]
  colnames(receiv.tgt) <- c('displayName','jerseyNumber','intend','target')
  receiv.tgt <- merge(x=receiv.tgt[receiv.tgt$intend==0,],y=receiv.tgt[receiv.tgt$intend==1,],
                      by=c('displayName','jerseyNumber'))[,c(-3,-5)]
  receiv.tgt$target <- receiv.tgt$target.x+receiv.tgt$target.y
  receiv.tgt$tgt.pct <- receiv.tgt$target.y/receiv.tgt$target
  
  #Merge Receiver Tables
  receiv.stat <- merge(x=receiv.spd,y=receiv.int[receiv.int$intend==1,],by=c('displayName','jerseyNumber'),all.y=T)[,-4]
  receiv.stat <- merge(x=receiv.stat,y=receiv.tgt,by=c('displayName','jerseyNumber'),all.x=T)[,c(-7,-8)]
  
  #Merge Defense Table with Receiver Sideline Distance
  receiv.sid <- aggregate(receiv$side.cdist,by=list(receiv$gameId,receiv$playId,receiv$intend),FUN=mean,na.rm=T)[-1,]
  colnames(receiv.sid) <- c('gameId','playId','intend','side.cdist')
  defend.mu <- merge(x=defend.mu,y=receiv.sid[receiv.sid$intend==1,-3],by=c('gameId','playId'),all.x=T)
  
  #Output
  return(list(defend.stat=defend.mu,receiv.stat=receiv.stat))
}
