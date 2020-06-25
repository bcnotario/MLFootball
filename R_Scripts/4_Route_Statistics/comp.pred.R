#Completion & Yardage GLMs

comp.pred <- function(cluster,distance){
  
  #Read Game Data
  games <- read.csv('~/Documents/Projects/NFL/games.csv',header=T)
  
  #Read Play Files w/o Penalties, Special Teams Plays, Spikes Running Plays & 2pt conversions
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  plays <- plays[plays$isPenalty==F&plays$isSTPlay==F&!is.na(plays$PassLength),]
  plays <- plays[-which(regexpr('spike',plays$playDescription)>0),]
  
  #Convert Play File
  plays.dat <- plays[,c('gameId','playId','quarter','GameClock','down','yardsToGo','possessionTeam','yardlineNumber',
                   'HomeScoreBeforePlay','VisitorScoreBeforePlay','PassResult','PlayResult','PassLength',
                   'offenseFormation','defendersInTheBox','numberOfPassRushers','personnel.defense')]
  plays.dat$completion <- as.numeric(plays.dat$PassResult=='C')
  plays.dat$quarter <- as.factor(plays.dat$quarter)
  plays.dat$down <- as.factor(plays.dat$down)
  plays.dat$scorediff <- 0; plays.dat$defTeam <- NA; plays.dat$time <- 0
  plays.dat$DL <- 0; plays.dat$LB <- 0; plays.dat$DB <- 0
  
  for(t in 1:nrow(plays.dat)){
    
    #Score Difference
    team.sign <- 2*(as.numeric(games[games$gameId==plays.dat$gameId[t],'homeTeamAbbr']==plays.dat$possessionTeam[t])-.5)
    plays.dat[t,19] <- team.sign*(plays.dat$HomeScoreBeforePlay[t]-plays.dat$VisitorScoreBeforePlay[t])
    
    #Defensive Team
    plays.dat[t,20] <- ifelse(games[games$gameId==plays.dat$gameId[t],'homeTeamAbbr']!=plays.dat$possessionTeam[t],
                              as.character(games[games$gameId==plays.dat$gameId[t],'homeTeamAbbr']),
                              as.character(games[games$gameId==plays.dat$gameId[t],'visitorTeamAbbr']))
    
    #Time Conversion
    plays.dat[t,21] <- as.numeric(strsplit(as.character(plays.dat$GameClock[t]),':')[[1]][1])+
    as.numeric(strsplit(as.character(plays.dat$GameClock[t]),':')[[1]][2])/60
    
    #Personnel Defense
    plays.dat[t,22] <- substring(strsplit(as.character(plays.dat$personnel.defense[t]),split=", ")[[1]][1],1,1)
    plays.dat[t,23] <- substring(strsplit(as.character(plays.dat$personnel.defense[t]),split=", ")[[1]][2],1,1)
    plays.dat[t,24] <- substring(strsplit(as.character(plays.dat$personnel.defense[t]),split=", ")[[1]][3],1,1)
  }
  
  plays.dat$DL <- as.factor(plays.dat$DL); plays.dat$LB <- as.factor(plays.dat$LB); plays.dat$DB <- as.factor(plays.dat$DB)
  
  #Aggregate Route Types by Position
  cluster <- cluster[order(cluster$gameId,cluster$playId,cluster$alpha),]
  rclus <- aggregate(cluster$alpha,by=list(cluster$gameId,cluster$playId,cluster$pos),FUN=paste0,collapse='')
  colnames(rclus) <- c('gameId','playId','pos','alpha')
  rclus <- rclus[order(rclus$gameId,rclus$playId),]
  
  #Aggregate Route Types for ALL
  zclus <- aggregate(cluster$alpha,by=list(cluster$gameId,cluster$playId),FUN=paste0,collapse='')
  colnames(zclus) <- c('gameId','playId','alpha')
  zclus <- zclus[order(zclus$gameId,zclus$playId),]
  
  #Merge Route Types into Play Data by Position
  rplay <- merge(x=plays.dat,y=rclus[rclus$pos=='WR',],by=c('gameId','playId'),all.x=T)
  rplay <- merge(x=rplay,y=rclus[rclus$pos=='TE',],by=c('gameId','playId'),all.x=T)
  rplay <- merge(x=rplay,y=rclus[rclus$pos=='RB',],by=c('gameId','playId'),all.x=T)
  rplay <- merge(x=rplay,y=zclus,by=c('gameId','playId'),all.x=T)
  colnames(rplay)[25:31] <- c('WR','alpha.wr','TE','alpha.te','RB','alpha.rb','alpha.all')
  
  #Merge Distance Stats into Play Data & Remove NAs
  rplay <- merge(x=rplay,y=distance$defend.stat,by=c('gameId','playId'),all.x=T)
  rplay$wr.cdist[is.na(rplay$wr.cdist)] <- 0; rplay$te.cdist[is.na(rplay$te.cdist)] <- 0
  rplay$rb.cdist[is.na(rplay$rb.cdist)] <- 0
  rplay$min.backf[is.infinite(rplay$min.backf)] <- NA
  
  #Completion Models
  summary(glm(completion~b.dist+min.bdist+backf.dist+PassLength+
                wr.cdist+te.cdist+rb.cdist+all.cdist+side.cdist+
                offenseFormation+defendersInTheBox+numberOfPassRushers+LB+DB+DL+
                quarter+time+down+yardsToGo+scorediff,data=rplay,family=binomial))
  complet.mod1b <- glm(completion~b.dist+min.backf+backf.dist+wr.cdist+te.cdist+rb.cdist,data=rplay,family=binomial)
  summary(complet.mod1b)
  complet.mod2b <- glm(completion~b.dist+min.bdist+backf.dist+PassLength+all.cdist+side.cdist+
                         offenseFormation+defendersInTheBox,data=rplay,family=binomial)
  summary(complet.mod2b)
  summary(glmer(completion~b.dist+wr.cdist+te.cdist+rb.cdist+min.bdist+backf.dist+side.cdist+
                  quarter+time+down+yardsToGo+scorediff+(1|possessionTeam)+(1|defTeam),data=rplay,family=binomial))
  
  #Yardage Model
  summary(lm(PlayResult~b.dist+min.bdist+backf.dist+PassLength+
               wr.cdist+te.cdist+rb.cdist+all.cdist+side.cdist+
               offenseFormation+defendersInTheBox+numberOfPassRushers+LB+DB+DL+
               quarter+time+down+yardsToGo+scorediff,data=rplay))
  yardage.mod1b <- lm(PlayResult~b.dist+min.bdist+min.backf+backf.dist+PassLength+wr.cdist+te.cdist+rb.cdist,data=rplay)
  summary(yardage.mod1b)
  yardage.mod2b <- lm(PlayResult~b.dist+min.bdist+backf.dist+PassLength+wr.cdist+te.cdist+rb.cdist+
                        defendersInTheBox+numberOfPassRushers,data=rplay)
  summary(yardage.mod2b)
  summary(lmer(PlayResult~b.dist+wr.cdist+te.cdist+rb.cdist+min.bdist+backf.dist+side.cdist+
                         quarter+time+down+yardsToGo+scorediff+(1|possessionTeam)+(1|defTeam),data=rplay))
  
  #Output
  return(list(complet.mod=complet.mod1b,yardage.mod=yardage.mod1b,rplay=rplay))
}
