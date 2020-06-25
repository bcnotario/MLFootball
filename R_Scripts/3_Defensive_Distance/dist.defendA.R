#Distance & Minimum Distance from Defender

dist.defendA <- function(){
  
  #Read Player & Play Files
  players <- read.csv('~/Documents/Projects/NFL/players.csv',header=T)
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  
  #Remove Penalties, Special Teams Plays, Running Plays & 2pt conversions
  plays <- plays[plays$isPenalty==F&plays$isSTPlay==F&!is.na(plays$PassLength),]
  
  #Remove Spikes
  plays <- plays[-which(regexpr('spike',plays$playDescription)>0),]
  
  #Defense & Receiver Data Frame
  defend.mat <- data.frame(gameId=0,playId=0,PositionAbbr=NA,displayName=NA,jerseyNumber=0,
                           b.dist=0,wr.cdist=0,te.cdist=0,rb.cdist=0,all.cdist=0,backfield=0)
  receiv.mat <- data.frame(gameId=0,playId=0,PositionAbbr=NA,displayName=NA,jerseyNumber=0,
                           speed=0,b.dist=0,min.dist=0,all.cdist=0,side.cdist=0,intend=0)
  
  #Loop Through All Plays
  for(j in 1:nrow(plays)){
    
    #Play Subset
    play.j <- plays[j,c('gameId','playId')]
    track.file <- play.dat0[[as.character(play.j$gameId)]]
    
    #All Players
    track.j <- merge(x=track.file[track.file$playId==play.j$playId,],y=players,by='nflId')[,c(1:14,17)]
    
    #If No Tracking Data Available for Play
    if(nrow(track.j)==0 | nrow(track.j[track.j$PositionAbbr=='QB'&!is.na(track.j$event)&track.j$event=='ball_snap',])==0){
      defend.mat <- defend.mat
      receiv.mat <- receiv.mat
    } else {
      
      #WR/TE/RB Full Routes & Players
      route.j <- track.j[track.j$PositionAbbr=='WR'|track.j$PositionAbbr=='TE'|track.j$PositionAbbr=='RB',]
      players.j <- unique(route.j[order(route.j$PositionAbbr,decreasing=T),c('PositionAbbr','displayName','jerseyNumber')])
      
      #Play Events by QB
      route.qb <- track.j[track.j$PositionAbbr=='QB',]
      event.j <- unique(route.qb[order(route.qb$frame.id),c('frame.id','event')])
      event.j <- event.j[which(!is.na(event.j$event)),]
      snap.j <- max(event.j[event.j$event=='ball_snap'|event.j$event=='man_in_motion','frame.id'])
      thrw.j <- max(event.j[event.j$event=='pass_forward','frame.id']) #Not currently used
      pass.j <- max(event.j[event.j$event=='pass_outcome_caught'|event.j$event=='pass_outcome_incomplete'|
                              event.j$event=='pass_outcome_touchdown'|event.j$event=='pass_outcome_interception'|
                              event.j$event=='pass_arrived'|event.j$event=='pass_shovel'|event.j$event=='pass_tipped','frame.id'])
      pass.j <- ifelse(pass.j==Inf|pass.j==-Inf,thrw.j,pass.j)
      catch <- route.j[route.j$frame.id>=snap.j&route.j$frame.id<=pass.j,]
      ball <- track.file[track.file$playId==play.j$playId&track.file$team=='ball'&track.file$frame.id<=pass.j,]
      frames.j <- (pass.j-snap.j+1)
      
      #Defender Distance
      fence.j <- track.j[track.j$team==ifelse(unique(route.j$team)[1]=='home','away','home')&
                           track.j$frame.id>=snap.j&track.j$frame.id<=pass.j,]
      defend.df <- data.frame(gameId=play.j$gameId,playId=play.j$playId,
                              unique(fence.j[order(fence.j$PositionAbbr),c('PositionAbbr','displayName','jerseyNumber')]),
                              b.dist=0,wr.cdist=0,te.cdist=0,rb.cdist=0,all.cdist=0,backfield=0)
      alldist.df <- data.frame(PositionAbbr=NA,displayName=NA,jerseyNumber=NA,matrix(0,1,frames.j))
      for(d in 1:nrow(defend.df)){
        
        #Defender Route
        fence.jd <- fence.j[fence.j$jerseyNumber==defend.df[d,'jerseyNumber']&fence.j$displayName==defend.df[d,'displayName'],]
        
        #Defender at Pass
        fence.jp <- fence.jd[fence.jd$frame.id==pass.j,c('x','y')]
        
        #Ball at Pass
        ball.jd <- ball[ball$frame.id==pass.j,c('x','y')]
        #Distance of Defender/Ball at Pass
        defend.df[d,6] <- ifelse(nrow(ball.jd)>0,dist(rbind(fence.jp,ball.jd),method='euclidean'),NA)
        
        #Distance for Defender vs. ALL Receivers (ALL Frames)
        offens.df <- data.frame(players.j,matrix(0,nrow(players.j),frames.j))
        for(m in 1:nrow(offens.df)){
          catch.jm <- catch[catch$jerseyNumber==offens.df[m,'jerseyNumber']&catch$displayName==offens.df[m,'displayName'],
                            c('x','y','frame.id')]
          temp.dist <- data.frame(merge(x=fence.jd[,c('x','y','frame.id')],y=catch.jm,by='frame.id'),fdist=0)
          for(f in 1:nrow(temp.dist)){temp.dist[f,6] <- dist(t(matrix(temp.dist[f,2:5],2,2)),method='euclidean')}
          offens.df[m,4:ncol(offens.df)] <- temp.dist$fdist
        }
        
        #Backfield Indicator (defenders behind ball)
        defend.df[d,11] <- ifelse(nrow(ball.jd)>0,
                                  as.numeric(sign(fence.jd[fence.jd$frame.id==snap.j,'x']-ball[ball$frame.id==snap.j,'x'])*
                                               sign(fence.jp$x-ball[ball$frame.id==pass.j,'x'])==1),NA)
        alldist.df <- rbind(alldist.df,offens.df)
      }
      
      #Minimum Distance for ALL Receveivers (ALL Frames)
      mindist.df <- data.frame(players.j,matrix(0,1,frames.j)); alldist.df <- alldist.df[-1,]
      for(l in 1:nrow(mindist.df)){
        player.temp <- alldist.df[alldist.df$displayName==mindist.df[l,'displayName']&
                                    alldist.df$jerseyNumber==mindist.df[l,'jerseyNumber'],]
        for(f in 4:ncol(mindist.df)){mindist.df[l,f] <- min(player.temp[,f])}
      }
      
      #Minimum Path Distance from WRs
      defend.df[,7] <- mean(rowMeans(mindist.df[mindist.df$PositionAbbr=='WR',4:ncol(mindist.df)]))
      
      #Minimum Path Distance from TEs
      defend.df[,8] <- mean(rowMeans(mindist.df[mindist.df$PositionAbbr=='TE',4:ncol(mindist.df)]))
      
      #Minimum Path Distance from RBs
      defend.df[,9] <- mean(rowMeans(mindist.df[mindist.df$PositionAbbr=='RB',4:ncol(mindist.df)]))
      
      #Cumulative Distance from ALL Recivers
      defend.df[,10] <- sum(rowMeans(mindist.df[,4:ncol(mindist.df)]))
      
      #Receiver Stats
      receiv.df <- data.frame(gameId=play.j$gameId,playId=play.j$playId,players.j,
                              speed=0,b.dist=0,min.dist=0,all.cdist=0,side.cdist=0,intend=0)
      for(k in 1:nrow(players.j)){
        catch.jk <- catch[catch$jerseyNumber==players.j[k,'jerseyNumber']&catch$displayName==players.j[k,'displayName'],]
        catch.jk <- catch.jk[order(catch.jk$frame.id),]
        catch.jp <- catch.jk[catch.jk$frame.id==pass.j,c('x','y')]
        
        #Yards/frame
        receiv.df[k,6] <- sum(catch.jk$dis)/frames.j
        
        #Distance from Ball
        ball.jk <- ball[ball$frame.id==pass.j,c('x','y')]
        receiv.df[k,7] <- ifelse(nrow(ball.jk)>0,dist(rbind(catch.jp,ball.jk),method='euclidean'),NA)
        
        #Distance from Closest Defender & Cumulative form ALL Defenders
        coverg.df <- data.frame(defend.df[,3:5],dist=0,cdist=0)
        for(n in 1:nrow(coverg.df)){
          fence.jn <- fence.j[fence.j$jerseyNumber==coverg.df[n,'jerseyNumber']&fence.j$displayName==coverg.df[n,'displayName'],
                              c('x','y','frame.id')]
          temp.dist <- data.frame(merge(x=catch.jk[,c('x','y','frame.id')],y=fence.jn,by='frame.id'),fdist=0)
          for(f in 1:nrow(temp.dist)){temp.dist[f,6] <- dist(t(matrix(temp.dist[f,2:5],2,2)),method='euclidean')}
          
          #Receiver Distance
          coverg.df[n,4] <- temp.dist[temp.dist$frame.id==pass.j,'fdist']
          
          #Cumulative Distance
          coverg.df[n,5] <- sum(temp.dist$fdist)
        }
        
        #Distance from Closest Defender
        receiv.df[k,8] <- min(coverg.df$dist)/frames.j
        
        #Cumulative Distance from ALL Defenders
        receiv.df[k,9] <- sum(coverg.df$cdist)/frames.j
        
        #Cumulative Sideline Distance
        receiv.df[k,10] <- sum(unlist(lapply(53.3-catch.jk$y,catch.jk$y,FUN=min)))/frames.j
      }
      receiv.df$intend <- as.numeric(receiv.df$b.dist==min(receiv.df$b.dist))
    }
    
    #Merge Tables
    defend.mat <- rbind(defend.mat,defend.df)
    receiv.mat <- rbind(receiv.mat,receiv.df)
  }
  
  #Output
  return(list(defend=defend.mat,receiv=receiv.mat))
}
