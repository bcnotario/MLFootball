#Route Characteristics for ALL Plays

route.pre <- function(){
  
  #Read Player & Play Files
  players <- read.csv('~/Documents/Projects/NFL/players.csv',header=T)
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  
  #Remove Penalties, Special Teams Plays, Running Plays & 2pt conversions
  plays <- plays[plays$isPenalty==F&plays$isSTPlay==F&!is.na(plays$PassLength),]
  
  #Remove Spikes
  plays <- plays[-which(regexpr('spike',plays$playDescription)>0),]
  
  #Route Matrix
  temp.mat <- data.frame(pos=NA,jersey=NA,nflId=0,topbot=0,dir=0,stem=0,angle=0,branch=0,conv.x=0,conv.y=0,sigma=0,model=0)
  temp.mat <- cbind(data.frame(gameId=0,playId=0),temp.mat,temp.mat,temp.mat,temp.mat,temp.mat,temp.mat,temp.mat)
  
  #Loop Through All Plays
  for(j in 1:nrow(plays)){
    
    #Play Subset
    play.j <- plays[j,c('gameId','playId')]
    track.file <- play.dat0[[as.character(play.j$gameId)]]
    
    #All Players
    track.j <- merge(x=track.file[track.file$playId==play.j$playId,],y=players,by='nflId')[,c(1:14,17)]
    
    #If No Tracking Data Available for Play
    if(nrow(track.j)==0 | nrow(track.j[track.j$PositionAbbr=='QB'&!is.na(track.j$event)&track.j$event=='ball_snap',])==0){
      temp.mat <- temp.mat
    } else {
      
      #WR/TE/RB Full Routes & Players
      route.j <- track.j[track.j$PositionAbbr=='WR'|track.j$PositionAbbr=='TE'|track.j$PositionAbbr=='RB',]
      players.j <- unique(route.j[order(route.j$PositionAbbr,decreasing=T),
                                  c('PositionAbbr','displayName','jerseyNumber','nflId')])
      
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
      
      #WR/TE/RB Snap2Catch Routes
      catch.j <- route.j[route.j$frame.id>=snap.j&route.j$frame.id<=pass.j,]
      
      #Route Characteristics per Play
      temp.mat <- rbind(temp.mat,cbind(play.j,route.class(play.j,catch.j,players.j,pass.j,snap.j,track.file)))
    }
  }
  
  #Merge Columns into Table
  temp.mat2 <- rbind(temp.mat[,1:14],temp.mat[,c(1:2,15:26)],temp.mat[,c(1:2,27:38)],temp.mat[,c(1:2,39:50)],
                     temp.mat[,c(1:2,51:62)],temp.mat[,c(1:2,63:74)],temp.mat[,c(1:2,75:86)])
  temp.mat2 <- temp.mat2[!is.na(temp.mat2$pos),]
  
  #Output
  return(list(route.wr=temp.mat2[temp.mat2$pos=='WR',],route.te=temp.mat2[temp.mat2$pos=='TE',],
              route.rb=temp.mat2[temp.mat2$pos=='RB',]))
}
