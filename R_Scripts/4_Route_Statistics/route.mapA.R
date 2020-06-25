#Route Display by Play

route.mapA <- function(j,defense){
  
  #Player & Play Files
  players <- read.csv('~/Documents/Projects/NFL/players.csv',header=T)
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  plays <- plays[plays$isPenalty==F&plays$isSTPlay==F&!is.na(plays$PassLength),]
  plays <- plays[-which(regexpr('spike',plays$playDescription)>0),]
  
  #Play Subset
  play <- plays[j,c('gameId','playId','yardlineNumber','yardsToGo')]
  track.ls <- play.dat0[[as.character(play$gameId)]]
  track.j <- merge(x=track.ls[track.ls$playId==play$playId,],y=players,by='nflId')[,c(1:14,17)]
  route.j <- track.j[track.j$PositionAbbr=='WR'|track.j$PositionAbbr=='TE'|track.j$PositionAbbr=='RB',]
  players.j <- unique(route.j[order(route.j$PositionAbbr,decreasing=T),c('PositionAbbr','displayName','jerseyNumber')])
  route.qb <- track.j[track.j$PositionAbbr=='QB',]
  event.j <- unique(route.qb[order(route.qb$frame.id),c('frame.id','event')])
  event.j <- event.j[which(!is.na(event.j$event)),]
  snap <- max(event.j[event.j$event=='ball_snap'|event.j$event=='man_in_motion','frame.id'])
  thrw <- max(event.j[event.j$event=='pass_forward','frame.id']) #Not currently used
  pass <- max(event.j[event.j$event=='pass_outcome_caught'|event.j$event=='pass_outcome_incomplete'|
                          event.j$event=='pass_outcome_touchdown'|event.j$event=='pass_outcome_interception'|
                          event.j$event=='pass_arrived'|event.j$event=='pass_shovel'|event.j$event=='pass_tipped','frame.id'])
  pass <- ifelse(pass==Inf|pass==-Inf,thrw,pass)
  catch <- route.j[route.j$frame.id>=snap&route.j$frame.id<=pass,]
  ball <- track.ls[track.ls$playId==play$playId&track.ls$team=='ball'&track.ls$frame.id<=pass,]
  b.sign <- sign(ball[1,'x']-mean(catch[catch$frame.id==snap,'x']))
  
  #Plot Range
  if(defense==T){field.x <- range(track.j[track.j$team!='ball','x'])} else {field.x <- range(c(catch$x,route.qb$x))}
  
  #Field
  plot(catch$x~catch$y,ylim=field.x,xlim=c(0,53.3),col='white',ylab='Sideline',xlab='Endzone',
       yaxt='n',main=paste0('Game.ID=',play[1],', Play.ID=',play[2]))
  axis(side=2,at=seq(0,120,10),labels=c('EZ',0,10,20,30,40,50,40,30,20,10,0,'EZ'))
  axis(side=4,at=seq(0,120,10),labels=c('EZ',0,10,20,30,40,50,40,30,20,10,0,'EZ'))
  rect(-5,field.x[1]-5,53.3+5,field.x[2]+5,col='springgreen3')
  abline(h=seq(0,120,5),lty=1,col='white')
  points(seq(10,110,1)~rep(.5,101),pch=95,col='white')
  points(seq(10,110,1)~rep(53.3-.5,101),pch=95,col='white')
  points(seq(10,110,1)~rep(53.3/2-5,101),pch=95,col='white')
  points(seq(10,110,1)~rep(53.3/2+5,101),pch=95,col='white')
  
  #Line of Scrimmage & 1st Down Marker
  abline(h=c(ball$x[1],ball$x[1]+b.sign*play$yardsToGo),lty=1,lwd=4,col=c('blue','orange'))
  
  #Routes
  points(catch$x~catch$y,pch=19,col='white')
  points(catch[catch$frame.id==pass,'x']~catch[catch$frame.id==pass,'y'],col='white',pch=8,cex=3,lwd=3)
  
  #Ball
  points(ball$x~ball$y,pch=19,cex=2,col='brown'); lines(ball$x~ball$y,lty=2,col='brown')
  
  #Receiver Fitted Routes
  for(k in 1:nrow(players.j)){
    catch.jk <- catch[catch$jerseyNumber==players.j[k,'jerseyNumber']&catch$displayName==players.j[k,'displayName'],]
    catch.jk <- catch.jk[order(catch.jk$frame.id),]
    catch.x <- catch.jk$x; catch.y <- catch.jk$y; catch.n <- length(catch.x)
    
    #Segmented/Polynomial/Linear Regression Models
    model.ls <- list()
    model.ls[[1]] <- lm(catch.y~catch.x)
    model.ls[[2]] <- lm(catch.x~catch.y)
    model.ls[[3]] <- tryCatch(segmented(obj=lm(catch.y~catch.x),seg.Z=~catch.x,psi=median(catch.x)),
                              error=function(e){lm(catch.y~catch.x)})
    model.ls[[4]] <- tryCatch(segmented(obj=lm(catch.x~catch.y),seg.Z=~catch.y,psi=median(catch.y)),
                              error=function(e){lm(catch.x~catch.y)})
    
    #Rotate Coordinates 45 deg
    catch.x <- (catch.jk$x-catch.jk$y)/sqrt(2); catch.y <- (catch.jk$y+catch.jk$x)/sqrt(2)
    model.ls[[5]] <- tryCatch(segmented(obj=lm(catch.y~catch.x),seg.Z=~catch.x,psi=median(catch.x)),
                              error=function(e){catch.x <- catch.jk$x; catch.y <- catch.jk$y; lm(catch.y~catch.x)})
    
    #Rotate Coordinates 135 deg
    catch.x <- (-catch.jk$x-catch.jk$y)/sqrt(2); catch.y <- (catch.jk$y-catch.jk$x)/sqrt(2)
    model.ls[[6]] <- tryCatch(segmented(obj=lm(catch.x~catch.y),seg.Z=~catch.y,psi=median(catch.y)),
                              error=function(e){catch.x <- catch.jk$x; catch.y <- catch.jk$y; lm(catch.x~catch.y)})
    
    #Change Coordinates Back
    catch.x <- catch.jk$x; catch.y <- catch.jk$y
    
    #Model Error
    model.df <- data.frame(mod=c('LM','LM','Seg','Seg','Rot','Rot'),input=c('x','y','x','y','x','y'),RSS=0)
    for(e in 1:6){model.df[e,3] <- summary(model.ls[[e]])$sigma}
    
    #Minimum Error Model
    model.n <- which(model.df$RSS==min(model.df$RSS))[1]
    
    #Linear Model
    if(model.df[model.n,'mod']=='LM'){
      branch.l <- 0
      angle <- atan(lm(catch.x~catch.y)$coef[2])*180/pi
      branch.x <- c(NA,NA)
      branch.y <- c(NA,NA)
      if(model.df[model.n,'input']=='x'){
        stem.x <- c(catch.x[1],catch.x[catch.n])
        stem.y <- c(model.ls[[model.n]]$fitted.values[1],model.ls[[model.n]]$fitted.values[catch.n])
        stem.l <- (as.numeric(catch.jk$x[1]<snap)-.5)*2*(stem.x[2]-stem.x[1])
      } else {
        stem.x <- c(model.ls[[model.n]]$fitted.values[1],model.ls[[model.n]]$fitted.values[catch.n])
        stem.y <- c(catch.y[1],catch.y[catch.n])
        stem.l <- (as.numeric(catch.jk$x[1]<snap)-.5)*2*(stem.x[2]-stem.x[1])
      }
      
      #Segment Model
    } else if (model.df[model.n,'mod']=='Seg') {
      if(model.df[model.n,'input']=='x'){
        stem.x <- c(catch.x[1],model.ls[[model.n]]$psi[2])
        stem.y <- predict(model.ls[[model.n]],newdata=data.frame(catch.x=stem.x))
        branch.x <- c(model.ls[[model.n]]$psi[2],catch.x[catch.n])
        branch.y <- predict(model.ls[[model.n]],newdata=data.frame(catch.x=branch.x))
      } else {
        stem.y <- c(catch.y[1],model.ls[[model.n]]$psi[2])
        stem.x <- predict(model.ls[[model.n]],newdata=data.frame(catch.y=stem.y))
        branch.y <- c(model.ls[[model.n]]$psi[2],catch.y[catch.n])
        branch.x <- predict(model.ls[[model.n]],newdata=data.frame(catch.y=branch.y))
      }
      stem.l <- (as.numeric(catch.jk$x[1]<snap)-.5)*2*(stem.x[2]-stem.x[1])
      angle <- atan(lm(branch.x~branch.y)$coef[2])*180/pi
      branch.l <- dist(cbind(branch.x,branch.y),method='euclidean')
      
      #Rotation Model
    } else if (model.df[model.n,'mod']=='Rot') {
      if(model.df[model.n,'input']=='x'){
        
        #Rotate Coordinates 45 deg
        catch.x <- (catch.jk$x-catch.jk$y)/sqrt(2); catch.y <- (catch.jk$y+catch.jk$x)/sqrt(2)
        stem.rx <- c(catch.x[1],model.ls[[model.n]]$psi[2])
        stem.ry <- predict(model.ls[[model.n]],newdata=data.frame(catch.x=stem.rx))
        branch.rx <- c(model.ls[[model.n]]$psi[2],catch.x[catch.n])
        branch.ry <- predict(model.ls[[model.n]],newdata=data.frame(catch.x=branch.rx))
        
        #Rotate Coordinates Back
        stem.x <- (stem.rx+stem.ry)/sqrt(2)
        stem.y <- (-stem.rx+stem.ry)/sqrt(2)
        branch.x <- (branch.rx+branch.ry)/sqrt(2)
        branch.y <- (-branch.rx+branch.ry)/sqrt(2)
      } else {
        
        #Rotate Coordinates 135 deg
        catch.x <- (-catch.jk$x-catch.jk$y)/sqrt(2); catch.y <- (catch.jk$y-catch.jk$x)/sqrt(2)
        stem.ry <- c(catch.y[1],model.ls[[model.n]]$psi[2])
        stem.rx <- predict(model.ls[[model.n]],newdata=data.frame(catch.y=stem.ry))
        branch.ry <- c(model.ls[[model.n]]$psi[2],catch.y[catch.n])
        branch.rx <- predict(model.ls[[model.n]],newdata=data.frame(catch.y=branch.ry))
        
        #Rotate Coordinates Back
        stem.x <- (-stem.rx-stem.ry)/sqrt(2)
        stem.y <- (-stem.rx+stem.ry)/sqrt(2)
        branch.x <- (-branch.rx-branch.ry)/sqrt(2)
        branch.y <- (-branch.rx+branch.ry)/sqrt(2)
      }
      stem.l <- (as.numeric(catch.jk$x[1]<snap)-.5)*2*(stem.x[2]-stem.x[1])
      angle <- atan(lm(branch.x~branch.y)$coef[2])*180/pi
      branch.l <- dist(cbind(branch.x,branch.y),method='euclidean')
    }
    
    #Plot Segments
    points(c(stem.x,branch.x)~c(stem.y,branch.y),pch=19,col='blue')
    lines(c(stem.x,branch.x)~c(stem.y,branch.y),lwd=2,col='blue')
  }
  
  #Defender Distance
  if(defense==T){
    fence.j <- track.j[track.j$team==ifelse(unique(route.j$team)[1]=='home','away','home')&
                         track.j$frame.id>=snap&track.j$frame.id<=pass,]
    defend.df <- data.frame(unique(fence.j[order(fence.j$PositionAbbr),
                                           c('PositionAbbr','displayName','jerseyNumber')]),b.dist=0,wr.dist=0,all.dist=0)
    points(fence.j[fence.j$frame.id==pass,'x']~fence.j[fence.j$frame.id==pass,'y'],pch=4,lwd=3)
    for(d in 1:nrow(defend.df)){
      lines(c(fence.j[fence.j$frame.id==pass&fence.j$jerseyNumber==defend.df[d,'jerseyNumber'],'x'],
              ball[ball$frame.id==pass,'x'])~
              c(fence.j[fence.j$frame.id==pass&fence.j$jerseyNumber==defend.df[d,'jerseyNumber'],'y'],
                ball[ball$frame.id==pass,'y']),lty=3)
    }
  }
  
  #Player Identifiers
  points(catch[catch$PositionAbbr=='WR'&catch$frame.id==pass,'x']~
           catch[catch$PositionAbbr=='WR'&catch$frame.id==pass,'y'],
         pch=21,cex=3,col='black',bg='white')
  points(catch[catch$PositionAbbr=='RB'&catch$frame.id==pass,'x']~
           catch[catch$PositionAbbr=='RB'&catch$frame.id==pass,'y'],
         pch=22,cex=3,col='black',bg='white')
  points(catch[catch$PositionAbbr=='TE'&catch$frame.id==pass,'x']~
           catch[catch$PositionAbbr=='TE'&catch$frame.id==pass,'y'],
         pch=24,cex=3,col='black',bg='white')
  for(k in 1:nrow(players.j)){
    text(catch[catch$jerseyNumber==players.j[k,'jerseyNumber']&catch$displayName==players.j[k,'displayName']&
                 catch$frame.id==pass,c('y','x')],label=players.j[k,'jerseyNumber'],cex=.75)
  }
}
