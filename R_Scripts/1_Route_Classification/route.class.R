#Route Characteristics by Play (up to 7 players)

route.class <- function(play,catch,players,pass,snap,track.ls){

  #Route Characteristics Dataframe
  route.mat <- data.frame(pos=rep(NA,nrow(players)),jersey=rep(NA,nrow(players)),nflId=0,topbot=0,dir=0,
                          stem=0,angle=0,branch=0,conv.x=0,conv.y=0,sigma=0,model=0)
  
  #Individual Route
  for(k in 1:nrow(players)){
    
    #Player Snap2Catch Route
    catch.jk <- catch[catch$jerseyNumber==players[k,'jerseyNumber']&catch$displayName==players[k,'displayName'],]
    catch.jk <- catch.jk[order(catch.jk$frame.id),]
    catch.x <- catch.jk$x; catch.y <- catch.jk$y; catch.n <- length(catch.x)
    snap.x <- if(nrow(track.ls[track.ls$playId==play$playId&track.ls$team=='ball',])==0) {catch.x[1]} else {
      ifelse(is.na(track.ls[track.ls$playId==play$playId&track.ls$team=='ball'&track.ls$frame.id==1,'x']),
             catch.x[1],track.ls[track.ls$playId==play$playId&track.ls$team=='ball'&track.ls$frame.id==1,'x'])}
    snap.y <- if(nrow(track.ls[track.ls$playId==play$playId&track.ls$team=='ball',])==0) {53.3/2} else {
      ifelse(is.na(track.ls[track.ls$playId==play$playId&track.ls$team=='ball'&track.ls$frame.id==1,'y']),53.3/2,
             track.ls[track.ls$playId==play$playId&track.ls$team=='ball'&track.ls$frame.id==1,'y'])}
    dist <- sqrt((diff(range(catch.x)))^2+(diff(range(catch.y)))^2)
    
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
      branch.x <- c(0,0)
      branch.y <- c(0,0)
      if(model.df[model.n,'input']=='x'){
        stem.x <- c(catch.x[1],catch.x[catch.n])
        stem.y <- c(model.ls[[model.n]]$fitted.values[1],model.ls[[model.n]]$fitted.values[catch.n])
        stem.l <- (as.numeric(catch.jk$x[1]<snap.x)-.5)*2*(stem.x[2]-stem.x[1])
      } else {
        stem.x <- c(model.ls[[model.n]]$fitted.values[1],model.ls[[model.n]]$fitted.values[catch.n])
        stem.y <- c(catch.y[1],catch.y[catch.n])
        stem.l <- (as.numeric(catch.jk$x[1]<snap.x)-.5)*2*(stem.x[2]-stem.x[1])
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
      stem.l <- (as.numeric(catch.jk$x[1]<snap.x)-.5)*2*(stem.x[2]-stem.x[1])
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
      stem.l <- (as.numeric(catch.jk$x[1]<snap.x)-.5)*2*(stem.x[2]-stem.x[1])
      angle <- atan(lm(branch.x~branch.y)$coef[2])*180/pi
      branch.l <- dist(cbind(branch.x,branch.y),method='euclidean')
    }
    
    #Position
    route.mat[k,1] <- as.character(players[k,'PositionAbbr'])
    route.mat[k,2] <- players[k,'jerseyNumber']
    route.mat[k,3] <- players[k,'nflId']
    
    #Top (1) or Bottom (-1) wrt Ball at Snap
    route.mat[k,4] <- (as.numeric(catch.jk$y[1]>snap.y)-.5)*2
    
    #Field Direction Right(1) or Left(-1)
    route.mat[k,5] <- (as.numeric(catch.jk$x[1]<snap.x)-.5)*2
    
    #Route Stem Length
    route.mat[k,6] <- stem.l
    
    #Route Angle from Stem
    route.mat[k,7] <- ((route.mat[k,4]*(route.mat[k,5]*angle+(as.numeric(branch.y[2]>branch.y[1])-.5)*2*90)+360)) %% 360
    
    #Route Branch Length
    route.mat[k,8] <- branch.l
    
    #Route Polar to xy-Coordinates
    route.mat[k,9] <- abs(stem.l) * cos(route.mat[k,7]*pi/180)
    route.mat[k,10] <- abs(stem.l) * sin(route.mat[k,7]*pi/180)
    route.mat[k,11] <- model.df[model.n,'RSS']
    route.mat[k,12] <- model.n
  }
  
  #Output
  route <- cbind(route.mat[1,],route.mat[2,],route.mat[3,],route.mat[4,],route.mat[5,],route.mat[6,],route.mat[7,])
  return(route)
}
