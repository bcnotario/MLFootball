#Route Overlay

cluster.plot <- function(cluster.new){
  
  #Read Player & Play Files
  players <- read.csv('~/Documents/Projects/NFL/players.csv',header=T)
  
  #Route Table
  alpha.df <- data.frame(alpha=c('A','B','C','D','E','F','G','H','I','J','K','L','M'),
                         route=c('Comeback','Out','Corner','Go','Post','Dig','Curl','Flat','Slant',
                                 'Hitch','Block','Flare','Other'))
  
  #Route List
  alpha.ls <- list()
  
  #Plot Each Route
  for(a in alpha.df$alpha){
    
    #Route Subset
    cluster.a <- cluster.new[cluster.new$alpha==a,]
    
    #All Routes
    route.temp <- data.frame(x=0,y=0,frame.id=0,gameId=0,playId=0,nflId=0,jerseyNumber=0,PositionAbbr=NA,x2=0,y2=0)
    for(j in 1:nrow(cluster.a)){
      
      #Game & Play File
      track.file <- play.dat0[[as.character(cluster.a[j,'gameId'])]]
      track.j <- merge(x=track.file[track.file$playId==cluster.a[j,'playId'],],y=players,by='nflId')[,c(1:14,17)]
      
      #If No Tracking Data Available for Play
      if(nrow(track.j)==0 | nrow(track.j[track.j$PositionAbbr=='QB'&!is.na(track.j$event)&track.j$event=='ball_snap',])==0){
        route.temp <- route.temp
      } else {
        
        #Play Events by QB
        route.qb <- track.j[track.j$PositionAbbr=='QB',]
        event.j <- unique(route.qb[order(route.qb$frame.id),c('frame.id','event')])
        event.j <- event.j[which(!is.na(event.j$event)),]
        snap.j <- max(event.j[event.j$event=='ball_snap'|event.j$event=='man_in_motion','frame.id'])
        thrw.j <- max(event.j[event.j$event=='pass_forward','frame.id'])
        pass.j <- max(event.j[event.j$event=='pass_outcome_caught'|event.j$event=='pass_outcome_incomplete'|
                                event.j$event=='pass_outcome_touchdown'|event.j$event=='pass_outcome_interception'|
                                event.j$event=='pass_arrived'|event.j$event=='pass_shovel'|event.j$event=='pass_tipped','frame.id'])
        pass.j <- ifelse(pass.j==Inf|pass.j==-Inf,thrw.j,pass.j)
        
        #Player Snap2Catch Route
        catch.j <- track.j[track.j$frame.id>=snap.j&track.j$frame.id<=pass.j&track.j$jerseyNumber==cluster.a[j,'jersey']&
                             track.j$PositionAbbr==cluster.a[j,'pos']&track.j$nflId==cluster.a[j,'nflId'],
                           c('x','y','frame.id','gameId','playId','nflId','jerseyNumber','PositionAbbr')]
        catch.j <- catch.j[order(catch.j$frame.id),]
        
        #Route Standardization
        catch.j$x2 <- (catch.j$x-catch.j$x[1])*cluster.a[j,'dir']
        catch.j$y2 <- (catch.j$y-catch.j$y[1])*cluster.a[j,'topbot']
        route.temp <- rbind(route.temp,catch.j)
      }
    }
    
    #Store Route
    alpha.ls[[which(alpha.df$alpha==a)]] <- route.temp
  }
  
  #Plots
  par(mfrow=c(4,3),mar=c(2,2,2,1) + 0.1)
  smoothScatter(alpha.ls[[5]]$x2~alpha.ls[[5]]$y2,pch=NA,ylim=c(-1,50),xlim=c(-30,15),xlab='',ylab='',main=alpha.df$route[5],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[4]]$x2~alpha.ls[[4]]$y2,pch=NA,ylim=c(-1,50),xlim=c(-20,20),xlab='',ylab='',main=alpha.df$route[4],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[3]]$x2~alpha.ls[[3]]$y2,pch=NA,ylim=c(-1,50),xlim=c(-15,30),xlab='',ylab='',main=alpha.df$route[3],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[6]]$x2~alpha.ls[[6]]$y2,pch=NA,ylim=c(-1,25),xlim=c(-30,15),xlab='',ylab='',main=alpha.df$route[6],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  
  plot(0~0,pch=NA,ylim=c(0,50),xlab='',ylab='',main='',axes=F,bty='n')
  smoothScatter(alpha.ls[[2]]$x2~alpha.ls[[2]]$y2,pch=NA,ylim=c(-1,25),xlim=c(-15,30),xlab='',ylab='',main=alpha.df$route[2],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[7]]$x2~alpha.ls[[7]]$y2,pch=NA,ylim=c(-1,25),xlim=c(-30,15),xlab='',ylab='',main=alpha.df$route[7],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  
  plot(0~0,pch=NA,ylim=c(0,50),xlab='',ylab='',main='',axes=F,bty='n')
  smoothScatter(alpha.ls[[1]]$x2~alpha.ls[[1]]$y2,pch=NA,ylim=c(-1,25),xlim=c(-15,30),xlab='',ylab='',main=alpha.df$route[1],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[9]]$x2~alpha.ls[[9]]$y2,pch=NA,ylim=c(-10,20),xlim=c(-30,15),xlab='',ylab='',main=alpha.df$route[9],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[10]]$x2~alpha.ls[[10]]$y2,pch=NA,ylim=c(-10,20),xlim=c(-20,20),xlab='',ylab='',main=alpha.df$route[10],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  smoothScatter(alpha.ls[[8]]$x2~alpha.ls[[8]]$y2,pch=NA,ylim=c(-10,20),xlim=c(-15,30),xlab='',ylab='',main=alpha.df$route[8],
                colramp=colorRampPalette(c('white','blue','dark blue'), space = "Lab"))
  
  #Output
  return(alpha.ls)
}
