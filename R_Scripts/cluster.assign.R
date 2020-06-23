#Assign Route Types w/route.pre data

cluster.assign <- function(data){
  
  #Route Table
  route.table <- data.frame(alpha=c('A','B','C','D','E','F','G','H','D','I','J','J','K','L','M','L','M','M','K'),
                            route=c('Comeback','Out','Corner','Go','Post','Dig','Curl',
                                    'Flat','Go','Slant','Hitch','Hitch','Block',
                                    'Flare','Other','Flare','Other','Other','Block'),
                            angle0=c(0,80,100,170,190,260,280,45,160,200,315,0,0,45,135,225,315,0,0),
                            angle1=c(80,100,170,190,260,280,360,160,200,315,360,45,360,135,225,315,360,45,360),
                            yard0=c(7.5,7.5,7.5,7.5,7.5,7.5,7.5,1,1,1,1,1,0,-50,-50,-50,-50,-50,-1),
                            yard1=c(120,120,120,120,120,120,120,7.5,7.5,7.5,7.5,7.5,1,-1,-1,-1,-1,-1,0),
                            branch1=c(0,0,0,0,0,0,0,2,2,2,0,0,0,2,2,2,2,2,0),
                            color.r=c(0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,0),
                            color.g=c(.25,.375,.50,.625,.75,.875,1,0,.625,.5,1,1,0,0,1,.5,1,1,0),
                            color.b=c(1,1,1,1,1,1,1,0,1,0,0,0,0,.5,0,.5,0,0,0))
  
  #Combine WR/RB/TE Tables
  temp <- rbind(data$route.wr,data$route.te,data$route.rb)
  temp <- temp[!is.na(temp$angle)&!is.na(temp$branch),]
  
  #Route Assignment
  temp <- cbind(temp,data.frame(alpha=NA,route=NA,col.r=NA,col.g=NA,col.b=NA))
  for(j in 1:nrow(temp)){
    alpha.temp <- as.character(route.table[route.table$angle0<=temp$angle[j] & route.table$angle1>temp$angle[j] &
                                   route.table$yard0<=temp$stem[j] & route.table$yard1>temp$stem[j],'alpha'])
    temp[j,15] <- ifelse(alpha.temp!='K'|temp$branch[j]<2,alpha.temp,
                         as.character(route.table[route.table$angle0<=temp$angle[j]&route.table$angle1>temp$angle[j]&
                                                    route.table$yard0<=sign(temp$stem[j])*2&
                                                    route.table$yard1>sign(temp$stem[j])*2,'alpha']))
    temp[j,16] <- as.character(route.table[temp$alpha[j]==route.table$alpha,'route'][1])
    temp[j,17] <- route.table[temp$alpha[j]==route.table$alpha,'color.r'][1]
    temp[j,18] <- route.table[temp$alpha[j]==route.table$alpha,'color.g'][1]
    temp[j,19] <- route.table[temp$alpha[j]==route.table$alpha,'color.b'][1]
  }
   
  #Subsets in Front/Behind Snap
  temp.pos <- temp[temp$stem>=0,]
  temp.neg <- temp[temp$stem<0,]

  #Classification Plots
  plot(-temp.pos$conv.x~-temp.pos$conv.y,col=rgb(temp.pos$col.r,temp.pos$col.g,temp.pos$col.b),xlim=c(-42,42),
       ylim=c(-40,40),main='Route Classification (Positive Yardage)',xlab='Sideline Direction',ylab='Endzone Direction')
  points(c(-35)~c(0),pch=16,cex=2); arrows(0,-35,0,35)
  arrows(-35,0,35,0,code=3); arrows(-25,-25,25,25,code=3); arrows(-25,25,25,-25,code=3)
  text(c(-40,-25,0,25,35,25,0,-25,3,3,-1)~c(-40,25,35,25,0,-25,-35,-25,-3,3,0),pos=c(3,1,4,3,3,3,2,1,2,4,1),
       labels=c('QB','Comeback','Out','Corner','Go','Post','Dig','Curl','Slant','Flat','Hitch'),cex=1.5)
  plot(-temp.neg$conv.x~-temp.neg$conv.y,col=rgb(temp.neg$col.r,temp.neg$col.g,temp.neg$col.b),xlim=c(-22,22),
       ylim=c(-22,22),main='Route Classification (Negative Yardage)',xlab='Sideline Direction',ylab='Endzone Direction')
  points(c(15)~c(0),pch=16,cex=2); arrows(0,-15,0,15,code=1); arrows(-15,0,15,0,code=3)
  text(c(15,0,15,0,-15)~c(-15,15,0,-15,0),pos=c(3,4,3,2,1),labels=c('QB','Wheel','Go/Block','Wheel','Other'),cex=1.5)
  
  #RSS Plots
  plot(y=-temp.pos$conv.x/abs(temp.pos$stem)*temp.pos$sigma,x=-temp.pos$conv.y/abs(temp.pos$stem)*temp.pos$sigma,
       col=rgb(temp.pos$col.r,temp.pos$col.g,temp.pos$col.b),xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),
       main='Residual Standard Error (Positive Yardage)',xlab='Sideline Direction',ylab='Endzone Direction')
  points(c(-2)~c(0),pch=16,cex=2); arrows(0,-2,0,2)
  arrows(-2,0,2,0,code=3); arrows(-1.5,-1.5,1.5,1.5,code=3); arrows(-1.5,1.5,1.5,-1.5,code=3)
  text(c(-2.5,-1.5,0,1.5,2,1.5,0,-2)~c(-2.5,1.5,2,1.5,0,-1.5,-2,-1.5),pos=c(3,1,4,3,3,3,2,1),
       labels=c('QB','Comeback','Out','Corner','Go','Post','Dig','Curl'),cex=1.5)
  plot(y=-temp.neg$conv.x/abs(temp.neg$stem)*temp.neg$sigma,x=-temp.neg$conv.y/abs(temp.neg$stem)*temp.neg$sigma,
       col=rgb(temp.neg$col.r,temp.neg$col.g,temp.neg$col.b),xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),
       main='Residual Standard Error (Negative Yardage)',xlab='Sideline Direction',ylab='Endzone Direction')
  points(c(1)~c(0),pch=16,cex=2); arrows(0,-1,0,1,code=1); arrows(-1,0,1,0,code=3)
  text(c(1,0,1,0,-1)~c(-1,1,0,-1,0),pos=c(3,4,3,2,1),labels=c('QB','Wheel','Go/Block','Wheel','Other'),cex=1.5)
  model.mat <- matrix(0,13,6); colnames(model.mat) <- 1:6; rownames(model.mat) <- LETTERS[1:13]
  for(a in 1:13){for(b in 1:6){model.mat[a,b] <- nrow(temp[temp$alpha==LETTERS[a]&temp$model==b,])}}
  bar <- barplot(model.mat[rev(1:10),c(3:6,1)],col=rev(rgb(unique(route.table[,c('color.r','color.g','color.b')]))[1:10]),
                 names.arg=c('Model 1','Model 2','Model 3','Model 4','Model 5'),ylim=c(0,max(colSums(model.mat[1:10,])[c(3:6,1)]+1000)))
  text(x=bar,y=colSums(model.mat[1:10,])[c(3:6,1)]+400,
       paste0(round(colSums(model.mat[1:10,])/sum(model.mat[1:10,]),3)*100,"%")[c(3:6,1)])
  
  #Output
  return(temp)
}
