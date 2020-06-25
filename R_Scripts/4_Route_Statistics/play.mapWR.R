#Route Map by Route Combination (WR only)

play.mapWR <- function(alpha,rplay,copy.n){
  
  #Play Files (remove penalties, special teams, running plays, 2pt conversions, spikes)
  plays <- read.csv('~/Documents/Projects/NFL/plays.csv',header=T)
  plays <- plays[plays$isPenalty==F&plays$isSTPlay==F&!is.na(plays$PassLength),]
  plays <- plays[-which(regexpr('spike',plays$playDescription)>0),]
  
  #Filter Plays by Alpha Route
  rplay.a <- rplay[rplay$alpha.wr==alpha&!is.na(rplay$alpha.wr),]
  
  #Plot Using route.mapA()
  pdf(file=paste0('~/Downloads/',alpha,'.plots.pdf'),height=22,width=7)
  par(mfrow=c(5,1))
  
  #Loop through 5 plays
  for(m in 1:copy.n){
    route.mapA(which(plays$gameId==rplay.a[m,'gameId']&plays$playId==rplay.a[m,'playId']),T)
    dist.txt <- round(rplay[rplay$gameId==rplay.a[m,'gameId']&rplay$playId==rplay.a[m,'playId'],
                            c('b.dist','cmpl.pred','yard.pred')],3)
    mtext(paste0('Avg Def Distance from Ball=',dist.txt[1],', Completion Probability=',dist.txt[2],
                 ', Expected Yardage=',dist.txt[3]),side=3,cex=.75)
  }
  dev.off()
}
