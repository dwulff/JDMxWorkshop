

cols0<-c(rgb(t(col2rgb("coral2")),maxColorValue=255,alpha=255),
         rgb(t(col2rgb("yellow3")),maxColorValue=255,alpha=255),
         rgb(t(col2rgb("seagreen")),maxColorValue=255,alpha=255))

cols1<-c(rgb(t(col2rgb("coral2")),maxColorValue=255,alpha=100),
         rgb(t(col2rgb("yellow3")),maxColorValue=255,alpha=100),
         rgb(t(col2rgb("seagreen")),maxColorValue=255,alpha=100))

cols2<-c(rgb(t(col2rgb("coral2")),maxColorValue=255,alpha=170),
         rgb(t(col2rgb("yellow3")),maxColorValue=255,alpha=170),
         rgb(t(col2rgb("seagreen")),maxColorValue=255,alpha=170))

black.t = rgb(0,0,0,maxColorValue = 255, alpha = 170)

dist = function(p) (p[1]-p[2]) / sqrt(2)

findIntersect = function(p) {
  
  a1 = 1
  b1 = -1
  c1 = 0
  
  a2 = -1
  b2 = -1
  c2 = -p[1] - p[2]
  
  xs = (c1*b2-c2*b1)/(a1*b2-a2*b1)
  ys = (a1*c2-a2*c1)/(a1*b2-a2*b1)
  
  return(c(xs,ys))
  }

outline = function(x,y,labels = 'test',col = 'black',bg = 'white',font=1,r=0.02,h = 1,w = 1,cex = 1,adj=.5){
  is = seq(0, 2*pi, length=72)
  for(i in is){
    xn = x+cos(i)*r*w
    yn = y+sin(i)*r*h
    text(xn,yn,labels = labels,col=bg,cex=cex,adj=adj)
  }
  text(x,y,labels = labels,col=col,cex=cex,adj=adj,font=font)
}


fSUM   = function(par) SUMf(get('s',.GlobalEnv),get('d',.GlobalEnv),par)
fitSUM = function(start) {
  a = nlminb(start,fSUM,scale=1,lower=c(.0001,.01),upper=c(10000,1000),control=list(maxit=10000000));
  b = a$message;
  res = c(a$objective,as.numeric(substr(b,nchar(b)-1,nchar(b)-1)))
  return(res)
  }


fVUM = function(par) VUMf(get('s',.GlobalEnv),get('d',.GlobalEnv),par[1],par[2])
fitVUM = function(start){
  a = nlminb(start,fVUM,scale=1,lower=c(.0001,.01),upper=c(2,1000),control=list(maxit=10000000));
  b = a$message;
  res = c(a$objective,as.numeric(substr(b,nchar(b)-1,nchar(b)-1)))
  return(res)
}  

fSWIM = function(par,win) SWIMf(get('s',.GlobalEnv),get('d',.GlobalEnv),win,par)
fitSWIM = function(start){
  s    = get('s',.GlobalEnv)
  ls   = length(s[[1]])
  mres = matrix(NA,ncol=2,nrow=ls)
  for(i in 1:ls){
    a = nlminb(start,fSWIM,win = i,scale=1,lower=c(.01),upper=c(1000),control=list(maxit=10000000)); 
    b=a$message;
    res = c(a$objective,as.numeric(substr(b,nchar(b)-1,nchar(b)-1)))
    mres[i,] = res
  }
  sel = which(mres[,1]==min(mres[,1]))
  return(mres[sel[sample(length(sel))[1]],])
}

complex = c(1,1,1)

run = function(type,no,n,sd,np){
  
  pr  <<- problemGenerator(np,no)
  s   <<- sampl(pr,n)
  
  if(type == 'SUM vs. VUM'){
    phi <<- ifelse(runif(1,0,1) > .5,exp(runif(1,log(1),log(10000))),rbeta(1,3,1))
    d   <<- SUMpr(s,sd)
    a1 = fitSUM(sd*10)
    b1 = fitVUM(c(1,sd*10))
    d  <<- VUMpr(s,phi,sd)
    a2 <<- fitVUM(c(phi,sd*10))
    b2 <<- fitSUM(sd*10)
    }
  if(type == 'SUM vs. SWIM'){
    zt <<- sample(1:n,1)
    d  <<- SUMpr(s,sd)
    a1 = fitSUM(sd*10)
    b1 = fitSWIM(sd*10)
    d  <<- SWIMpr(s,zt,sd)
    a2 <<- fitSWIM(sd*10)
    b2 <<- fitSUM(sd*10)
    }
  if(type == 'VUM vs. SWIM'){
    phi <<- ifelse(runif(1,0,1) > .5,exp(runif(1,log(1),log(10000))),rbeta(1,3,1))
    zt <<- sample(1:n,1)
    d  <<- VUMpr(s,phi,sd)
    a1 = fitVUM(c(phi,sd*10))
    b1 = fitSWIM(sd*10)
    d  <<- SWIMpr(s,zt,sd)
    a2 <<- fitSWIM(sd*10)
    b2 <<- fitVUM(c(.5,sd*10))
    }
  return(c(get('a1',.GlobalEnv),get('b1',GlobalEnv),get('a2',.GlobalEnv),get('b2',.GlobalEnv)))
}


runm = function(nm,type,no,n,sd,np){
  
  res = matrix(NA,nrow=nm,ncol=8)
  
  for(i in 1:nm){
  
  pr  <<- problemGenerator(np,no)
  s   <<- sampl(pr,n)
  
  if(type == 'SUM vs. VUM'){
    phi <<- ifelse(runif(1,0,1) > .5,exp(runif(1,log(1),log(10000))),rbeta(1,3,1))
    d   <<- SUMpr(s,sd)
    a1 = fitSUM(sd*10)
    b1 = fitVUM(c(1,sd*10))
    d  <<- VUMpr(s,phi,sd)
    a2 = fitVUM(c(phi,sd*10))
    b2 = fitSUM(sd*10)
  }
  if(type == 'SUM vs. SWIM'){
    zt <<- sample(1:n,1)
    d  <<- SUMpr(s,sd)
    a1 = fitSUM(sd*10)
    b1 = fitSWIM(sd*10)
    d  <<- SWIMpr(s,zt,sd)
    a2 = fitSWIM(sd*10)
    b2 = fitSUM(sd*10)
  }
  if(type == 'VUM vs. SWIM'){
    phi <<- ifelse(runif(1,0,1) > .5,exp(runif(1,log(1),log(10000))),rbeta(1,3,1))
    zt <<- sample(1:n,1)
    d  <<- VUMpr(s,phi,sd)
    a1 = fitVUM(c(phi,sd*10))
    b1 = fitSWIM(sd*10)
    d  <<- SWIMpr(s,zt,sd)
    a2 = fitSWIM(sd*10)
    b2 = fitVUM(c(.5,sd*10))
  }
  
  res[i,] = c(a1,b1,a2,b2)
  
  }
  return(res)
}





