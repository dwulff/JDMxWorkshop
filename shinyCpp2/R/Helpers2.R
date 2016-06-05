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


gen.p = function(np,n) {a = problemGenerator(np,n,n); transformProblems(a)} 

fTK = function(par) cpt_lik_tk(par,get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
fitTK = function(start){
  a = nlminb(start,fTK,lower=c(.01,.01),upper=c(100,100),control=list(eval.max=1000,iter.max=1000))
  return(a$objective)
}

fGE = function(par) cpt_lik_ge(par,get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
fitGE = function(start){
  a = nlminb(start,fGE,lower=c(.01,.01,.01),upper=c(100,100,100),control=list(eval.max=1000,iter.max=1000))
  return(a$objective)
}

fP = function(par) cpt_lik_p(par,get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
fitP = function(start){
  a = nlminb(start,fP,lower=c(.01,.01,.01),upper=c(100,100,100),control=list(eval.max=1000,iter.max=1000))
  return(a$objective)
}


run = function(type,no,sens,np){
  
  pr  <<- gen.p(np,no)
  
  if(type == 'Tversky-Kahneman. vs. Goldstein-Einhorn'){
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_tk(c(gamma,sens),get('pr'))
    a1 <<- fitTK(c(gamma,sens/100))
    b1 <<- fitGE(c(1,gamma,sens/100))
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_ge(c(delta,gamma,sens),get('pr'))
    a2 <<- fitGE(c(delta,gamma,sens/100))
    b2 <<- fitTK(c(gamma,sens/100))
  }
  
  if(type == 'Tversky-Kahneman vs. Prelec'){
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_tk(c(gamma,sens),get('pr'))
    a1 <<- fitTK(c(gamma,sens/100))
    b1 <<- fitP(c(1,gamma,sens/100))
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_p(c(delta,gamma,sens),get('pr'))
    a2 <<- fitP(c(delta,gamma,sens/100))
    b2 <<- fitTK(c(gamma,sens/100))
  }
  
  if(type == 'Goldstein-Einhorn vs. Prelec'){
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_ge(c(delta,gamma,sens),get('pr'))
    a1 <<- fitGE(c(delta,gamma,sens/100))
    b1 <<- fitP(c(delta,gamma,sens/100))
    gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
    ch <<-  cpt_randchoice_p(c(delta,gamma,sens),get('pr'))
    a2 <<- fitP(c(delta,gamma,sens/100))
    b2 <<- fitGE(c(delta,gamma,sens/100))
  }
  return(c(get('a1',.GlobalEnv),get('b1',.GlobalEnv),get('a2',.GlobalEnv),get('b2',.GlobalEnv)))
}


runm = function(nm,type,no,sens,np){
  
  res = matrix(NA,nrow=nm,ncol=8)
  
  for(i in 1:nm){
    
    pr  <<- gen.p(np,no)
    
    if(type == 'Tversky-Kahneman. vs. Goldstein-Einhorn'){
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_tk(c(gamma,sens),get('pr'))
      a1 <<- fitTK(c(gamma,sens/100))
      b1 <<- fitGE(c(1,gamma,sens/100))
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_ge(c(delta,gamma,sens),get('pr'))
      a2 <<- fitGE(c(delta,gamma,sens/100))
      b2 <<- fitTK(c(gamma,sens/100))
    }
    
    if(type == 'Tversky-Kahneman vs. Prelec'){
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_tk(c(gamma,sens),get('pr'))
      a1 <<- fitTK(c(gamma,sens/100))
      b1 <<- fitP(c(1,gamma,sens/100))
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_p(c(delta,gamma,sens),get('pr'))
      a2 <<- fitP(c(delta,gamma,sens/100))
      b2 <<- fitTK(c(gamma,sens/100))
    }
    
    if(type == 'Goldstein-Einhorn vs. Prelec'){
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_ge(c(delta,gamma,sens),get('pr'))
      a1 <<- fitGE(c(delta,gamma,sens/100))
      b1 <<- fitP(c(delta,gamma,sens/100))
      gamma = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      delta = ifelse(runif(1,0,1)<.5,runif(1,.01,1),runif(1,1,100))
      ch <<-  cpt_randchoice_p(c(delta,gamma,sens),get('pr'))
      a2 <<- fitP(c(delta,gamma,sens/100))
      b2 <<- fitGE(c(delta,gamma,sens/100))
    }
    
    res[i,] = c(get('a1',.GlobalEnv),get('b1',.GlobalEnv),get('a2',.GlobalEnv),get('b2',.GlobalEnv))
    
  }
  return(res)
}


# t = proc.time()[3]
# 
# for(i in 1:1000) cpt_lik_ge(c(1,1,1),get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
# for(i in 1:1000) cpt_lik_p(c(1,1,1),get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
# for(i in 1:1000) cpt_lik_tk(c(1,1,1),get('pr',.GlobalEnv),get('ch','.GlobalEnv'))
# 
# proc.time()[3] - t


