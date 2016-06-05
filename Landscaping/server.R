shinyServer(function(input, output) {

  # +++++++++++ Preliminaries
      
  # Load packages    
  if(!require(snowfall,quietly = T)) install.packages('snowfall') ; require(snowfall)
  if(!require(devtools,quietly = T)) install.packages('devtools') ; require(devtools)
  if(!require(shinyCpp,quietly = T)) install_github('dwulff/JDMxWorkshop/shinyCpp')

  require(shinyCpp) # Note make available through gist  
  
##  Variables for testing
#    type = 'SUM vs. VUM'
#    method = 1
#    np = 10
#    no = 2
#    n = 10
#    dev = 10
#    nrun = 300
#    ncores = 1

  # +++++++++++ initialize objects
  
  empty = lapply(1:2,function(x) lapply(1:11,function(x) list(NA,NA)))
  
  l.dist = 0
  r.dist = 0
  l.prop = .5
  r.prop = .5
  
  lab = 'np'
  ticks = c()
  
  active = 'np'
  
  i.np  = 1
  i.no  = 1
  i.n   = 1
  i.dev = 1
  
  nps  = round(seq(10,50,4))
  nos  = round(seq(2,22,2))
  ns   = round(seq(2,22,2))
  devs = round(seq(2,22,2))
  
  r.nps   = empty
  r.nos   = empty
  r.ns    = empty
  r.devs  = empty
  plt.res = empty
  
  # ++++++++++++ Specify handlers 
  
  sel.np = reactive({
    input$Nproblem
    r.nps[[1]][[i.np]][[1]] <<- c(r.nps[[1]][[i.np]][[1]],l.dist)
    r.nps[[1]][[i.np]][[2]] <<- c(r.nps[[1]][[i.np]][[2]],r.dist)
    r.nps[[2]][[i.np]][[1]] <<- c(r.nps[[2]][[i.np]][[1]],l.prop)
    r.nps[[2]][[i.np]][[2]] <<- c(r.nps[[2]][[i.np]][[2]],r.prop)
    r.nos  <<- empty
    r.ns   <<- empty
    r.devs <<- empty
    plt.res <<- r.nps
    lab <<- 'Number of problems'
    ticks <<- nps
    active <<- 'np'
    })
  
  sel.no = reactive({
    input$Noutcome
    r.nos[[1]][[i.no]][[1]] <<- c(r.nos[[1]][[i.no]][[1]],l.dist)
    r.nos[[1]][[i.no]][[2]] <<- c(r.nos[[1]][[i.no]][[2]],r.dist)
    r.nos[[2]][[i.no]][[1]] <<- c(r.nos[[2]][[i.no]][[1]],l.prop)
    r.nos[[2]][[i.no]][[2]] <<- c(r.nos[[2]][[i.no]][[2]],r.prop)
    r.nps  <<- empty
    r.ns   <<- empty
    r.devs <<- empty
    plt.res <<- r.nos
    lab <<- 'Number of outcomes'
    ticks <<- nos
    active <<- 'no'
    })
  
  sel.n = reactive({
    input$Nsamples
    r.ns[[1]][[i.n]][[1]] <<- c(r.ns[[1]][[i.n]][[1]],l.dist)
    r.ns[[1]][[i.n]][[2]] <<- c(r.ns[[1]][[i.n]][[2]],r.dist)
    r.ns[[2]][[i.n]][[1]] <<- c(r.ns[[2]][[i.n]][[1]],l.prop)
    r.ns[[2]][[i.n]][[2]] <<- c(r.ns[[2]][[i.n]][[2]],r.prop)
    r.nps  <<- empty
    r.nos  <<- empty
    r.devs <<- empty
    plt.res <<- r.ns
    lab <<- 'Sample size'
    ticks <<- ns
    active <<- 'n'
    })
  
  sel.dev = reactive({
    input$Noise
    r.devs[[1]][[i.dev]][[1]] <<- c(r.devs[[1]][[i.dev]][[1]],l.dist)
    r.devs[[1]][[i.dev]][[2]] <<- c(r.devs[[1]][[i.dev]][[2]],r.dist)
    r.devs[[2]][[i.dev]][[1]] <<- c(r.devs[[2]][[i.dev]][[1]],l.prop)
    r.devs[[2]][[i.dev]][[2]] <<- c(r.devs[[2]][[i.dev]][[2]],r.prop)
    r.nps  <<- empty
    r.nos  <<- empty
    r.ns   <<- empty
    plt.res <<- r.devs
    lab <<- 'Noise level'
    ticks <<- devs
    active <<- 'dev'
    })
  
  rerun = reactive({
    input$Rerun
    if(active == 'np'){
      r.nps[[1]][[i.np]][[1]] <<- c(r.nps[[1]][[i.np]][[1]],l.dist)
      r.nps[[1]][[i.np]][[2]] <<- c(r.nps[[1]][[i.np]][[2]],r.dist)
      r.nps[[2]][[i.np]][[1]] <<- c(r.nps[[2]][[i.np]][[1]],l.prop)
      r.nps[[2]][[i.np]][[2]] <<- c(r.nps[[2]][[i.np]][[2]],r.prop)
      plt.res <<- r.nps
      }
    if(active == 'no'){
      r.nos[[1]][[i.no]][[1]] <<- c(r.nos[[1]][[i.no]][[1]],l.dist)
      r.nos[[1]][[i.no]][[2]] <<- c(r.nos[[1]][[i.no]][[2]],r.dist)
      r.nos[[2]][[i.no]][[1]] <<- c(r.nos[[2]][[i.no]][[1]],l.prop)
      r.nos[[2]][[i.no]][[2]] <<- c(r.nos[[2]][[i.no]][[2]],r.prop)
      plt.res <<- r.nos
      }
    if(active == 'n'){
      r.ns[[1]][[i.n]][[1]] <<- c(r.ns[[1]][[i.n]][[1]],l.dist)
      r.ns[[1]][[i.n]][[2]] <<- c(r.ns[[1]][[i.n]][[2]],r.dist)
      r.ns[[2]][[i.n]][[1]] <<- c(r.ns[[2]][[i.n]][[1]],l.prop)
      r.ns[[2]][[i.n]][[2]] <<- c(r.ns[[2]][[i.n]][[2]],r.prop)
      plt.res <<- r.ns
      }
    if(active == 'dev'){
      r.devs[[1]][[i.dev]][[1]] <<- c(r.devs[[1]][[i.dev]][[1]],l.dist)
      r.devs[[1]][[i.dev]][[2]] <<- c(r.devs[[1]][[i.dev]][[2]],r.dist)
      r.devs[[2]][[i.dev]][[1]] <<- c(r.devs[[2]][[i.dev]][[1]],l.prop)
      r.devs[[2]][[i.dev]][[2]] <<- c(r.devs[[2]][[i.dev]][[2]],r.prop)
      plt.res <<- r.devs
      }
    })
  
  
  clear = reactive({
    input$Type
    input$Method
    input$Clearplt
    r.nps   <<- empty
    r.nos   <<- empty
    r.ns    <<- empty
    r.devs  <<- empty
    plt.res <<- empty
    })
  
  
  
  # ++++++++++++ Render function
  
  output$biasVarPlot = renderPlot({
    
    # ++++++++++ Preliminaries
    
    # make dependent on...
    type   = input$Type
    method = input$Method
    
    print(type)
    
    # use shorter names
    np   = input$Nproblem   
    no   = input$Noutcome   
    n    = input$Nsamples
    dev  = input$Noise 
    nrun     = input$Nrun
    ncores   = input$Ncores
    
    # translate variable in indices
    i.np  <<- which(nps  == np)
    i.no  <<- which(nos  == no)
    i.n   <<- which(ns   == n )
    i.dev <<- which(devs == dev)
    
    # ++++++++++ Fitting
    
    # collect time
    t = proc.time()[3]
    
    # Test number of cores
    if(ncores != 1){
      
      # Multi core execution
      sfInit(parallel = T, cpus = ncores)
      sfLibrary(shinyCpp)
      sfExport('type','no','n','dev','np')
      res = sfClusterApplyLB(rep(round(nrun/(ncores)),ncores),
                             function(x) shinyCpp::runm(nm = x,type = type, no = no, n = n, sd = dev, np = np))
      sfStop()
    } else {
      
      # Single core execution
      res = list()
      for(i in 1:nrun){
        res[[i]] = shinyCpp::run(type = type, no = no, n = n, sd = dev, np = np)
        }
      }
    
    # print time
    # print(proc.time()[3]-t)
    
    # collect results
    res = do.call(rbind,res)
    LLs = res[,c(1,3,5,7)]      

    # +++++++++ Plot preparations
    
    nam = '-LL'
    if(type == 'SUM vs. VUM') { # SUM-VUM ; VUM-SUM
      csel = c(1,2);ms.l=c('SUM','VUM')
      if(method == 'AIC') {nam='AIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + 2;        LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 4 }
      if(method == 'BIC') {nam='BIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + log(np);   LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 2*log(np) }   
      if(method == 'NML') {nam='NML';LLs[,c(1,4)] = LLs[,c(1,4)] / complex[1]; LLs[,c(2,3)] =   LLs[,c(2,3)] / complex[2] }
    }
    if(type == 'SUM vs. SWIM') {
      csel = c(1,3);ms.l=c('SUM','SWIM')
      if(method == 'AIC') {nam='AIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + 2;        LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 4 }
      if(method == 'BIC') {nam='BIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + log(np);   LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 2*log(np) }   
      if(method == 'NML') {nam='NML';LLs[,c(1,4)] = LLs[,c(1,4)] / complex[1]; LLs[,c(2,3)] =   LLs[,c(2,3)] / complex[3] }
    }
    if(type == 'VUM vs. SWIM') {
      csel = c(2,3);ms.l=c('VUM','SWIM')
      if(method == 'AIC') {nam='AIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + 4;        LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 4 }
      if(method == 'BIC') {nam='BIC';LLs[,c(1,4)] = 2*LLs[,c(1,4)] + 2*log(np);   LLs[,c(2,3)] = 2*LLs[,c(2,3)] + 2*log(np) }   
      if(method == 'NML') {nam='NML';LLs[,c(1,4)] = LLs[,c(1,4)] / complex[2]; LLs[,c(2,3)] =   LLs[,c(2,3)] / complex[3] }
    }
    
    # split in data for left and right panel
    left  = LLs[,c(1,2)]  
    right = LLs[,c(3,4)] 
    
    # cOmpute points
    l.cent = colMeans(left)
    r.cent = colMeans(right)
    l.dist <<- dist(l.cent)
    r.dist <<- dist(r.cent) 
    l.prop1 = sum(left[,1]>left[,2]) ; l.prop2 = sum(left[,1]<left[,2])
    l.prop <<- l.prop1 / (l.prop1 + l.prop2)
    r.prop1 = sum(right[,1]>right[,2]) ; r.prop2 = sum(right[,1]<right[,2])
    r.prop <<- r.prop1 / (r.prop1 + r.prop2)
    
    # set colors
    cols.l = cols0[csel]
    cols.r = cols0[rev(csel)]
    cols.l.t = cols1[csel]
    cols.r.t = cols1[rev(csel)]
    cols.l.t2 = cols2[csel]
    cols.r.t2 = cols2[rev(csel)]
    
    # set model names
    ms.r   = rev(ms.l)
    
    
    # +++++++++++ Start plotting
    
    layout(matrix(c(1,2,3,3,4,4),ncol=2,byrow=T))
    par(mar=c(4,4,1.5,1))
    
      # +++++++++++ 1: Landscaping panels
      
      # left panel
      rng = range(c(left))
      xlim = ylim = rng + c(-1,1)*diff(rng)*.001
      plot.new();plot.window(xlim,ylim)
      usr = par()$usr
      grid(20,20,col='grey75',lty=1,lwd=.3);box(col='black',lwd=2)
      lines(usr[1:2],usr[3:4],col='black',lwd=2)
      mtext(nam,side=c(1,2),line=c(.9,.5),cex=1.5,font=2)
      
      text(usr[1]+.96*diff(usr[1:2]),usr[3]+.17*diff(usr[3:4]),ms.l[2],cex=4,col=cols.l[2],font=2,adj=1)
      text(usr[1]+.96*diff(usr[1:2]),usr[3]+ifelse(ms.l[2]=='SWIM',.068,.07)*diff(usr[3:4]),'contender',cex=ifelse(ms.l[2]=='SWIM',2.2,1.8),col='grey75',font=2,adj=1)
      text(usr[1]+.04*diff(usr[1:2]),usr[3]+.90*diff(usr[3:4]),ms.l[1],cex=4,col=cols.l[1],font=2,adj=0)
      text(usr[1]+.04*diff(usr[1:2]),usr[3]+ifelse(ms.l[1]=='SWIM',.78,.785)*diff(usr[3:4]),'source',cex=ifelse(ms.l[1]=='SWIM',3.32,2.7),col='grey75',font=2,adj=0)
      
      points(left,pch=16,cex=.8)
  
      intsct = findIntersect(l.cent)
      points(l.cent[1],l.cent[2],pch=16,col=rgb(1,1,1,alpha=1),cex=3.1)
      lines(rbind(l.cent,intsct),col='white',lwd=3.5)    
      
      points(l.cent[1],l.cent[2],pch=16,col='red',cex=1.9)
      lines(rbind(l.cent,intsct),col='red',lwd=2)
      
      outline(l.cent[1] + diff(rng)*ifelse(l.dist<0,-.03,.03),
              l.cent[2], #+ diff(rng)*ifelse(l.dist<0,.05,-.05),
              adj = ifelse(l.dist<0,1,0),
              labels = abs(round(l.dist,2)),col = 'red',bg = rgb(1,1,1,alpha=1),
              h = diff(rng)*.4, w = diff(rng)*.4,cex=1.4, font=1
              )
      

      # right panel
      rng = range(c(right))
      xlim = ylim = rng + c(-1,1)*diff(rng)*.001
      plot.new();plot.window(xlim,ylim)
      usr = par()$usr
      grid(20,20,col='grey75',lty=1,lwd=.3);box(col='black',lwd=2)
      lines(usr[1:2],usr[3:4],col='black',lwd=2)
      mtext(nam,side=c(1,2),line=c(.9,.5),cex=1.5,font=2)
      
      text(usr[1]+.96*diff(usr[1:2]),usr[3]+.17*diff(usr[3:4]),ms.r[2],cex=4,col=cols.r[2],font=2,adj=1)
      text(usr[1]+.96*diff(usr[1:2]),usr[3]+ifelse(ms.r[2]=='SWIM',.068,.07)*diff(usr[3:4]),'contender',cex=ifelse(ms.r[2]=='SWIM',2.2,1.8),col='grey75',font=2,adj=1)
      text(usr[1]+.04*diff(usr[1:2]),usr[3]+.90*diff(usr[3:4]),ms.r[1],cex=4,col=cols.r[1],font=2,adj=0)
      text(usr[1]+.04*diff(usr[1:2]),usr[3]+ifelse(ms.r[1]=='SWIM',.78,.785)*diff(usr[3:4]),'source',cex=ifelse(ms.r[1]=='SWIM',3.32,2.7),col='grey75',font=2,adj=0)
      
      points(right,pch=16,cex=.8)
      
      intsct = findIntersect(r.cent)
      points(r.cent[1],r.cent[2],pch=16,col=rgb(1,1,1,alpha=1),cex=3.1)
      lines(rbind(r.cent,intsct),col='white',lwd=3.5)
      
      points(r.cent[1],r.cent[2],pch=16,col='red',cex=1.9)
      lines(rbind(r.cent,intsct),col='red',lwd=2)
      

      outline(r.cent[1] + diff(rng)*ifelse(r.dist<0,-.03,.03),
              r.cent[2], #+ diff(rng)*ifelse(r.dist<0,.05,-.05),
              adj = ifelse(r.dist<0,1,0),
              labels = abs(round(r.dist,2)),col = 'red',bg = rgb(1,1,1,alpha=.8),
              h = diff(rng)*.4, w = diff(rng)*.4,cex=1.4, font=1
      )
    
      # +++++++++++ Update results
      
      sel.no()
      sel.n()
      sel.dev()
      sel.np()
      rerun()
      clear()
    
      # +++++++++++ Average fit value
            
      l.lin = lapply(plt.res[[1]],function(x) x[[1]])
      r.lin = lapply(plt.res[[1]],function(x) x[[2]])
      
      ls = unlist(l.lin) ; ls = ls[!is.na(ls)]
      rs = unlist(r.lin) ; rs = rs[!is.na(rs)]
      ds = ls - rs
      
      if(length(ds)>0){
        mx = max(abs(c(ls,rs,ds)))
        } else {
        mx = 1
        }
      ylim = c(-mx,mx)
      xlim=c(.5,11.5)
      plot.new();plot.window(xlim,ylim)
      usr = par()$usr
      grid(20,20,col='grey75',lty=1,lwd=.3);box(col='grey75')
      lines(usr[1:2],c(0,0),col='grey25',lwd=2)
      
      legend('center',horiz=T,lwd=2,pch=c(16,17),col=c(cols.l[1],cols.r[1]),
             legend=c(paste0('generated by ',ms.l[1]),paste0('generated by ',ms.r[1])),cex=1.1,box.col='grey25',
             pt.cex=1.7,text.col='black')
      
      for(i in 1:11) {a = l.lin[[i]]; points(rep(i,length(a)),a,    pch=16,col=cols.l.t[1],cex=2)}
      for(i in 1:11) {a = r.lin[[i]]; points(rep(i,length(a)),-1 * a, pch=17,col=cols.r.t[1],cex=2)}
      
      m.l = sapply(l.lin,mean,na.rm=T) ; l.p = which(!is.na(m.l))
      m.r = sapply(r.lin,mean,na.rm=T) ; r.p = which(!is.na(m.r))
      
      lines(l.p,   m.l[l.p],col=cols.l[1],lwd=3)
      lines(r.p,-1*m.r[r.p],col=cols.r[1],lwd=3)
      
      points(l.p,     m.l[l.p],pch=21,col='white',bg=cols.l[1],cex=2.5,lwd=1.5)
      points(r.p,-1 * m.r[r.p],pch=24,col='white',bg=cols.r[1],cex=2.5,lwd=1.5)
      
      mtext(ticks,at=1:11,side=1,line=.8)
      mtext(lab,side=1,line=2.9,cex=1.5,font=2)
      mtext(round(c(-mx,0,mx),2),side=2,line=.5,at=c(-mx,0,mx),las=1)
      mtext(paste0('Delta ',method),side=2,line=2.4,cex=1.5,font=2)
      
      text(usr[1]+.505*diff(usr[1:2]),usr[3]+.92*diff(usr[3:4]),ms.r[1],cex=2,col=cols.r[1],font=2,adj=0)
      text(usr[1]+.505*diff(usr[1:2]),usr[3]+.08*diff(usr[3:4]),ms.l[1],cex=2,col=cols.l[1],font=2,adj=0)
      
      text(usr[1]+.495*diff(usr[1:2]),usr[3]+.92*diff(usr[3:4]),'in favor of',cex=2,font=1,adj=1,col='black')
      text(usr[1]+.495*diff(usr[1:2]),usr[3]+.08*diff(usr[3:4]),'in favor of',cex=2,font=1,adj=1,col='black')
    
      
      # +++++++++++ Percentage best fit
    
      l.lin = lapply(plt.res[[2]],function(x) x[[1]])
      r.lin = lapply(plt.res[[2]],function(x) x[[2]])
      
      xlim=c(.5,11.5)
      plot.new();plot.window(xlim,ylim=c(0,1))
      usr = par()$usr
      grid(20,20,col='grey75',lty=1,lwd=.3);box(col='grey75')
      lines(usr[1:2],c(.5,.5),col='grey25',lwd=2)
      
      legend('center',horiz=T,lwd=2,pch=c(16,17),col=c(cols.l[1],cols.r[1]),
             legend=c(paste0('generated by ',ms.l[1]),paste0('generated by ',ms.r[1])),cex=1.1,box.col='grey25',
             pt.cex=1.7,text.col='black')
      
      for(i in 1:11) {a = l.lin[[i]]; points(rep(i-.04,length(a)),  a, pch=16,col=cols.l.t2[1],cex=2)}
      for(i in 1:11) {a = r.lin[[i]]; points(rep(i+.04,length(a)),1-a, pch=17,col=cols.r.t2[1],cex=2)}
      
      m.l = sapply(l.lin,mean,na.rm=T) ; l.p = which(!is.na(m.l))
      m.r = sapply(r.lin,mean,na.rm=T) ; r.p = which(!is.na(m.r))
      
      lines(l.p,   m.l[l.p],col=cols.l[1],lwd=3)
      lines(r.p, 1-m.r[r.p],col=cols.r[1],lwd=3)
      
      points(l.p-.04,   m.l[l.p],pch=21,col='white',bg=cols.l[1],cex=2.5,lwd=1.5)
      points(r.p+.04, 1-m.r[r.p],pch=24,col='white',bg=cols.r[1],cex=2.5,lwd=1.5)
      
      mtext(ticks,at=1:11,side=1,line=.8)
      mtext(lab,side=1,line=2.9,cex=1.5,font=2)
      mtext(round(c(0,.5,1),2),side=2,line=.5,at=c(0,.5,1),las=1)
      mtext(paste0('% best ',method),side=2,line=2.4,cex=1.5,font=2)
      
      text(usr[1]+.505*diff(usr[1:2]),usr[3]+.92*diff(usr[3:4]),ms.r[1],cex=2,col=cols.r[1],font=2,adj=0)
      text(usr[1]+.505*diff(usr[1:2]),usr[3]+.08*diff(usr[3:4]),ms.l[1],cex=2,col=cols.l[1],font=2,adj=0)
      
      text(usr[1]+.495*diff(usr[1:2]),usr[3]+.92*diff(usr[3:4]),'in favor of',cex=2,font=1,adj=1,col='black')
      text(usr[1]+.495*diff(usr[1:2]),usr[3]+.08*diff(usr[3:4]),'in favor of',cex=2,font=1,adj=1,col='black')
      
  },width=550,height=800)
})


