shinyServer(function(input, output) {

 if(!require(devtools,silent=T)) install.packages('devtools') ; require(devtools)
 if(!require(shinyCpp,silent=T)) install_github('dwulff/JDMxWorkshop/shinyCpp')

 require(shinyCpp)
  
  
  maxN = 300
  np = 100
  d  = 1
  run = T
  
  n = 2
  
  vums  = as.list(rep(NA,maxN-1))
  swims = as.list(rep(NA,maxN-1))
  
  cols  = c(rgb(t(col2rgb("yellow3")),maxColorValue=255,alpha=255),
            rgb(t(col2rgb("seagreen")),maxColorValue=255,alpha=255))
  colst = c(rgb(t(col2rgb("yellow3")),maxColorValue=255,alpha=255*.3),
            rgb(t(col2rgb("seagreen")),maxColorValue=255,alpha=255*.3))
  
  probs = reactive({
    
    #vums  <<- as.list(rep(NA,maxN-1))
    #swims <<- as.list(rep(NA,maxN-1))
    
    p = problemGenerator(100,input$Noutcome)    
  
    p
    })
  
   changeN1 = reactive({
     n <<- input$Ssize1
     })
  
   changeN2 = reactive({
     n <<- input$Ssize2
     })
   
  clear = reactive({
    input$Clear
    vums  <<- as.list(rep(NA,maxN-1))
    swims <<- as.list(rep(NA,maxN-1))
    })
  
  calc = reactive({
    
    input$Ssize1
    input$Ssize2
    input$Run
    
    p = probs()
    s = sampl(p,n)
    
    isolate({pvum  = mean(VUMpuni(s,d,nsteps = input$nsteps)/(100*10**d))})
    pswim = mean(SWIMpuni(s,d)/(100*10**d))
    
    vums[[n-1]]  <<- c(vums[[n-1]],pvum)
    swims[[n-1]] <<- c(swims[[n-1]],pswim)
    })
  
  
  output$biasVarPlot = renderPlot({
    
    changeN2()
    changeN1()
    calc()
    clear()
    
    input$Run
    
    mvums = sapply(vums,mean,na.rm=T)
    mswims = sapply(swims,mean,na.rm=T)
    
    entries = which(!is.na(mvums))+1
    
    if(length(entries) > 1){
      xlims = range(entries)
      if(xlims[2]<20) xlims[2] = 20
      if(xlims[1]>2)  xlims[1] = 2
    } else {
      xlims = c(2,20)
      }
    
    xticks = c(2,seq(round(xlims[2]/5),round(xlims[2]),round(xlims[2]/5)))
    
    par(mar=c(3,3,1,1))
    xlim=xlims;ylim=c(0,1)
    plot.new();plot.window(xlim,ylim)
    grid(20,20,col='grey75',lty=1,lwd=.3);box(col='grey75')
    mtext(xticks,side=1,at=xticks,line=.5,cex=1.3)
    mtext(c('Sample size',expression(Phi)),line=c(2.3,1),cex=2,side=c(1,2),las=1)
    mtext(c(0,1),side=2,at=c(0,1),las=1,line=.5,cex=1.3)
    
    for(i in 1:(maxN-1)) {a=vums[[i]]; points(rep(i+1,length(a)),a,col=colst[1],cex=1,pch=16)}
    for(i in 1:(maxN-1)) {a=swims[[i]];points(rep(i+1,length(a)),a,col=colst[2],cex=1,pch=17)}
    
    if(sum(!is.na(mvums))>1) lines(which(!is.na(mvums))+1,mvums[which(!is.na(mvums))],col=cols[1],lwd=4)
    if(sum(!is.na(mswims))>1) lines(which(!is.na(mswims))+1,mswims[which(!is.na(mswims))],col=cols[2],lwd=4)
    
    points(2:maxN,mvums,bg=cols[1],col='white',cex=1.5,pch=21)
    points(2:maxN,mswims,bg=cols[2],col='white',cex=1.5,pch=24)
    
    legend('top',legend=c('VUM','SWIM'),pch=c(16,17),lwd=4,col=cols,cex=1,box.col='grey75',pt.cex=1.5)
    
    
  },width=600,height=545)
})
