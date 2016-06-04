shinyServer(function(input, output) {

  
  max.est.pol = 10
  n = 1000
  
  x = seq(-1,1,length=n)
  y = rep(0,length(x))

  yhat.all = rep(NA,length(x))
    
  est.errs = as.list(rep(NA,max.est.pol))
  pre.errs = as.list(rep(NA,max.est.pol))

  
  gety = reactive({
    
    input$NewTruth
    
    est.errs <<- as.list(rep(NA,max.est.pol))
    pre.errs <<- as.list(rep(NA,max.est.pol))
    
    gen.des = matrix(0,nrow=length(x),ncol=input$Degree.true)
    for(i in 1:input$Degree.true) gen.des[,i] = x**i

    ws = runif(input$Degree.true,-10,0)  
    
    y = gen.des %*% ws

    y
    
    })

  
  est.des = reactive({
    input$Degree.model
    input$SampleData
    isolate({
      est.des = matrix(0,nrow=length(x),ncol=input$Degree.model)
      for(i in 1:input$Degree.model) est.des[,i] = x**i    
      })
    
    est.des
    })
  
  ids = reactive({
    input$NewTruth
    input$SampleData
    sample(2:(n-1))
    })
  
  noise = reactive({
    input$NewTruth
    rnorm(length(y), 0, input$Noise * sd(x))
    })

  clear = reactive({
    input$Degree.true
    input$NewTruth
    input$Clear
    input$Noise
    input$Sample.size
    est.errs <<- as.list(rep(NA,max.est.pol))
    pre.errs <<- as.list(rep(NA,max.est.pol))
    yhat.all <<- rep(NA,length(x))
    })
  
  output$biasVarPlot = renderPlot({
    
    #isolate({
      esti = c(1,ids()[1:(input$Sample.size-2)],n)
      prei = ids()[(input$Sample.size-1):(n-2)]
    #  })
    y. = gety() + noise()
    
    yest = y.[esti]
    ypre = y.[prei]
    xest = est.des()[esti,]
    xpre = est.des()[prei,]
    
    pars = solve(t(xest)%*%xest)%*%t(xest)%*%yest
    
    yhat.all <<- est.des() %*% pars
    yhat.est = xest %*% pars
    yhat.pre = xpre %*% pars
    
    est.err = mean((yest - c(yhat.est))**2)
    pre.err = mean((ypre - c(yhat.pre))**2)
    
    max.est.err = mean((yest - mean(yest))**2)
    max.pre.err = mean((ypre - mean(ypre))**2)
    
    est.errs[[input$Degree.model]] <<- c(est.errs[[input$Degree.model]],est.err / max.est.err)
    pre.errs[[input$Degree.model]] <<- c(pre.errs[[input$Degree.model]],pre.err / max.pre.err)
    
    clear()
        
    m.est.errs = sapply(est.errs,mean,na.rm=T)
    m.pre.errs = sapply(pre.errs,mean,na.rm=T)
    
    par(mfrow = c(2,1),mar=c(3,3,1,1))
    plot.new();plot.window(range(x),ylim=range(y.))
    grid(20,20,col='grey75',lty=1,lwd=.3);box(col='grey75')
    mtext(c('X','Y'),side=c(1,2),font=1,las=1,cex=1.5,line=.5)
    points(x[prei],ypre,pch=16,cex=.3)
    lines(x,yhat.all,col='red',lwd=4)
    points(x[esti],yest,pch=16,cex=1.3)
    legend('topright',bg='white',box.col='grey75',
           legend=c('Seen data','Unseen data','Prediction'),pch=c(16,16,NA),lwd=c(NA,NA,4),cex=.9,
           col=c('black','black','red'),pt.cex=c(1.5,.3,NA))
    
    ys = c(unlist(est.errs),unlist(pre.errs))
    if(sum(!is.na(ys))!=0){
      ys = ys[!is.na(ys)]
      ys = ys[ys > mean(ys)-5*sd(ys) & ys < mean(ys)+5*sd(ys)]
      ylim = range(ys)
      } else {
        ylim = c(0,1)
      }
    xlim = c(1,max.est.pol)+c(-.5,.5)
    plot.new();plot.window(ylim=ylim,xlim=xlim)
    mtext(round(ylim,2),side=2,at=ylim,las=1,line=.5)
    usr = par()$usr
    rect(input$Degree.true-.5,usr[3],input$Degree.true+.5,usr[4],
         col=rgb(0,0,0,alpha=.1),border=NA)
    grid(20,20,col='grey75',lty=1,lwd=.3);box(col='grey75')
    if(ylim[2]>1) lines(usr[1:2],c(1,1),col='grey25')
    mtext(c('Degree of polynomial',expression(sigma[error]^2/sigma[true]^2)),side=c(1,2),line=c(2.1,.5),font=1,cex=1.5)
    mtext(1:10,side=1,at=1:10,line=.4)
    legend('top',bg='white',box.col='grey75',
           legend=c('Seen data','Unseen data'),pch=c(16,17),lwd=c(3,3),cex=.9,
           col=c('black','red'),pt.cex=c(1,1))
    for(i in 1:max.est.pol) {a = est.errs[[i]]; points(rep(i,length(a)),a,col=rgb(0,0,0,alpha=.3),pch=16)}
    for(i in 1:max.est.pol) {a = pre.errs[[i]]; points(rep(i,length(a)),a,col=rgb(1,0,0,alpha=.3),pch=17)}
    if(sum(!is.na(m.est.errs))>1) lines(which(!is.na(m.est.errs)),m.est.errs[which(!is.na(m.est.errs))],lwd=4)
    if(sum(!is.na(m.pre.errs))>1) lines(which(!is.na(m.pre.errs)),m.pre.errs[which(!is.na(m.pre.errs))],col='red',lwd=4)
    points(1:max.est.pol,m.est.errs,pch=21,bg='black',lwd=2,cex=2,col='white')
    points(1:max.est.pol,m.pre.errs,pch=24,bg='red',lwd=2,cex=2,col='white')

      
  },width=600,height=550)
})