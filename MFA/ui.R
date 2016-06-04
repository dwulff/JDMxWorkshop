########
#
#



require(shiny)
shinyUI(fluidPage(
  titlePanel('Model flexilibility analysis (Veksler et al., 2015)'),
  sidebarLayout(sidebarPanel(
    sliderInput('Noutcome',"Number of outcomes:",min=2,max=20,value=2),
    sliderInput('Ssize1',"Sample size",min=2,max=20,value=1,step = 1,
                animate=animationOptions(interval=300, loop=F)),
    sliderInput('Ssize2',"Sample size",min=20,max=200,value=19,step = 10,
                animate=animationOptions(interval=300, loop=F)),
    actionButton('Run','Run simulation',width='100%'),
    actionButton('Clear','Clear plot',width='100%'),
    sliderInput('nsteps',"Number of VUM evaluations",min=1000,max=101000,value=11000,step = 10000,
                animate=animationOptions(interval=300, loop=F))
    ),
  mainPanel(
    plotOutput('biasVarPlot')
  )
  )
))





