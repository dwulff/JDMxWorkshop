########
#
#



require(shiny)
shinyUI(fluidPage(
  titlePanel('Landscaping CPT'),
  sidebarLayout(sidebarPanel(
    radioButtons('Type',  "Comparison",choices = c('Tversky-Kahneman. vs. Goldstein-Einhorn','Tversky-Kahneman vs. Prelec','Goldstein-Einhorn vs. Prelec'),width='100%'),
    radioButtons('Method',"Method",choices = c('LL','AIC','BIC'),width='100%'),
    radioButtons('Sens',"Sensitivity",choices = c('.001','.1','10','Inf'),width='100%'),
    sliderInput('Nproblem',"Number of problems",min=10,max=50,value=10,step=4,
                animate=animationOptions(interval=1000, loop=F)),
    sliderInput('Noutcome',"Number of outcomes",min=2,max=22,value=2,step=2,
                animate=animationOptions(interval=1000, loop=F)),
    actionButton('Rerun','Rerun simulation',width='100%'),
    actionButton('Clearplt','Clear plot',width='100%'),
    sliderInput('Nrun',"Expert: Number of runs",min=100,max=1000,value=100,step=100),
    sliderInput('Ncores',"Expert: Number of cores",min=1,max=8,value=1,step=1)
    ),
  mainPanel(
    plotOutput('biasVarPlot')
    )
  )
))





