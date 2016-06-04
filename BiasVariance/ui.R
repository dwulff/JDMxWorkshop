########
#
#



require(shiny)
shinyUI(fluidPage(
  titlePanel('Bias-variance dilemma'),
  sidebarLayout(sidebarPanel(
    sliderInput('Degree.true',"True degree:",min=1,max=10,value=3),
    sliderInput('Noise',"Noise:",min=0,max=10,value=2),
    sliderInput('Sample.size',"Sample size:",min=12,max=100,value=20),
    actionButton('NewTruth','Generate experiment',width='100%'),
    sliderInput('Degree.model',"Model degree:",min=1,max=10,value=10,
                animate=animationOptions(interval=300, loop=F)),
    actionButton('SampleData',' \nConduct single experiment',width='100%'),
    actionButton('Clear','Clear plot',width='100%')
  ),
  mainPanel(
    plotOutput('biasVarPlot')
  )
  )
))





