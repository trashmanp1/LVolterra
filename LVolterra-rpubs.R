#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)
library(ggplot2)
library(rsconnect)
#library(scales)
##############################################################
## based on https://www.r-bloggers.com/lotka-volterra-model%C2%A0%C2%A0intro/
## alpha = crescimento linear da presa
## beta = taxa de morte por predação do predador 
## gamma = taxa de mortalidade do predador
## delta = taxa de crescimento associada à abundancia de presas 
##############################################################
# criar uma interface para analisar a
ui <- fluidPage(
   
   # Application title
   titlePanel("Prey-predator relationship Lotka-Volterra model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "linear seal growth:",
                     min = 0.5,
                     max = 10.5,
                     step= 0.1,
                     animate=TRUE,
                     value = 3),
         sliderInput("beta",
                     "death rate by seal predation:",
                     min = 0.1,
                     max = 1.0,
                     step= 0.1,
                     animate=TRUE,
                     value = .6),
         sliderInput("gamma",
                     "bear mortality rate:",
                     min = 0.1,
                     max = 1.0,
                     step= 0.1,
                     animate=TRUE,
                     value = .1),
         sliderInput("delta",
                     "growth rate of the bear associated with the abundance of seals:",
                     min = 0.1,
                     max = 5.0,
                     step= 0.1,
                     animate=TRUE,
                     value = .8) ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        
        #plotOutput("plot_final")
        plotOutput("plot_final",width="100%"),
        verbatimTextOutput("summary"), HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
        p("Lotka-Volterra model of prey-predator relationship,  in blue the number seals ,  in green the bears."),
        p("The population of seals x has a growth proportional to the number of elements of that population. And is predated by a species of bears y that has a growth proportional to its own number and x (ie x * y is a number proportional to encounters with predation) and soon decrease the population of x seals with the increase of bears. On the other hand, if the population of bears increases and the population of seals decreases, there is less food for the population of bears that it is dependent on the abundance of seals x soon  will diminish proportionally to y. The system can be described by the nonlinear system of differential eq.:"),
        p(
          withMathJax("$$\\dot{x}=ax-bxy=x(a-by)$$")
        ),
        p(
          withMathJax("$$\\dot{y}=dxy-cy=y(-c+dx)$$")
        ),
        p("where a is the rate of growth of species x, b is the rate of predation, c rate of mortality of predator, d rate of growth of predator y associated with encounters with predation.
          By adjusting the values of vars, we can see some periodic grow similar to that observed in nature!")#
        #p("By adjusting the slides, we can see some periodic grow similar to that observed in nature!"),
        #p(" Pedro Rebelo, email: trashmanp1@gmail.com"),
        
        #p("Podemos rescalar a população, fazendo K=1. Obtemos assim a equação: "),
        #p(
        #  withMathJax("$$\\ x_{n+1}=x_{n}a(1-x_{n})$$")
        #),
        #p("onde x(n) representa assim a razão entre a população presente na geração n e o maximo possivel para essa população. Fazendo a=1+r, podemos com esta web.app feita com o R visualizar o comportamento para diferentes valores selecionaveis de a.")
        #p("O periodograma, permite detectar a presença de fenomenos periodicos da distribuição de x ao longo das geraç~ões")
        
        
        
        
         
      )
   )
)

# server
server <- function(input, output) {
 
  output$plot_final <- renderPlot({
    LotVmod <- function (Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dx = x*(alpha - beta*y)
        dy = -y*(gamma - delta*x)
        return(list(c(dx, dy)))
      })
    }
    
   
    Pars<-c(alpha=input$alpha , beta=input$beta, gamma= input$gamma, delta= input$delta)
    State <- c(x = 1, y = 10)
    Time <- seq(0, 100, by = 0.1)
    
    out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars , times = Time))
    ###################################################################################
    # simplicidade ´é sempre melhor
    #output_simulacao<-melt(out,id=c("time"))
    #names(output_simulacao)<-c("tempo","especie","n_pop")
    #p<-ggplot(data=output_simulacao)+geom_line(aes(x=tempo, y=n_pop, color=especie))
    #print(p)
    ##################################################################################
    plot(x = out[,"x"], xlab = "tempo", ylab = "n_pop",
         main = "Prey-predator relationship, Lotka-Volterra model", ylim = c(0.0, max(out$y)),
         minor.ticks = FALSE, col = "darkgreen",lty = 1)
    lines(x = out[,"x"], col = "darkgreen")
    lines(x = out[,"y"], col = "darkblue")
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


