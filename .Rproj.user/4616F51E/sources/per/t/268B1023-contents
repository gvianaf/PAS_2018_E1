#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load data

setwd("X:/CIG/PAS")
PAS2019 <- rio::import("NotasPAS2019.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Notas no PAS 2019"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("Inscrição",
                     "Selecione seu número de inscrição:",
                     choices = unique(PAS2019$Inscrição))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("notasPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$notasPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     inscricao <- input$Inscrição
     texto_nota <- paste("Sua nota foi melhor que", 
                         scales::percent(round(ecdf(PAS2019$EscoreBruto1)(PAS2019$EscoreBruto1)[PAS2019$Inscrição == inscricao],2)),
                         "dos participantes")
     
     ggplot(data = PAS2019, aes(EscoreBruto1)) +
       geom_histogram(binwidth = 1,
                      color = "royalblue",
                      fill = "lightblue",
                      boundary = 0) +
       geom_vline(xintercept = PAS2019$EscoreBruto1[PAS2019$Inscrição == inscricao],
                  color = "red",
                  size = 2) +
       labs(x = "Escore Bruto 1",
            y = "Número de estudantes",
            title = texto_nota) +
       scale_x_continuous(breaks = c(seq(0:10)),
                          position = "right") +
       theme_classic()

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

