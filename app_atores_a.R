# Produzindo a interface para carregar planilha dos atores
# Suporte em: http://shiny.rstudio.com/

# Adriano Lauro
# Rio de Janeiro, em 24 de outubro de 2022.
#############################################################################
#install.packages('rsconnect')
rsconnect::setAccountInfo(name='adriano-lauro',
                          token='FA4627027EDAFBEE4EC028A9B558EFAE',
                          secret='/NZRTg8euZMBrr6AFe2CMpbb1einL+Squr9ti11+')

library(rsconnect)

library(shiny)
library(markdown)

library(shinythemes)
library(DT)

library(readxl)
library(tidyverse)

library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gapminder)
library(grid)
library(gridExtra)
library(GGally)

library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Grafico de Atores",
    tabPanel("Carregando Planilha",
      sidebarLayout(
         sidebarPanel(
           
           h3("Formato da planilha"),
           h4("Quatro colunas:"),
           h6("1 - Nome da situacao - deve ser repetido para todas as linhas."),
           h6("2 - Nome do ator - nome do ator a ser incluido."),
           h6("3 - Influencia - valor entre 0 e 3 para gradar a influencia do ator naquele evento."),
           h6("4 - Interesse - valores entre -3 e 3, em que os atores com interesses opostos devem ter valores com sinais opostos."),
           tags$hr(),
           h6("A planilha deve ter apenas as colunas acima descritas - evite comentarios ou textos diversos"),
           h6("Essas colunas devem ter o titulo na primeira linha."),
           h6("Utilize nomes curtos tanto para as situacoes como para os atores."),
           
           hr(),
           h5("Exemplo pode ser obtido no link: "),
           
           #tableOutput("exemplo")
           
           uiOutput("exemplo_planilha"),
           
           tags$hr(),
           
           fileInput("file1", "Escolha um arquivo Excel",
                     multiple = FALSE),
           
           tableOutput("exemplo_tabela")
       ),
       mainPanel(
         # Output: Data file ----
         #tableOutput("contents"),
         tableOutput("exemplo_tabela"),
         hr(),
         plotOutput("graf_ator")
       )
      )
    ),
      
    tabPanel("Carregando dados na interface", 
      sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
          # Output: Data file ----
          tableOutput("contents"),
          hr(),
          plotOutput("graf_ator")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # lendo a planilha Excel
    tryCatch(
      {
        df <- read_excel(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$exemplo_tabela <- renderTable({
    exemplo <- read_excel("C:/Users/ADRIANO LAURO/Documents/Adriano/Trabalho/R/dados/Atores/exemplo_shiny.xlsx")
  })
  
  url <- a("Exemplo de Planilha", href="https://drive.google.com/drive/folders/1Pd-xUGa1Ep0OakbdJXNg6o9ypmJTWrRw?usp=sharing")
  output$exemplo_planilha <- renderUI({
    tagList("Arquivo em:", url)
  })
  
  #colnames(df) <- c("Situacao", "Ator", "Influencia", "Interesse")
  #df$Influencia <- as.numeric(atores$Influencia)
  
  
  output$graf_ator <- renderPlot(
    
    ggplot(df) +
      aes(Interesse, Influencia, color = Ator) +
      theme_bw() + 
      geom_point() +
      scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-3.5,3.5)) +
      scale_y_continuous(breaks=c(0,1,2,3),limits = c(-0.3, 3.3)) +
      geom_vline(xintercept = 1, linetype="dotdash") +
      geom_vline(xintercept = -1, linetype="dotdash") +
      geom_hline(yintercept = 1, linetype="dotdash") +
      geom_text(mapping=aes(label=Ator),  
                check_overlap = FALSE,
                nudge_x = 0,
                nudge_y = 0.1) +
      labs(title = "Grafico de Atores",
           subtitle = df$Situacao[1]) +  
      annotate("Text", x = -2, y = 1.5, label = "Jogador", color = blues9) +
      annotate("Text", x = 2, y = 1.5, label = "Jogador", color = "red") +
      annotate("Text", x = 0, y = 1.5, label = "Juiz") +
      annotate("Text", x = -2, y = 0.5, label = "Apostador", color = blues9) +
      annotate("Text", x = 2, y = 0.5, label = "Apostador", color = "red") +
      annotate("Text", x = 0, y = 0.5, label = "Plateia")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
