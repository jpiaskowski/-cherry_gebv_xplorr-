
shinyUI(fluidPage(
  titlePanel("Sweet Cherry Genome-Enabled Breeding Values XplorR"),
  p("Find the best individuals by their breeding values (additive-only effects) or
    total genetic values (additive and non-additive sources of genetic variance.)"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
      fluidRow(
        radioButtons("gen.val", label = h4("Genetic Parameter"),
                     choices = list("breeding values" = "a", "genetic values" = "g",
                                    "phenotypic means" = "p"),selected = "a"),
        tags$h4("Traits to View"),
        actionButton("unCheck", "Deselect All Traits"),
        checkboxGroupInput('show_vars', label = NULL, names(A1)[3:ncol(A1)],
                           selected = c("Germplasm","Bloom Date","Bloom Time"))
        
        )
      ),
  mainPanel(
    tags$h3("Filter Predicted Genetic Performance of Individuals"),
    dataTableOutput("mytable"),
    dataTableOutput("filtered_data"),
    fluidRow(
      h4("Download Filtered Results"),
      downloadButton("downloadData1", label = ".csv"),br(),br()
          ),
    tags$hr(),
    h3("Scatter Plots!"),
    fluidRow(
      column(4,
             selectInput("trait1", "variable for y-axis", choices = colnames(A1)[4:ncol(A1)],selected = "SSC")),
      column(4,
             selectInput("trait2", "variable for x-axis", choices = colnames(A1)[4:ncol(A1)],selected = "Fruit Weight")),
    br()), 
      fluidRow(
             plotlyOutput("bivar")),
    fluidRow(
        h4("View Chosen Individuals from Scatterplot"),
        #textOutput("clickTable")),br(),
        tableOutput("clickTable")),br(),
    fluidRow(
      h4("Download Data for Selected Points"),
      downloadButton("downloadData2", label = ".csv"),br(),br())
    # h3("Those Blasted Dynamic Plots"),
    # uiOutput("plots")
      )
    )
  
  )
)
