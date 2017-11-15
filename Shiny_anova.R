# Date: November 13, 2017
# For: DBT-BIF

# Import required libraries -----------------------------------------------

library(shiny)

# Cretae User Interface ---------------------------------------------------

ui <- fluidPage(
  titlePanel("Upload data file"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Kindly select a [.CSV] file",
                multiple = TRUE, accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
      radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',
                                                "Single Quote" = "'"),selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",choices = c(Head = "head",
                                                 All = "all"),selected = "head")
    ),
    mainPanel(
      tableOutput("view_data"),
      dataTableOutput("data_summary"),
      # dataTableOutput("data_summary2"),
      plotOutput("box_plot")
    )
  )
)

# Create server -----------------------------------------------------------

server <- function(input, output) {
  output$view_data <- renderTable({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath,
                   header = input$header,
                   sep = input$sep,quote = input$quote)
    if(input$disp == "head"){
      return(head(df))}
    else{
      return(df)}
  })
  output$data_summary <- renderDataTable({
    req(input$datafile)
    return(summary(df))
  })
  output$box_plot <- renderPlot({
    req(input$datafile)
    return(boxplot(as.matrix(df[,c(-1,-6)])))
  })
}

# Run application ---------------------------------------------------------

shinyApp(ui, server)