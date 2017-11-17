# Date: November 13, 2017
# For: DBT-BIF

# Import required libraries -----------------------------------------------
# rm(list = setdiff(ls(), lsf.str()))
cat('\f')   # Clear console
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
      plotOutput("box_plot"),
      dataTableOutput("out_anova")
    )
  )
)

# Create server -----------------------------------------------------------

server <- function(input, output) {
  output$view_data <- renderTable({
    req(input$datafile)
    our_data <- read.csv(file=paste(input$datafile$datapath),
            header = input$header,
            sep = input$sep,quote = input$quote)
    attach(our_data)
    if(input$disp == "head"){
      return(head(our_data))}
    else{
      return(our_data)}
  })
  output$data_summary <- renderDataTable({
    req(input$datafile)
    return(summary(output$our_data))
  })
  output$box_plot <- renderPlot({
    req(input$datafile)
    return(boxplot(as.matrix(our_data[,c(-1,-6)])))
  })
  output$out_anova <- renderDataTable({
    req(input$datafile)
    fit <- lm(our_data[,4]~our_data[,3],data = our_data)
    return(anova(fit))
  })
}

# Run application ---------------------------------------------------------

shinyApp(ui, server)
