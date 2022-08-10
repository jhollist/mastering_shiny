library(shiny)
library(ggplot2)

plotit <- function(dat){
  plot(dat[,1], dat[,2])
}

change_it <- function(){
  my_iris <- iris[sample(seq_along(iris$Sepal.Length), 10, replace = T),]
  readr::write_csv(my_iris, "iris.csv")
}

ui <- fluidPage(
  textInput("flame_csv", "FLAMe output file path and name.",placeholder = ""),
  actionButton("change", "Change it"),
  #actionButton("start", "Start collecting FLAMe data."),
  #actionButton("stop", "Stop collecting FLAMe data."),
  plotOutput("flame_plot"),
  tableOutput("flame_table")
)

server <- function(input, output, session){

  # One way to start and stop execution of modpoll
  #https://stackoverflow.com/questions/61853507/using-r-shiny-server-for-controlling-cyclic-execution-in-while-loop
  csv <- reactive(input$flame_csv)

  flame_data <- reactiveFileReader(
    intervalMillis = 1000,
    session = NULL,
    filePath = reactive(input$flame_csv),
    readFunc = readr::read_csv
  )

  observeEvent(input$change, {change_it()})

  output$flame_table <- renderTable(flame_data())

  output$flame_plot <- renderPlot({
    dat <- as.data.frame(flame_data())
    plot(dat[,1], dat[,2])
  })
}

shinyApp(ui, server)
