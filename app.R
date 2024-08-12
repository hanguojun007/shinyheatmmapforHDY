library(shiny)
library(reactable)
library(ComplexHeatmap)

ui <- fluidPage(
  titlePanel("Heatmap"),
  fluidRow(
    column(
      4,
      fileInput("file", label = NULL, placeholder = "Upload File", accept = c("tsv", "csv")),
      checkboxGroupInput("cols", "Columns", choices = NULL, selected = NULL)
    ),
    column(8, reactable::reactableOutput("table"))
  ),
  fluidRow(
    column(
      4,
      checkboxInput("show_rownames", "Show Row Names", value = TRUE),
      checkboxInput("show_colnames", "Show Column Names", value = TRUE)
    ),
    column(8, plotOutput("heatmap"))
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    vroom::vroom(input$file$datapath)
  })

  output$table <- renderReactable({
    reactable(data(), paginationType = "jump", selection = "multiple", onClick = "select", defaultSelected = c(1:nrow(data())))
  })
  # req(data())
  selected <- reactive(getReactableState("table", "selected"))

  observeEvent(data(), {
    updateCheckboxGroupInput(session, "cols", choices = colnames(data()), selected = colnames(data()))
  })

  col_selected <- reactive({
    req(input$cols)
    input$cols
  })

  showRownames <- reactive(input$show_rownames)
  showColnames <- reactive(input$show_colnames)

  output$heatmap <- renderPlot(
    {
      Heatmap(
        as.matrix(data()[selected(), col_selected()]),
        show_row_names = showRownames(),
        show_column_names = showColnames()
      )
    },
    res = 96
  )
}


shinyApp(ui, server)
