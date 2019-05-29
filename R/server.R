#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export
app_server <- function(input, output, session) {

  #library(data.tree) # collapsibleTree package relies on loading the lib, unfortunately

  shiny::callModule(processView::dendrogram_server,
                    "dendrogram",
                    height = shiny::reactive(input$height),
                    resetButton = shiny::reactive(input$reset))



}
