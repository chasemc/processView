#' main app ui
#'
#' @return shiny ui
#' @export
#'
app_ui <- function(){

  shiny::pageWithSidebar(
    shiny::headerPanel('Windows Process Viewer'),
    shiny::sidebarPanel(
      shiny::actionButton('reset',
                          'Initiate/Reset'),
      shiny::numericInput("height",
                          "height",
                          value = 500,
                          min = 1,
                          max = 100000)
    ),

    shiny::mainPanel(
      processView::dendrogram_ui("dendrogram")
    )
  )}



