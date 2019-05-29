#' Dendrogram UI
#'
#' @param id shiny namespace
#'
#' @return shiny module ui
#' @export
dendrogram_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dendroPlot"))

}


#' Dendrogram Server
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param height height of dendrogram
#' @param resetButton resetButton (actionbutton)
#'
#' @return shiny server
#' @export
#'
dendrogram_server <- function(input,
                              output,
                              session,
                              height = "1000px",
                              resetButton){

  dendrogramData <- shiny::reactiveValues()

  shiny::observeEvent(resetButton(), {

    a <- system2('wmic', 'process get Caption,ParentProcessId,ProcessId',
                 stdout = TRUE )
    b <- strsplit(a,
                  "\r")
    bb <- strsplit(unlist(b),
                   "[ ]{2,}")
    z <- do.call(rbind, bb)

    zz <- as.data.frame(z[-1, ],
                        stringsAsFactors = FALSE)
    colnames(zz) <- z[1, ]

    zz$pathString <- paste("proc",
                           zz$ParentProcessId ,
                           zz$ProcessId,
                           zz$Caption,
                           sep = "/")
    dendrogramData$a <- data.tree::as.Node(zz)

  })

  output$dendrogram <- collapsibleTree::renderCollapsibleTree({
    shiny::req(!is.null(dendrogramData$a))
    processView::makeTree(dendrogramData$a,
                          collapsed = FALSE)

  })

  output$dendroPlot <- shiny::renderUI({

    shiny::validate(shiny::need(height() > 1, "Height must be > 1"))
    shiny::validate(shiny::need(height() < 10000, "Height must be < 100000"))
    shiny::validate(shiny::need(is.numeric(height()), "Height must be < 100000"))

    dendHeight <- as.integer(height())
    dendHeight <- paste0(dendHeight, "px")


    collapsibleTree::collapsibleTreeOutput(session$ns('dendrogram'),
                                           height = dendHeight)

  })

}
