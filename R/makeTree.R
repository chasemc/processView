
#' Fast version of collapsibleTree.Node from collapsibleTree package
#'
#' @param df see https://github.com/AdeelK93/collapsibleTree
#' @param hierarchy_attribute see https://github.com/AdeelK93/collapsibleTree
#' @param root see https://github.com/AdeelK93/collapsibleTree
#' @param inputId see https://github.com/AdeelK93/collapsibleTree
#' @param attribute see https://github.com/AdeelK93/collapsibleTree
#' @param aggFun see https://github.com/AdeelK93/collapsibleTree
#' @param fill see https://github.com/AdeelK93/collapsibleTree
#' @param linkLength see https://github.com/AdeelK93/collapsibleTree
#' @param fontSize see https://github.com/AdeelK93/collapsibleTree
#' @param tooltip see https://github.com/AdeelK93/collapsibleTree
#' @param tooltipHtml see https://github.com/AdeelK93/collapsibleTree
#' @param nodeSize see https://github.com/AdeelK93/collapsibleTree
#' @param collapsed see https://github.com/AdeelK93/collapsibleTree
#' @param zoomable see https://github.com/AdeelK93/collapsibleTree
#' @param width see https://github.com/AdeelK93/collapsibleTree
#' @param height see https://github.com/AdeelK93/collapsibleTree
#' @param ... see https://github.com/AdeelK93/collapsibleTree
#'
#' @return see https://github.com/AdeelK93/collapsibleTree
#' @export
#'
makeTree <- function(df,
                     hierarchy_attribute = "level",
                     root = df$name,
                     inputId = NULL,
                     attribute = "leafCount",
                     aggFun = sum,
                     fill = "lightsteelblue",
                     linkLength = NULL,
                     fontSize = 10,
                     tooltip = FALSE,
                     tooltipHtml = NULL,
                     nodeSize = NULL,
                     collapsed = TRUE,
                     zoomable = TRUE,
                     width = NULL,
                     height = NULL, ...) {


  # Modified, faster version of function from https://github.com/AdeelK93/collapsibleTree

  # acceptable inherent node attributes
  nodeAttr <- c("leafCount", "count")

  # calculate the right and left margins in pixelsprocessView
  leftMargin <- nchar(root)
  #  rightLabelVector <- df$Get("name", filterFun = function(x) x$level==df$height)
  rightMargin <- 20

  # Deriving hierarchy variable from data.tree input
  hierarchy <- unique(data.tree::ToDataFrameTree(df, hierarchy_attribute)[[hierarchy_attribute]])
  if(length(hierarchy) <= 1) stop("hierarchy vector must be greater than length 1")

  # create a list that contains the options
  options <- list(
    hierarchy = hierarchy,
    input = inputId,
    attribute = attribute,
    linkLength = linkLength,
    fontSize = fontSize,
    tooltip = tooltip,
    collapsed = collapsed,
    zoomable = zoomable,
    margin = list(
      top = 20,
      bottom = 20,
      left = (leftMargin * fontSize/2) + 25,
      right = (rightMargin * fontSize/2) + 25
    )
  )

  # these are the fields that will ultimately end up in the json
  jsonFields <- NULL

  if(fill %in% df$fields) {
    # fill in node colors based on column name
    df$Do(function(x) x$fill <- x[[fill]])
    jsonFields <- c(jsonFields, "fill")
  } else {
    # default to using fill value as literal color name
    options$fill <- fill
  }




  # if collapsed is specified, pass it on in the data
  if(is.character(collapsed)) jsonFields <- c(jsonFields, collapsed)



  # keep only the JSON fields that are necessary
  if(is.null(jsonFields)) jsonFields <- NA
  data <- data.tree::ToListExplicit(df, unname = TRUE, keepOnly = jsonFields)

  # pass the data and options using 'x'
  x <- list(
    data = data,
    options = options
  )

  # create the widget
  htmlwidgets::createWidget(
    "collapsibleTree", x, width = width, height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 0)
  )
}
