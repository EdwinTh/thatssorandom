#' Select columnst from a data frame.
#'
#' Shiny gadget that will show the data frame in a miniUI. Select the columns
#' that should be kept and hit Done.
#' @param df the \code{data.frame} you would like to subset.
#' @param ret should the subsetted data frame or the \code{dplyr}
#' code to make the subset be returned?
#' @param top_n for performance \code{col_select} only shows the first n
#' records, here you specify how many records you'd like to see. Default is 100.
#' @seealso \url{https://edwinth.github.io/blog/column-selecter/}
#' @examples
#' col_select(mtcars)
#' col_select(mtcars, 'dplyr_code')

col_select <- function(df,
                       ret = c("df_select", "dplyr_code"),
                       top_n = 100) {
  ret <- match.arg(ret)
  stopifnot(is.data.frame(df))
  df_head <- head(df, top_n)

  ui <- miniPage(
    gadgetTitleBar("Have your pick"),
    miniContentPanel(
      dataTableOutput("selection_df", height = "100%")
    )
  )

  server <- function(input, output, session){
    options(DT.options = list(pageLength = 10))
    output$selection_df <- DT::renderDataTable(
      df_head, server = FALSE, selection = list(target = "column")
    )
    observeEvent(input$done, stopApp(  input$selection_df_columns_selected))
  }

  cols_selected <- runGadget(ui, server)

  if (ret == "df_select") {
    return( df %>% select(cols_selected) )
  } else {
    df_name <- deparse(substitute(df))
    colnames_selected <-  colnames(df)[cols_selected] %>%
      paste(collapse = ", ")
    rstudioapi::insertText(
      paste(df_name, " %>% select(", colnames_selected, ")", sep = "")
    )
  }
}
