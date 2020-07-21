#' Calculate Proficiency Percentages
#'
#' @description easily calculate proficiency levels for ISASP
#'
#' @param df A data frame from the ISASP report
#' @param col the column name to calculate proficiency percentages from
#' @param by the column to aggregate by (School, Grade, Etc)
#' @param A list of levels as they appear in the data. This should not typically have to change.
#'
#' @return a data frame with aggregated percentages by the chosen grouping
#'
#' @export
proficency_pct <- function(df, col=ELAAchLvl, by=NULL, levels = c('A', 'P', 'N') ){

  # prep arguments for use in pipeline
  col <- rlang::enquo(col)
  by  <- rlang::enquo(by)

  # make sure the column is the right type
  usethis::ui_todo("Validating column_data")

  if(is.character(df%>%dplyr::select(!!col)%>%dplyr::pull())){
    usethis::ui_done("Validated")
  } else{
    usethis::ui_oops("Not a valid column")
    stop("Please check the column selected for proficency levels")
  }

  # Filter out all levles not in the argument
  usethis::ui_line("Filtering out non-valid levels")
   df <- df %>%
     dplyr::filter(!!col %in% levels)

   # Make the summary
  usethis::ui_todo("Starting Summarization")
  smry <- df %>%
    dplyr::group_by(!!by, !!col, .add = TRUE) %>%
    dplyr::count()  %>%
    tidyr::pivot_wider(names_from = !!col, values_from = n) %>%
    dplyr::mutate(percent_proficient = (1-(N/(A+N+P)))*100)
  usethis::ui_done("Summarization Complete")


  return(smry)
}

