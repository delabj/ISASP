ISASP_DATA %>%
  pivot_wider(names_from = kid)
  pivot_longer( names_to = "Test", values_to = "value")




#' Extract Subtest Scores By Student
#'
#' @description Extract a particular column of subscores. Mostly useful for stacking said subscores and rejoining with student data
#'
student_subtest_score <- function(df,
                                  label_col=NULL,
                                  pct_col=NULL,
                                  points_col=NULL,
                                  raw_score_col=NULL,
                                  student_id=StateID,
                                  grade=Grade){

  # equo all options
  label_col      <- enquo(label_col)
  points_col     <- enquo(points_col)
  raw_score_col  <- enquo(raw_score_col)
  student_id     <- enquo(student_id)
  grade          <- enquo(grade)

  # Select tje cprrect columns
  sub_score <- df %>%
    select(
      !!student_id,
      !!grade,
      "test_label" = !!label_col,
      "raw_score" = !!raw_score_col,
      "pct_correct" = !!points_col,
      "points_possible" = !! points_col)

  return(sub_score)
}

drop_filler_columns <- function(df){

  df <- df %>%
    select(-starts_with("Filler"))

  return(df)
}
