#' Tidy ISASP Data By Sub-test
#'
#' @md
#'
#' @description Take the data (directly from the ISASP Extract) and tidy it by subtest.
#' This will result in a data frame that is arranged so that students have multiple rows
#' for each sub-test they've taken. All Label, pct_correct, pnt_poss, and raw_score columns
#' for all sub-tests are `tidyr::pivoted_wider()` into new columns named `test_label`,`raw_score `,
#' `points_possible`, and `pct_correct` This function is not very adaptable nor abstracted,
#' and may be replaced in a future version
#'
#' @param df A Data Frame with columns named exactly as they appear on the ISASP extract in 2019
#'
#' @return A Data Frame in the long format with columns `test_label`,`raw_score `, `points_possible`,and `pct_correct`
#'
#' @export
tidy_by_subtest <- function(df){


  usethis::ui_todo("Cleaning Column Names")
  cleaned <- df %>%
  #Transform Variable names for splitting
  janitor::clean_names() %>% janitor::clean_names() %>%
  rename(
    d1_label = math_d1label, d1_pct_correct=math_d1pct_correct, d1_pnt_poss=math_d1pnt_poss, d1_raw_score=math_d1raw_score,
    d2_label = math_d2label, d2_pct_correct=math_d2pct_correct, d2_pnt_poss=math_d2pnt_poss, d2_raw_score=math_d2raw_score,
    d3_label = math_d3label, d3_pct_correct=math_d3pct_correct, d3_pnt_poss=math_d3pnt_poss, d3_raw_score=math_d3raw_score,
    d4_label = math_d4label, d4_pct_correct=math_d4pct_correct, d4_pnt_poss=math_d4pnt_poss, d4_raw_score=math_d4raw_score,
    d5_label = math_d5label, d5_pct_correct=math_d5pct_correct, d5_pnt_poss=math_d5pnt_poss, d5_raw_score=math_d5raw_score
             )
  usethis::ui_done("Column Names Cleaned")

  usethis::ui_todo("Pivoting Multiple Columns")
  pivoted <- cleaned %>%
    pivot_longer(
    #  select columns
    #        Test Name      Raw Score          Points Possible       % correct            Subject Area
    cols = c(kid_label,     kid_raw_score,     kid_pnt_poss,         kid_pct_correct,     # Reading
             cs_label,      cs_raw_score,      cs_pnt_poss,          cs_pct_correct,      # Reading
             iki_label,     iki_raw_score,     iki_pnt_poss,         iki_pct_correct,     # Reading
             rpk_label,     rpk_raw_score,     rpk_pnt_poss,         rpk_pct_correct,     # Language/Writing
             pdw_label,     pdw_raw_score,     pdw_pnt_poss,         pdw_pct_correct,     # Language/Writing
             ttp_label,     ttp_raw_score,     ttp_pnt_poss,         ttp_pct_correct,     # Language/Writing
             coseklp_label, coseklp_raw_score, coseklp_pnt_poss,     coseklp_pct_correct, # Language/Writing
             vau_label,     vau_raw_score,     vau_pnt_poss,         vau_pct_correct,     # Language/Writing
             ls_label,      ls_raw_score,      ls_pnt_poss,          ls_pct_correct,      # Science
             ps_label,      ps_raw_score,      ps_pnt_poss,          ps_pct_correct,      # Science
             es_label,      es_raw_score,      es_pnt_poss,          es_pct_correct,      # Science
             d1_label,      d1_raw_score,      d1_pnt_poss,          d1_pct_correct,      # Math
             d2_label,      d2_raw_score,      d2_pnt_poss,          d2_pct_correct,      # Math
             d3_label,      d3_raw_score,      d3_pnt_poss,          d3_pct_correct,      # Math
             d4_label,      d4_raw_score,      d4_pnt_poss,          d4_pct_correct,      # Math
             d5_label,      d5_raw_score,      d5_pnt_poss,          d5_pct_correct,      # Math


             ),
    names_to =c("test", ".value"),
    names_sep = "_",
    values_drop_na = TRUE

  )
  usethis::ui_done("Data Pivoted")

  usethis::ui_todo("Cleaning Results")
  result <- pivoted %>%
  select (-test) %>%
  rename(test_label      = label,
         raw_score       = raw,
         points_possible = pnt,
         pct_correct     = pct
         )
  usethis::ui_done("Results Cleaned")



  return(result)
}




#' Extract Sub-Test Scores By Student
#' @md
#' @description Extract a particular column of sub-test scores.
#' Mostly useful for stacking said sub-test scores and rejoining with student data later on.
#'
#' @param df A data frame
#' @param label_col Column name of the test label for the sub-test
#' @param pct_col Column name of the test percent for the sub-test
#' @param points_col Column name of the total points possible column for the sub-test
#' @param raw_score_col Column name of the raw Score for the sub-test
#' @student_id Column name of the column containing the unique student identifier
#' @grade = Column Name containing the students grade
#'
#' @return A 6 column data frame containing the columns `student_id`, `grade`, `test_label`, `raw_score`, `pct_correct`, and `points_possible`
#'
#' @export
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
  pct_col        <- enquo(pct_col)
  student_id     <- enquo(student_id)
  grade          <- enquo(grade)

  # Select the correct columns
  sub_score <- df %>%
    select(
      !!student_id,
      !!grade,
      "test_label" = !!label_col,
      "raw_score" = !!raw_score_col,
      "pct_correct" = !!pct_col,
      "points_possible" = !! points_col)

  return(sub_score)
}

drop_filler_columns <- function(df){

  df <- df %>%
    select(-starts_with("Filler"))

  return(df)
}
