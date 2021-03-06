% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_ISASP_data.R
\name{tidy_by_subtest}
\alias{tidy_by_subtest}
\title{Tidy ISASP Data By Sub-test}
\usage{
tidy_by_subtest(df)
}
\arguments{
\item{df}{A Data Frame with columns named exactly as they appear on the ISASP extract in 2019}
}
\value{
A Data Frame in the long format with columns \code{test_label},\code{raw_score }, \code{points_possible},and \code{pct_correct}
}
\description{
Take the data (directly from the ISASP Extract) and tidy it by subtest.
This will result in a data frame that is arranged so that students have multiple rows
for each sub-test they've taken. All Label, pct_correct, pnt_poss, and raw_score columns
for all sub-tests are \code{tidyr::pivoted_wider()} into new columns named \code{test_label},\code{raw_score },
\code{points_possible}, and \code{pct_correct} This function is not very adaptable nor abstracted,
and may be replaced in a future version
}
