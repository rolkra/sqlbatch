% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlbatch.R
\name{sql_run}
\alias{sql_run}
\title{Run SQL statement}
\usage{
sql_run(
  con,
  sql = NA,
  file = NA,
  clean = FALSE,
  glue = FALSE,
  simulate = FALSE
)
}
\arguments{
\item{con}{Connection to a Database}

\item{sql}{SQL statement}

\item{file}{Filename containing SQL statement}

\item{clean}{Cleaning SQL statement before execution?
(dropping of empty line, dropping of comments, ...)}

\item{glue}{Use glue function to replace placeholders in SQL with 
the value of global variables}

\item{simulate}{Just simulate, don't actually run SQL statement}
}
\value{
data (if SQL contains a SELECT statement)
}
\description{
Runs all SQL statements contained in a file or in a string (sql).
If sql contains multiple statements separated by a semicolon
all SQL statements are run in serial.
}
\examples{
\dontrun{
# run sql
sql_run("SELECT * \nFROM mytable")

# run sql with glue
sql_param_table <- "mytable"
sql_run("SELECT * \nFROM {sql_param_table}")

# run sql with clean
sql_run("-- Test \nSELECT * FROM mytable\n-- End\n", clean = TRUE)

}
}
