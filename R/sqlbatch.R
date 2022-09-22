#' Print SQL statement to console.
#'
#' @param sql SQL statement
#' @param compress Compresses SQL to max. 80 characters
#' @return SQL statement
#' @examples
#' sql_show("SELECT * \nFROM mytable")
#' @export

sql_show <- function(sql, compress = FALSE) {
  
  if (compress) {
    
    sql <- sql_clean(sql, safe = TRUE)
    sql <- stringr::str_replace_all(sql, "\n", " ")
    
    if (length(sql) > 80) {
      sql <- paste(stringr::str_sub(sql, 1,74), "...")
      
    }
  }
  
  cat(cli::col_silver(sql, "\n"))
  
}

#' Read SQL from a file
#'
#' Reading SQL statements from a text-file. 
#' The file may contain a single SQL or multiple SQL separated by semicolon
#'
#' @param file path and filename
#' @return SQL statement
#' @examples
#' \dontrun{
#' sql <- sql_read_file("C:/temp/demo.sql")
#' }
#' @export
#' 
sql_read_file <- function(file) {
  
  if (!is.vector(file)) {
    
    sql <- readr::read_file(file)
    sql  
  } else {
    
    sql_vector <- vector()
    for (i in seq_along(file)) {
      sql <- readr::read_file(file[i])
      sql_vector <- c(sql_vector, sql)
    }
    sql_vector 
  }
} # sql_read_file

#' Clean SQL statement
#'
#' @param sql SQL statement (or vector of SQL statements)
#' @param safe Only clean parts of SQL that are save (no disruption)
#' @return SQL statement
#' @examples
#' sql_clean("\n\n-- Test\nSELECT * FROM mytable")
#' @export

sql_clean <- function(sql, safe = FALSE) {
  
  done <- FALSE
  
  
  while (!done)  {
    
    sql_before <- sql
    
    # drop empty lines & tab, spaces at beginning and end
    sql <- stringr::str_replace_all(sql, "^[;\\s\\t\r\n]*", "")
    sql <- stringr::str_replace_all(sql, "[;\\s\\t\r\n]*$", "")
    
    # drop comment at beginning
    sql <- stringr::str_replace_all(sql, "^--.+\n","")
    sql <- stringr::str_replace_all(sql, "^/\\*[\\w\\W]*?(?=\\*/)\\*/", "")
  
    if (sql == sql_before) { 
      done <- TRUE 
    }
    
  } # while
  
  # clean everything else
  if (!safe) {
    sql <- stringr::str_replace_all(sql, "/\\*[\\w\\W]*?(?=\\*/)\\*/", "") ## remove /* */ comments (multi line)
    sql <- stringr::str_replace_all(sql, "\r\n","\n")       ## remove CR
    sql <- stringr::str_replace_all(sql, "--.+\n","\n")     ## remove -- comments
    sql <- stringr::str_replace_all(sql, "--.+?$","\n")     ## remove -- comments at last line (no \n)
    sql <- stringr::str_replace_all(sql, "\n+\n", "\n")     ## remove empty lines
    sql <- stringr::str_replace_all(sql, "^\n+", "")        ## remove empty lines at beginning
    sql <- stringr::str_replace_all(sql, "\\t", "    ")     ## replace tab with 4 space 
    sql <- stringr::str_replace_all(sql, ";[ ]*$", "")      ## remove ending ;
    sql <- stringr::str_replace_all(sql, ";[ ]*\n$", "")    ## remove ending ;\n
  }
  
  sql  
  
} # sql_clean

#' Split SQL statement
#' 
#' Splits SQL containing multiple statements separated by a semicolon
#' into a vector of single SQL statements.
#'
#' @param sql SQL statement
#' @return Vector of SQL statements
#' @examples
#' sql_show("SELECT * \nFROM mytable")
#' @export

sql_split <- function(sql) {
  
  # split at semicolon
  sql_list <- unlist(stringr::str_split(sql,"(?<=;)"))
  
  # remove head & tail (space, tab, CR, new line, semicolon)
  sql_list <- stringr::str_replace_all(sql_list, "^[;\\s\\t\r\n]*", "")
  sql_list <- stringr::str_replace_all(sql_list, "[;\\s\\t\r\n]*$", "")
  
  # remove empty lines
  sql_list <- sql_list[sql_list != ""]
  
  # return
  sql_list
}

#' Run SQL statement
#' 
#' Runs all SQL statements contained in a file or in a string (sql).
#' If sql contains multiple statements separated by a semicolon
#' all SQL statements are run in serial.
#'
#' @param con Connection to a Database
#' @param sql SQL statement
#' @param file Filename containing SQL statement
#' @param clean Cleaning SQL statement before execution?
#' (dropping of empty line, dropping of comments, ...)
#' @param glue Use glue function to replace placeholders in SQL with 
#' the value of global variables
#' @param simulate Just simulate, don't actually run SQL statement
#' @return data (if SQL contains a SELECT statement)
#' @examples
#' \dontrun{
#' # run sql
#' sql_run("SELECT * \nFROM mytable")
#' 
#' # run sql with glue
#' sql_param_table <- "mytable"
#' sql_run("SELECT * \nFROM {sql_param_table}")
#' 
#' # run sql with clean
#' sql_run("-- Test \nSELECT * FROM mytable\n-- End\n", clean = TRUE)

#' 
#' }
#' @export

sql_run <- function(con, sql = NA, file = NA, clean = FALSE, glue = FALSE, 
                    simulate = FALSE) {
  
  if (all(is.na(sql)) & all(is.na(sql))) {
    stop("Either sql or file must be defined")
  }
  
  if (!all(is.na(file))) {
    cat("Reading SQL from", basename(file))
    sql <- sql_read_file(file)
  }
  
  # glue sql
  if (glue) {
    sql <- glue::glue(sql)
  }
  
  # clean and split sql
  sql <- sql_clean(sql, safe = TRUE)
  sql_list <- sql_split(sql)
  
  if (clean) { 
    sql_list <- sql_clean(sql_list, safe = TRUE) 
  } else {
    sql_list <- sql_clean(sql_list, safe = FALSE)
  }

  # init
  t1 <- Sys.time()
  data <- NA
  
  if (!all(is.na(file))) {
    cat(">> File", basename(file), "\n")
  }
  
  # run all parts of sql seperately
  for (i in seq_along(sql_list)) {
    
    cat(paste0(">> Part ", i, " of ", length(sql_list), ": "))
    sql <- sql_list[i]
    
    if (clean) { 
      sql <- sql_clean(sql, safe = FALSE) 
    } else {
      sql <- sql_clean(sql, safe = TRUE)
    }
    
    sql_command <- stringr::str_extract(sql, "^[a-zA-Z]+[ ]")
    sql_command <- stringr::str_to_upper(stringr::str_trim(sql_command))
    sql_command  
    
    cat("\n")
    sql_show(sql, compress = TRUE)
    
    if (is.na(sql_command))  {
      
      cat("NOT TO EXECUTE\n")
      
    } else if (sql_command == "SELECT") {
      
      cat("SELECT ... ")
      
      if (!simulate) {
         data <- DBI::dbGetQuery(con, sql)
      } 
      
      if (is.data.frame(data)) {
        cat(nrow(data), "records\n")
      } else {
        cat("NO DATA FOUND\n")
      }
      
    } else if (sql_command == "DROP") {
      
      cat("DROP (silent) ...\n")
      if (!simulate) {
         out <- try(DBI::dbExecute(con, sql), silent = TRUE)
      }
      
    } else {
      
      cat(sql_command, "...\n")
      if (!simulate) {
        DBI::dbExecute(con, sql)
      }
    }
  } # for
  
  # how log did it take
  t2 <- Sys.time()
  t <- difftime(t2, t1, unit="min")[[1]]
  cat(">> Finished in", 
      format(round(t,1), big.mark = " ", decimal.mark = "."),
      "minutes\n")
  
  # return data, if no select then NA
  data
  
} # sql_run

#' Run all SQL statement
#' 
#' Runs all SQL statements in a vector of files or in a vectot of strings.
#' If sql contains multiple statements separated by a semicolon
#' all SQL statements are run in serial.
#'
#' @param con Connection to a Database
#' @param sql Vector of strings containing SQL statements 
#' (either single statements or multiple statements separated by a semicolon)
#' @param file Vector of filename containing SQL statement
#' @param clean Cleaning SQL statement before execution?
#' (dropping of empty line, dropping of comments, ...)
#' @param simulate Just simulate, don't actually run SQL statement
#' @return data (if SQL contains a SELECT statement) otherwise NA
#' @examples
#' \dontrun{
#' sql_run("SELECT * \nFROM mytable")
#' }
#' @export

sql_batch <- function(con, sql = NA, file = NA, clean = FALSE, simulate = FALSE)  {

  if (all(is.na(sql)) & all(is.na(sql))) {
    stop("Either sql or file must be defined")
  }

  if (!is.na(file)) {
    cat("Reading", length(file), "files ...\n")
    sql <- sql_read_file(file)
  }
  
  t1_total <- Sys.time()
  
  for (i in seq_along(sql))  {
    
    if (!is.na(file)) {
      cat("> File", basename(file[i]), 
          paste0( "(",i," of ", length(file),")\n")
      )
    } else {
      cat("> Run SQLs", i, "of", length(sql), "\n")
    }
    
    result <- sql_run(con, sql = sql[i], clean = clean, simulate = simulate)
    
  } # for
  
  t2_total <- Sys.time()
  t_total <- difftime(t2_total, t1_total, unit="min")[[1]]
  cat("Finished in", 
      format(round(t_total,1), big.mark = " ", decimal.mark = "."),
      "minutes\n")
  
  
  result
  
} # sql_batch
