library(sqlbatch)

test_that("sql_clean(safe = TRUE) works", {

  sql_clean <- "select '--/**/;\n' as str from table"
    
  # no cleaning necessary
  sql <- "select '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop empty lines
  sql <- "\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- "\n\n\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop spaces
  sql <- " select '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- "   select '--/**/;\n' as str from table   "
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop ; at end
  sql <- " select '--/**/;\n' as str from table;"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- " select '--/**/;\n' as str from table;;;"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop comments --
  sql <- "-- comment\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- "-- comment\n-- comment 2\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop comments /* */
  sql <- "/* comment */\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- "/* comment */\n/*comment 2 */\nselect '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  
  # drop ;
  sql <- ";select '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
  sql <- ";;;select '--/**/;\n' as str from table"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)
 
  # mix
  # drop comments /* */
  sql <- "\n  ; /* comment */\nselect '--/**/;\n' as str from table ;; \n\n"
  expect_equal(sql_clean(sql, safe = TRUE), sql_clean)

})
  