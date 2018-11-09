find_duplicate_files <- function(x) {
  # Find the duplicate files in a folder, i.e. the files with the same md5 vlues.
  # x: a string of file folder, e.g. "C:/example_folder"
  # Return: a list of duplicate files.

  f <- dir(x, recursive = T, all.files = T, full.names = T) # file dir
  f_md5 <- sapply(f, tools::md5sum, USE.NAMES = T) # file md5 values
  f_grouped <- split(f, f_md5) # Group f by f_md5. 
  
  # Divide the list into duplicates (length > 1) and uniques (length = 1).
  d_i <- sapply(f_grouped, function(x) length(x) > 1) # index of the duplicate files
  y <- f_grouped[d_i]
  return(y)
}

d_files <- find_duplicate_files(x = "E:/iPhone_7")
