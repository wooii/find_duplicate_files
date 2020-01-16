# This R script can be used to find and remove duplicate files in one or
# multiple folders.


# Functions -------------------------------------------------------------------

find_duplicate_files <- function(x, sub.folder = T) {
  # Find the duplicate files in one or multiple folders x, based on md5 values.
  # x: a vector containing file folders, e.g. x = c("C:/folder1", "D:/folder2")
  # sub.folder: logical. T, try to find duplicate files in the sub folders; 
  #   F: sub folders are not included for tracking down duplicate files.
  # Return: a list containing the full paths to the duplicate files.

  # Get the full paths to every file in x.
  d <- lapply(x, dir, recursive = sub.folder, all.files = T, full.names = T)
  d <- unlist(d, use.names = F)
  d.md5 <- sapply(d, tools::md5sum, USE.NAMES = T) # Get md5 values.
  d.grouped <- split(x = d, f = d.md5) # Group d by d.md5.
  i <- sapply(d.grouped, function(x) length(x) > 1) # Index for duplicates.
  y <- d.grouped[i]
  return(y)
}


# Find and remove duplicate files in selected folders -------------------------

if (F) {
  # A list of duplicate files.
  df <- find_duplicate_files(x = c("C:/folder1", "D:/folder2"), sub.folder = F)
  n <- sapply(df, length) # number of duplicates.
  df2 <- df[n == 2] # select those n == 2.
  df3 <- df[n != 2] # select those n > 2.
  
  # A vector of files to be deleted. For each duplicate, only one coply will be
  # kept, and all other copies will be deleted.
  df.r <- unlist(lapply(df, function(x) x[-1]))
  # Warning, batch deleting!!!
  # The function will permenantly delete the selected files.
  # The deleted files cannot be recovered with the recycle bin.
  if (F) {
    if (interactive() & askYesNo("Warning!!! Delete all the files?")) {
      deleted <- sapply(df.r, file.remove)
      cat("Sucessfully deleted", length(deleted == T), "files.", sep = " ")
    }
  }
}
