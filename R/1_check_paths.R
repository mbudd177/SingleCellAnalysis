#' Validate barcode inputs
#'
#'CheckPaths will verify that the specified input path contains the necessary
#'barcode matrices, and then return a console output to verify that all files
#'were found. Will produce an error if the path is invalid or a file is missing.
#'
#' @export
CheckPaths <- function(input) {
  df <- data.frame("Dir exists" = dir.exists(input),
                       "barcodes.tsv.gz found" = "barcodes.tsv.gz" %in% list.files(input),
                       "features.tsv.gz found" = "features.tsv.gz" %in% list.files(input),
                       "matrix.mtx.gz found" = "matrix.mtx.gz" %in% list.files(input))
  stopifnot(all(df))
  return(df)
}
