#' Load data
#'
#' This function takes in directory pointers for the raw and filtered expression
#' matrices and splits them into the RNA and surface protein components. It
#' also separates empty droplets from cell-containing droplets and stores them
#' as separate objects for background normalization downstream.
#' @param raw_dir Path to the raw expression matrix
#' @param filtered_dir Path to the filtered expression matrix
#' @return De-multiplexed protein and RNA expression matrices,
#' @export
load_data <- function(raw_dir, filtered_dir){
  # Create UMI count matrices from Cell Ranger outputs
  raw <<- Seurat::Read10X(data.dir = raw_dir)
  cells <<- Seurat::Read10X(data.dir = filtered_dir)

  # define cell-containing barcodes and separate cells and empty drops
  stained_cells <<- colnames(cells$'Gene Expression')
  background <<- dplyr::setdiff(colnames(raw$'Gene Expression'), stained_cells)

  # split the data into separate matrices for RNA and ADT
  prot <<- raw$'Antibody Capture'
  rna <<- raw$'Gene Expression'
}
