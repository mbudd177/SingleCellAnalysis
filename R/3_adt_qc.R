#' Remove background noise
#' @export
RemoveBackground <- function(meta.data, background, adt.matrix, rna.matrix, mt.pct = 0.14){

  # calculate statistical thresholds for droplet filtering.
  cellmd <- meta.data[meta.data$drop.class == 'cell', ]

  # filter drops with +/- 3 median absolute deviations from the median library
  # size
  rna.mult <- (3*mad(cellmd$rna.size))
  prot.mult <- (3*mad(cellmd$prot.size))
  rna.lower <- median(cellmd$rna.size) - rna.mult
  rna.upper <- median(cellmd$rna.size) + rna.mult
  prot.lower <- median(cellmd$prot.size) - prot.mult
  prot.upper <- median(cellmd$prot.size) + prot.mult

  # filter rows based on droplet quality control metrics
  qc_filtered <- rownames(
    cellmd[cellmd$prot.size > prot.lower &
             cellmd$prot.size < prot.upper &
             cellmd$rna.size > rna.lower &
             cellmd$rna.size < rna.upper &
             cellmd$mt.prop < mt.pct, ]
  )

  message <- "The number of cells passing QC is: "
  print(paste0(message, length(qc_filtered)))

  qc.data <- list(
    background.adt.mtx = as.matrix(adt.matrix[ , background]),
    cell.adt.raw = as.matrix(adt.matrix[ ,qc_filtered]),
    cell.rna.raw = rna.matrix[ ,qc_filtered],
    cellmd = cellmd[qc_filtered, ]
  )

  return(qc.data)
}

#' @export
CheckStains <- function(counts, num = 10) {
  pm <- sort(apply(counts, 1, max))
  print(head(pm, num))
  hist(pm)
}
