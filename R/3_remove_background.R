#' Remove background noise
#' @export
RemoveBackground <- function(meta.data, background, adt.matrix, rna.matrix, mt.pct = 0.14){

  background.adt.mtx <- as.matrix(adt.matrix[ , background])

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

  return(qc_filtered)
}
