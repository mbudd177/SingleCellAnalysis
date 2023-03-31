#' Create cell metadata matrix
#' @export
CreateMD <- function(rna_in, prot_in){
  # create metadata of droplet QC stats used in standard scRNAseq processing
  mtgene = grep(pattern = "^MT-", rownames(rna_in), value = TRUE) # used below
  md = data.frame(
    rna.size = log10(Matrix::colSums(rna_in)),
    prot.size = log10(Matrix::colSums(prot_in)),
    n.gene = Matrix::colSums(rna_in > 0),
    mt.prop = Matrix::colSums(rna_in[mtgene, ]) / Matrix::colSums(rna_in)
  )

  # add indicator for barcodes Cell Ranger called as cells
  md$drop.class <- ifelse(rownames(md) %in% stained_cells, 'cell', 'background')

  # remove barcodes with no evidence of capture in the experiment
  md <- md[md$rna.size > 0 & md$prot.size > 0, ]
  return(md)
}
