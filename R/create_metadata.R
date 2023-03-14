add_md <- function(){
  # create metadata of droplet QC stats used in standard scRNAseq processing
  mtgene = grep(pattern = "^MT-", rownames(rna), value = TRUE) # used below
  md = data.frame(
    rna.size = log10(Matrix::colSums(rna)),
    prot.size = log10(Matrix::colSums(prot)),
    n.gene = Matrix::colSums(rna > 0),
    mt.prop = Matrix::colSums(rna[mtgene, ]) / Matrix::colSums(rna)
  )

  # add indicator for barcodes Cell Ranger called as cells
  md$drop.class = ifelse(rownames(md) %in% stained_cells, 'cell', 'background')

  # remove barcodes with no evidence of capture in the experiment
  md = md[md$rna.size > 0 & md$prot.size > 0, ]
}
