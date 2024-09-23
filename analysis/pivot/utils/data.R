library(dplyr, quietly = T, warn.conflicts = F)
library(pgenlibr, quietly = T, warn.conflicts = F)

default.na <- function(err) NA

convert.marker.ids <- function(marker.ids) {
  # converts CSRA marker IDs to consistent format
  # used to match direct CSRA IDs to Open Cravat format
  #  - ensures chrom 23 represented as X
  #  - represents indels with "-" (e.g. 1:1:ATCA:A -> 1:2:TCA:-)
  #
  # @param marker.ids  vector of genomic marker IDs (e.g. 1:1:A:T)
  #
  # @returns  vector of converted marker IDs
  
  make.marker.id <- function(chrom, pos, ref, alt) {
    paste(gsub('23', 'X', chrom), pos, ref, alt, sep = ':')
  }

  convert.marker.repr <- function(chrom, pos, ref, alt) {
    if (nchar(ref) > nchar(alt)) {
      m <- substr(ref, 1, nchar(alt))
      if (m != alt) {
        return(make.marker.id(chrom, pos, ref, alt))
      }
      return(make.marker.id(chrom, as.integer(pos) + length(alt), gsub(paste0('^', alt), '', ref), '-'))
    }

    if (nchar(alt) > nchar(ref)) {
      m <- substr(alt, 1, nchar(ref))
      if (m != ref) {
        return(make.marker.id(chrom, pos, ref, alt))
      }
      return(make.marker.id(chrom, as.integer(pos) + length(ref), '-', gsub(paste0('^', ref), '', alt)))
    }

    return(make.marker.id(chrom, pos, ref, alt))
  }

  strsplit(marker.ids, ':') %>%
    sapply(function(x) { do.call(convert.marker.repr, as.list(x)) })
}

to.model.mtx <- function(marks) {
  # creates model matrix from named row/sample group vector
  #
  # @param  marks  named vector of row/sample groups
  
  # convert marks to factor
  marks <- factor(marks)
  # save the factor levels to rename design matrix columns
  level.names <- levels(marks)
  # create model matrix
  model.mtx <- model.matrix(~ 0 + marks) # 0 + means no intercept
  # rename the columns
  colnames(model.mtx) <- level.names
  
  model.mtx
}

alt.sums.by.var <- function(gt.mtx, marked.vars) {
  # performs dark wizardry to calculate grouped rowsums across marked variants
  #
  # this allows quick reduction of sparse matrix formats into summary matrices
  # used to count events on specific genes or gene groups by sample
  #
  # @param    gt.mtx      genotype matrix (samples x variants, 0/1/2 GT)
  # @param    marked.vars   vector of group labels w/ names indexing gt.mtx cols
  #
  # @returns  reduced matrix
  
  # to save space-time subset gt.mtx to only markers of interest
  # this also re-orders gt.mtx to match the marked.vars vector
  gt.mtx <- gt.mtx[,match(names(marked.vars), colnames(gt.mtx))]
  
  if (length(unique(marked.vars)) == 1) {
    # this dark magicks don't work with only one group
    # in this case use rowSums on our subsetted matrix
    r <- matrix(rowSums(gt.mtx), ncol = 1)
    colnames(r) <- unique(marked.vars)[1]
    return(r)
  }
  
  # create binary design matrix where columns correspond to each gene
  # the row values are 1 if marker is in gene and 0 otherwise
  # Note: the rowsums of this matrix should all be `1` since each marker is in only 1 gene
  model.mtx <- to.model.mtx(marked.vars)

  # what is a row sum really? 
  # it's the same as multiplying your matrix by an mx1 vector of 1s!
  # now if you split those ones across different columns representing groups
  # then you're row summing by groups!
  # !!! GENIUS !!!
  gt.mtx %*% model.mtx
}

alt.sums.by.sample <- function(gt.mtx, marked.samples) {
  # performs dark wizardry to calculate grouped rowsums across marked samples
  # 
  # this allows quick reduction of sparse matrix formats into summary matrices
  # used to count number of samples w/ event per marker
  #
  # @param    gt.mtx      genotype matrix (samples x variants, 0/1/2 GT)
  # @param    marked.samples   vector of group labels w/ names indexing gt.mtx rows
  #
  # @returns  reduced matrix
  
  # very similar logic to alt.sums.by.var
  gt.mtx <- gt.mtx[match(names(marked.samples), rownames(gt.mtx)),]
  
  if (length(unique(marked.samples)) == 1) {
    r <- matrix(colSums(gt.mtx), nrow = 1)
    colnames(r) <- colnames(gt.mtx)
    rownames(r) <- unique(marked.samples)[1]
    return(r)
  }
  
  model.mtx <- t(to.model.mtx(marked.samples))
  model.mtx %*% gt.mtx
}

mtx.to.df <- function(mtx, row.lbl = 'row.id', names_to = 'name', values_to = 'value') {
  # converts sparse matrix to long format (melted) dataframe
  #
  # @param    mtx       input sparse matrix
  # @param    row.lbl   column destination for rownames
  # @param    names_to  column destination for colnames
  # @param    values_to column destination for matrix values
  #
  # @returns  data.frame
  
  df <- as.data.frame(as.matrix(mtx))
  df[[row.lbl]] <- rownames(df)
  df <- pivot_longer(df, cols=-all_of(row.lbl), names_to=names_to, values_to=values_to)
  df[(!is.na(df[[values_to]])) & (df[[values_to]] != 0),]
}