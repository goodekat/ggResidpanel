
specify_dim <- function(nrow, ncol, default.ncol, default.nrow, nplots){

    if (is.na(ncol) & is.na(nrow)){
      ncol = default.ncol
      nrow = default.nrow
    } else if (is.na(ncol) & !is.na(nrow)){
      ncol = ceiling(nplots/nrow)
    } else if (!is.na(ncol) & is.na(nrow)){
      nrow = ceiling(nplots/ncol)
    }

  return(data.frame(nrow, ncol))

}
