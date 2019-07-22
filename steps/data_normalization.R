data_normalization <- function(x, normalizer=NULL) {as.matr
  if (is.null(normalizer)) {
    scaled <- scale(x, center = T, scale = T)
  }
  else {
    scaled <- scale(x, normalizer$center, normalizer$scale)
  }
  scaled11 <<- scaled
  return(list(normalizer=list(center=attr(scaled,"scaled:center"), scale=attr(scaled,"scaled:scale")), output=as.data.frame(scaled)))
}