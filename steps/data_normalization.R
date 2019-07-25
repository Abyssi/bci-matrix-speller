#scaling center normalization 
data_normalization <- function(x, normalizer=NULL) {
  if (is.null(normalizer)) {
    scaled <- scale(x, center = T, scale = T)
  }
  else {
    scaled <- scale(x, normalizer$center, normalizer$scale)
  }
  return(list(normalizer=list(center=attr(scaled,"scaled:center"), scale=attr(scaled,"scaled:scale")), output=as.data.frame(scaled)))
}