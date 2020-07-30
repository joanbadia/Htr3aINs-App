
#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param samples character vector of length > 1, names that will after be used for indexing or filtering.
#' @param labels character vector of length(samples), labels that indicate the identity of each sample. Will be factorised.
#' @param pct numeric between 0 to 1, ratio that indicates how many samples you wish to have in the training set.
#' @keywords plot

split.samples <- function(samples, labels, pct=0.66)
{
  cells.trn <- list()
  for(i in levels(factor(labels))){
    x.sub <- samples[labels == i]
    cells.trn[[i]] <- sample(x.sub, size=round(length(x.sub)*pct))}

  list(train=samples[samples %in% unlist(cells.trn)], test=samples[!(samples %in% unlist(cells.trn))])
}



#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param samples character vector of length > 1, names that will after be used for indexing or filtering.
#' @param labels character vector of length(samples), labels that indicate the identity of each sample. Will be factorised.
#' @param pct numeric between 0 to 1, ratio that indicates how many samples you wish to have in the training set.
#' @keywords plot

svm.weights <- function(x.trn,y.trn,...) {
  loss.weights <- ifelse(y.trn,1/sum(y.trn),1/sum(!y.trn))/2*100

  # w <- nrbm(hingeLoss(cbind(intercept=1000,x.trn),y.trn,loss.weights=loss.weights),...)
  # names(w) <- c("intercept",colnames(x.trn))
  w <- nrbm(hingeLoss(x.trn,y.trn,loss.weights=loss.weights),...)
  names(w) <- colnames(x.trn)

  # attr(w, "rank") <- pmin(rank(+w,ties.method="first"),rank(-w,ties.method="first"))

  w
}



#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param samples character vector of length > 1, names that will after be used for indexing or filtering.
#' @param labels character vector of length(samples), labels that indicate the identity of each sample. Will be factorised.
#' @param pct numeric between 0 to 1, ratio that indicates how many samples you wish to have in the training set.
#' @keywords plot

svm.predict <- function(w, x.tst, y.tst){
  # p <- predict(w, cbind(intercept=1000,x.tst))
  p <- predict(w, x.tst)
  roc.stat(attr(p,"decision.value"), y.tst)
}








#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param samples character vector of length > 1, names that will after be used for indexing or filtering.
#' @param labels character vector of length(samples), labels that indicate the identity of each sample. Will be factorised.
#' @param pct numeric between 0 to 1, ratio that indicates how many samples you wish to have in the training set.
#' @keywords plot

genes.stats <- function(m, contrast, w, expr.threshold=0.5, foldchange.threshold=2,weight.threshold=0.05){
  x <- m[,contrast]
  y <- m[,!(contrast)]
  freq.x <- rowMeans(x > expr.threshold)
  freq.y <- rowMeans(y > expr.threshold)
  log2FC <- rowMeans(x) - rowMeans(y)

  res <- data.frame(genes=names(log2FC),log2FC, weight=as.vector(w), freq.x, freq.y, stringsAsFactors = F, check.names = F, check.rows = F)
  res$selected <- abs(res$weight) > 0.05 & abs(res$log2FC) > 1
  res$label <-ifelse(res$selected, rownames(res), "")
  res$key <- res$genes
  res
}





GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

# GetColorHexAndDecimal("lightblue") #ADD8E6
# GetColorHexAndDecimal("lightgoldenrod") #EEDD82



.util.aggregate <- function(..., group=NULL){
  l <- list(...)
  if(length(l) == 0) 
    stop("provide objects to aggregate")
  if(is.null(group)) 
    group <- sprintf("g.%1s", 1:length(l))
  
  L <- do.call(rbind, lapply(1:length(l), function(ll) cbind(gene=l[[ll]], comparison=group[ll])))
  as.data.frame(aggregate(comparison ~ gene, data=L, FUN=table))
}


.logical.aggregate <- function(..., group=NULL){
  l <- list(...)
  if(length(l) == 0) 
    stop("provide objects to aggregate")
  if(is.null(group)) 
    group <- sprintf("g.%1s", 1:length(l))
  
  LL <- do.call(rbind, lapply(1:length(l), function(ll) cbind(gene=l[[ll]], comparison=group[ll])))
  as.data.frame(aggregate(comparison ~ gene, data=LL, FUN=table))
}
