# original author: Tobias Meissner

my.libplot <- function(x, hline=25, col=rainbow(10), legend) {
  libsize <- colSums(counts(x))/1000000
  x <- barplot(libsize, col=col, xaxt="n", ylim=c(0,120))
  text(cex=0.7, x=x, y=-1.25, legend, xpd=TRUE, srt=90, adj=1, srt=60)
  abline(h=hline, lty=3, col='red')
}

my.vsdplot <- function(x, col=rainbow(10), legend) {
  boxplot(x, col=col, names=legend, las=2)
}

my.pca <- function(x, condition) {
  pca    <- prcomp(t(x), retx=T, scale.=T) # scaled pca
  scores <- pca$x[,1:3]                        # scores for first three PC's
  
  # k-means clustering [assume 3 clusters]
  km     <- kmeans(scores, centers=4, nstart=5)
  ggdata <- data.frame(scores, Cluster=km$cluster, condition=condition)
  
  # stat_ellipse is not part of the base ggplot package
  source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 
  
  ggplot(ggdata) +
    geom_point(aes(x=PC1, y=PC2, color=factor(condition)), size=5, shape=20) +
    stat_ellipse(aes(x=PC1,y=PC2,fill=factor(condition)),
                 geom="polygon", level=0.95, alpha=0.2) +
    guides(color=guide_legend("condition"),fill=guide_legend("condition"))
  
}

my.ssDist <- function(x, condition) {
  vsdMatFactors <- x
  colnames(vsdMatFactors) <- condition
  distsRL <- dist(t(vsdMatFactors))
  mat <- as.matrix(distsRL)
  cormat <- as.matrix(cor(vsdMatFactors, method='spearman'))
  hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
  heatmap.2(cormat, 
            trace="none", 
            col = rev(hmcol), 
            margin=c(14, 14),
            cexRow = 0.1 + 1/log10(nrow(x)),
            cexCol = 0.1 + 1/log10(ncol(x))
  )
}