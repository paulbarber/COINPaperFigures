plotHRTable <- function(dataIn, cex = 1.0, rowspacing = 0.5, col = 1, xmin = -2.3, xmax = 3.5,
                        hrTicks = c(0.2, 0.5, 1, 2, 5, 10),
                        covariatepos = xmin, classpos = -0.8, hrpos = 1.9, pvaluepos = 3.1,
                        weightpos = 4.0, rankpos = 4.7,
                        useClass = FALSE, useWeight = FALSE, useRank = FALSE){
# Expects a data frame in this format:
#mydf <- data.frame(
#  Covariate = FRET_LCA$Covname,
#  Class = FRET_LCA$Class,
#  HazardRatio = FRET_LCA$HR,
#  HazardLower = first_number(FRET_LCA$X95.CI, decimals=T),
#  HazardUpper = nth_number(FRET_LCA$X95.CI, 2, decimals=T),
#  Pvalue = FRET_LCA$p.value,
#  stringsAsFactors=FALSE
#)

# rowspacing Defines the positions of the rows
  
mydf <- dataIn

# everything is placed on the plot of HR, this has log x axis
# it is convenient to think of something being at position 2, then the real pos on the plot is 10^2

# extremes of the page
xmin_v <- 10^xmin
xmax_v <- 10^xmax


nrows = nrow(mydf)
rowseq <- seq(nrows*rowspacing, 0.1, length.out = nrows)
gvec <- seq(1, nrows, by=2)  # Choose alternate rows to put a grey box behind
greyseq <- rowseq[gvec] - rowspacing/2
title_pos = (nrows + 1.2) * rowspacing
pos4offset = -2*rowspacing/19

# do the HR plot
par(mai=c(1,0,0,0))
plot(NULL, xlim=c(xmin_v, xmax_v), ylim=c(0, title_pos+rowspacing),
     xlab='', ylab='', yaxt='n', xaxt='n',
     bty='n', log = 'x')
rect(xleft = xmin_v, xright = xmax_v, 
     ytop = greyseq, ybottom = greyseq + rowspacing,
     col="gray90", border=NA)
points(mydf$HazardRatio, rowseq, pch=18, col=col)
axis(1, hrTicks, labels = as.character(hrTicks), cex.axis=cex)

# add vertical line at HR=1
segments(1, -1, 1, (nrows + 0.5) * rowspacing, lty=3)

# add confidence interval lines
segments(mydf$HazardLower, rowseq, mydf$HazardUpper, rowseq, col=col)

# x axis legend
mtext('HR', 1, line=2.5, at=1, cex=cex, font=2)

# other cols

pos <- 10^covariatepos
text(pos, title_pos+pos4offset, "Covariate", cex=cex, font=2, pos=4)
t <- ifelse(!is.na(mydf$Covariate), mydf$Covariate, '')
text(pos, rowseq+pos4offset, t, cex=cex, font=1, pos=4)  # set font=3 for italic

if(useClass){
  pos <- 10^classpos
  text(pos, title_pos, "Class", cex=cex, font=2)
  t <- ifelse(!is.na(mydf$Class), format(mydf$Class, big.mark=","), '')
  text(pos, rowseq, t, cex=cex)
}

pos <- 10^hrpos
text(pos-11, title_pos, "Hazard Ratio (95% CI)", cex=cex, font=2)
t <- ifelse(!is.na(mydf$HazardRatio), with(mydf, paste(format(round(HazardRatio, 2), nsmall = 2),' (',
                                                       format(round(HazardLower, 2), nsmall = 2),'-',
                                                       format(round(HazardUpper, 2), nsmall = 2),')',sep='')), '')
text(pos, rowseq, t, cex=cex)

pos <- 10^pvaluepos
text(pos, title_pos, "p-value", cex=cex, font=2)
t <- ifelse(!is.na(mydf$Pvalue), mydf$Pvalue, '')
text(pos, rowseq, t, cex=cex)

if(useWeight){
  pos <- 10^weightpos
  text(pos, title_pos, "Weight", cex=cex, font=2)
  t <- ifelse(!is.na(mydf$Weight), format(mydf$Weight, big.mark=","), '')
  text(pos, rowseq, t, cex=cex)
}

if(useRank){
  pos <- 10^rankpos
  text(pos, title_pos, "Rank", cex=cex, font=2)
  t <- ifelse(!is.na(mydf$Rank), format(mydf$Rank, big.mark=","), '')
  text(pos, rowseq, t, cex=cex)
}


}