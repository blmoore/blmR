########## hvl.R ##################
# Function repository for code    #
# that's reused across scripts.   #
###################################

#' Load all packages for project
#'
#' Load all packages used in higher order project. Probably a 
#' good way to test if you have all installed. Aside, could take
#' a parameter to specify which project you're loading packages for
#' and what to do if they're missing (try to install?). N.B. 
#' Obviously not all of these packages required in each script.
#' 
#' @examples
#' \dontrun{blmR::load.all.packages() }
load.all.pkgs <- function(){
  # fn courtesy of: http://stackoverflow.com/a/9341833/1274516
  pkgs <- c("caret", "corrgram", "dplyr", "fGarch", "ggplot2", 
            "gridExtra", "MASS", "plotrix", "randomForest", "reshape2",
            "RColorBrewer", "scales", "snow", "stats", "RHmm", "lavann")
  
  pkgTest <- function(x) {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  
  pkgTest <- Vectorize(pkgTest)
  pkgTest(pkgs)
  invisible()
}

#' Panel drop-in for corrgram
#'
#' Helps with overplotting for high density scatterplots. 
#' Use in e.g. upper.panel=panel.pts2
#' @export
#' @examples
#' library("corrgram")
#' 
#' corrgram(matrix(rnorm(10000), 2000), 
#'   upper.panel=panel.pts2, 
#'   lower.panel=panel.pts)
panel.pts2 <- function (x, y, corr = NULL, col.regions, ...){
  if (!is.null(corr)) 
    return()
  plot.xy(xy.coords(x, y), type = "p", col=rgb(0.6,0.6,0.6,0.3),...)
  box(col = "white")
}

#' Barplot with standard errors
#' 
#' Takes a wide-format matrix and builds a bar chart with standard errors
#' i.e. used for plotting model results and variable important with k-fold
#' cross-validation results
#' @export
#' @param any Numeric matrix in wide format
#' @examples
#' barsWErrors(matrix(rnorm(100, mean=2, sd=.5), ncol=10))
barsWErrors <- function(any, ...){
  require("plotrix")
  # from glasso.R
  any <- any[rowSums(any) != 0,]
  ind <- sort(rowMeans(any), index.return=T)
  avs <- sort(rowMeans(any))
  bars <- barplot(avs, horiz=T, las=1, cex.names=0.7, ...,
                  xlim=c(0, max(rowMeans(any)+.1*max(rowMeans(any)))))
  #names.arg=colnames(any)[ind])
  arrows(avs+(1.96*apply(any[ind$ix,], 1, std.error)), bars, 
         avs-(1.96*apply(any[ind$ix,], 1, std.error)), bars,
         angle=90, code=3, length=0.03, lwd=1)
}

#' Call compartment state sequence
#' 
#' Uses an HMM to call the most likely state sequence
#' from a numeric vector of observed compartment eigenvectors.
#' 
#' @export
#' @param any Numeric matrix in wide format
#' @return data.frame with eigen and state
#' @examples
#' barsWErrors(matrix(rnorm(100, mean=2, sd=.5), ncol=10))
callStates <- function(indat){
  require("RHmm")
  set.seed(42)
  hmm <- HMMFit(indat, dis="NORMAL", nStates=2, control=list(init="RANDOM", iter=5000))
  calls <- viterbi(hmm, indat)
  out <- data.frame(eigen=indat, state=calls$states)
  out
}

#' Circular permutation of a vector
#' 
#' Takes a vector and shifts it \code{n} places,
#' wrapping shifted cells to effect a "circular permutation".
#' 
#' @param vec vector to permute
#' @param n number of positions to shift
#' @keywords permute
#' @export
#' @examples
#' circPermute(1:10, 3)
circPermute <- function(vec, n){
  c(vec[(length(vec)-n+1):length(vec)], vec[1:(length(vec)-n)])
}


getAUC.gen <- function(cv.out){
  require("ROCR")
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  g.pred <- prediction(range01(cv.out[,1]), 
                       ifelse(cv.out[,2]>0,1,0))
  g.perf <- performance(g.pred, "tpr", "fpr")
  auc <- attr(performance(g.pred, "auc"), "y.values")[[1]]
  auc
}

#' Add linear regression eqn to ggplot2 graph
#' 
#' Use with e.g. \code{annotate} to add a linear regression
#' equation and \eqn{R^2}{R^2} value to a plot.
#' 
#' @examples 
#' library("ggplot2")
#' 
#' set.seed(42)
#' df <- data.frame(x=1:10, y=1:10*rnorm(10))
#' 
#' ggplot(df, aes(x=x, y=y)) + geom_point() +
#'  geom_smooth(method="lm", se=F) +
#'  annotate("text", x=4, y=10, label=lm_eqn(lm(y ~ x, df)), parse=T)
#' 
#' @references SO answer by user Jayden and others: \url{http://stackoverflow.com/a/13451587/1274516}
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

#' randomForest modelling of eigenvectors
#' 
#' \strong{Deprecated}, use \code{modelEigens.all}. This version creates manual
#' folds for \eqn{k}{k}-fold cross-validation (default k = 10), manual
#' as opposed to using \code{caret} methods correctly. Cross-validation
#' isn't really necessary with a \code{randomForest} approach as each 
#' tree is tested on out-of-bag (OOB) data and this can be averaged over
#' the forest to reproduce essentially a bootstrapped, many sample 
#' cross-validation scheme.
#' 
#' @param dat \code{data.frame} in the format \eqn{[Y, X_1, X_2, ... X_n]}{[Y, X_1, X_2, ... X_n]}
#' @param k number of folds
#' 
#' @export
#' @seealso \code{\link{modelEigens.all}}
#' 
#' @examples
#' data("blmR")
#' modelEigens(h.dat)
modelEigens <- function(all.dat, n=10, ...){
  require("caret")
  require("randomForest")

  nfolds <- n
  folds <- createFolds(seq(1,nrow(all.dat)), nfolds)  
  rf.res <- matrix(nrow=nrow(all.dat), ncol=1)
  rf.imp <- matrix(nrow=ncol(all.dat)-1, ncol=nfolds)
  cors <- c()
  pb  <- txtProgressBar(min=0, max=n, style=3)
  for ( j in 1:nfolds) {
    train <- all.dat[-folds[[j]],]
    test <- all.dat[folds[[j]],]
    rfmod <- randomForest(eigen~., data=train, importance=T)
    rfpred <- as.vector(predict(rfmod, test[,2:ncol(all.dat)], type="response"))
    rf.res[folds[[j]]] <- rfpred
    ## nb first column: Mean decrease in accuracy,
    ##    second col  : Mean decrease in MSE.
    rf.imp[,j] <- rfmod$importance[,1]
    #models[[j]] <- rfmod
    cors <- c(cors, cor(rfpred, test$eigen))
    setTxtProgressBar(pb, j)
  }
  rf.res <- cbind(rf.res, all.dat$eigen)
  rownames(rf.imp) <- names(rfmod$importance[,1])
  return(list(preds=rf.res, imp=rf.imp, data=all.dat, f.cors=cors))
}

#' Build Random Forest to predict compartments
#' 
#' Uses the Random Forest library to build a model
#' of a properly-formatted *dat data.frame (i.e., 
#' contains "eigen" column (y) and all other columns
#' are named features).
#' 
#' @export
#' @param all.dat Numeric data.frame with one feature per col
#' @return data.frame model with importance metrics 
#' @examples
#' modelEigens.all(h.dat)
modelEigens.all <- function(all.dat, n=500, ...){
  require("randomForest")
  all.mod <- randomForest(eigen~., data=all.dat, ntree=n, do.trace=25, keep.inbag=T, 
                          keep.forest=T, nPerm=5, importance=T, ...)
  return(all.mod)
}


ntocol <- function(cname, alp=40){
  return(rgb(matrix(col2rgb(muted("red")), ncol=3), alpha=alp, max=255))
}

plotPredRes.hist <- function (modelOut, col="blue", ct="H1"){
  d1 <- modelOut$preds[,1]
  d2 <- modelOut$preds[,2]
  colour  <- "#0000ff42"
  if ( col != "blue" ) {
    ifelse(col == "red", colour <- "#ff000042", colour  <- "#FFA50062")
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  xhist <- hist(d1, plot=FALSE)
  yhist <- hist(d2, breaks=100, plot=F)
  top <- max(c(xhist$counts, yhist$counts))
  nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(6,1), c(1,6), TRUE)
  par(mar=c(5,5,0,0), mgp=c(1.8,.4,0))
  plot(d1, d2, xlab="Predicted eig", ylab="Empirical eig", col=colour, 
       pch=16, type='n', xlim=c(-2.2,2.2), ylim=c(-2.6,2.6))
  abline(h=0,v=0, lty=2)
  points(d1, d2, col=colour, pch=16)
  #textlabs <- if (ct == "H1") c(-.9,2.2,-.8,1.8,-.95,1.4) else
  #  if (ct == "K562") c(-1.2,2.1,-1.05,1.75,-1.25,1.4) else 
  #    c(-1,1.9,-1,1.58,-1.15,1.3)
  textlabs <- c(-1.5,2.2,-1.2,1.6, -1.3,1.3)
  text(textlabs[1],textlabs[2], ct, font=2, cex=1.2)
  text(textlabs[3], textlabs[4], paste("PCC = ", signif(cor(d1,d2), 3), sep=""), col="navy")
  text(textlabs[5], textlabs[6], paste("RMSE = ", signif(rmse(d1,d2), 3), sep = ""), col="navy")
  text(1.2,-1.8,paste("Acc. = ",signif(100*(sum(apply(modelOut$preds,1,
           function(x) if(all(x > 0) || all(x<0)) 1 else 0))
              /nrow(modelOut$preds)),4), sep=""), col="darkgreen")
  text(1.2,-2.1,paste("AUROC = ",signif(getAUC.gen(modelOut),3), sep=""), col="darkgreen") 
  require(calibrate)
  par(mar=c(0,5,1,1))
#  hist(d1, breaks=100, freq=F, col=colour, border=colour, 
#       axes=FALSE, ylab="", xlab="",main="", xlim=c(-2.2,2.2))
  plot(density(d1, from=-2.2, to=2.2), frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", xlim=c(-2.2,2.2))
  polygon(density(d1,from=-2.2, to=2.2), col=colour, border=colour, lwd=4) 
  par(mar=c(5,0,2,1))
  plot(density(d2,from=-2.6, to=2.6)$y, density(d2,from=-2.6, to=2.6)$x, 
       frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", ylim=c(-2.6,2.6))
  polygon(density(d2, from=-2.6, to=2.6)$y, density(d2, from=-2.6, to=2.6)$x, 
          col=colour, border=colour, lwd=4)
  #barplot(yhist$density, axes=FALSE, space=0, horiz=TRUE, col=colour, border=colour)
  par(def.par)
}


plotPredRes.h <- function (modelOut, col="blue", ct="H1"){
  d1 <- modelOut$preds[,1]
  d2 <- modelOut$preds[,2]
  colour  <- "#0000ff42"
  if ( col != "blue" ) {
    ifelse(col == "red", colour <- "#ff000042", colour  <- "#FFA50062")
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  xhist <- hist(d1, plot=FALSE)
  yhist <- hist(d2, breaks=100, plot=F)
  top <- max(c(xhist$counts, yhist$counts))
  #nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(6,1), c(1,6), TRUE)
  par(mar=c(5,5,4,4), mgp=c(1.8,.4,0))
  plot(d1, d2, xlab="Predicted eig", ylab="Empirical eig", col=colour, 
       pch=16, type='n', xlim=c(-18,18), ylim=c(-20,20), frame=F)
  abline(h=0,v=0, lty=2)
  points(d1, d2, col=colour, pch=16)
  textlabs <- c(-13,18,-13,16, -13,14)
  text(textlabs[1],textlabs[2], ct, font=2, cex=1.2)
  text(textlabs[3], textlabs[4], paste("PCC = ", signif(cor(d1,d2), 3), sep=""), col="navy")
  text(textlabs[5], textlabs[6], paste("RMSE = ", signif(rmse(d1,d2), 3), sep = ""), col="navy")
  text(13,-16,paste("Acc. = ",signif(100*(sum(apply(modelOut$preds,1,
                                                      function(x) if(all(x > 0) || all(x<0)) 1 else 0))
                                            /nrow(modelOut$preds)),4), sep=""), col="darkgreen")
  text(13,-18,paste("AUROC = ",signif(getAUC.gen(modelOut),3), sep=""), col="darkgreen") 
  require(calibrate)
  ### TOP PLOT
  rug(d1, side=1, col=colour)
  ### RIGHT PLOT
  rug(d2, side=4, col=colour)
  box()
  par(def.par)
}

plotPredRes.homer <- function (modelOut=NA, x=NA, y=NA, col="blue", ct="H1"){
  require("calibrate")
  if(!is.na(x) & !is.na(y)){
    d1 <- x
    d2 <- y
  } else {
    d1 <- modelOut$preds[,1]
    d2 <- modelOut$preds[,2]
  }
  xy <- cbind(d1, d2)
  colour  <- "#0000ff42"
  if ( col != "blue" ) {
    ifelse(col == "red", colour <- "#ff000042", colour  <- "#FFA50062")
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  xhist <- hist(d1, plot=FALSE)
  yhist <- hist(d2, breaks=100, plot=F)
  top <- max(c(xhist$counts, yhist$counts))
  nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(6,1), c(1,6), TRUE)
  par(mar=c(5,5,0,0), mgp=c(1.8,.4,0))
  max <- max(abs(c(d1, d2))) * 1.05
  plot(d1, d2, xlab="Predicted eig", ylab="Empirical eig", col=colour, 
       pch=16, type='n', xlim=c(-max*.9,max*.9), ylim=c(-max,max))
  abline(h=0,v=0, lty=2)
  points(d1, d2, col=colour, pch=16)
  contour(kde2d(d1, d2, h=.125), nlevels=8, drawlabels=F, add=T)
  pos <- c(-max/1.5, max*.9, # ct x, y --TOP
                -max/1.5, max*.8, # pcc x, y
                -max/1.5, max*.7, # RMSE
                max/1.5, -max*.9, # AUROC --bottom
                max/1.5, -max*.8) # Acc.
  text(pos[1], pos[2], ct, font=2, cex=1.2)
  text(pos[3], pos[4], paste("PCC = ", signif(cor(d1,d2), 3), sep=""), col="navy")
  text(pos[5], pos[6], paste("RMSE = ", signif(rmse(d1,d2), 3), sep = ""), col="navy")
  text(pos[9], pos[10], paste("Acc. = ",signif(100*(sum(apply(xy,1,
                                        function(x) if(all(x > 0) || all(x<0)) 1 else 0))
                                                    /nrow(xy)),4), sep=""), col="darkgreen")
  text(pos[7], pos[8], paste("AUROC = ", signif(getAUC.gen(xy),3), sep=""), col="darkgreen") 
  par(mar=c(0,5,1,1))
  plot(density(d1, from=-max, to=max), 
       frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", xlim=c(-max*.9,max*.9))
  polygon(density(d1,from=-max, to=max), 
          col=colour, border=colour, lwd=4) 
  par(mar=c(5,0,1,1))
  plot(density(d2,from=-max, to=max)$y, density(d2,from=-max, to=max)$x, 
       frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", ylim=c(-max,max))
  polygon(density(d2, from=-max, to=max)$y, density(d2, from=-max, to=max)$x, 
          col=colour, border=colour, lwd=4)
  par(def.par)
}

plotPredRes.ice <- function(modelOut=NA, x=NA, y=NA, 
                             col="blue", ct="H1", scale.factor=.7){
  require("calibrate")
  if(!is.na(x) & !is.na(y)){
    d1 <- x
    d2 <- y
  } else {
    d1 <- modelOut$preds[,1]
    d2 <- modelOut$preds[,2]
  }
  xy <- cbind(d1, d2)
  colour  <- "#0000ff42"
  if ( col != "blue" ) {
    ifelse(col == "red", colour <- "#ff000042", colour  <- "#FFA50062")
  }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  xhist <- hist(d1, plot=FALSE)
  yhist <- hist(d2, breaks=100, plot=F)
  top <- max(c(xhist$counts, yhist$counts))
  nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(6,1), c(1,6), TRUE)
  par(mar=c(5,5,0,0), mgp=c(1.8,.4,0))
  max <- max(abs(c(d1, d2))) * 1.02
  xmin <- -max*scale.factor
  xmax <- max*scale.factor
  # Middle scatterplot:
  plot(d1, d2, xlab="Predicted eig", ylab="Empirical eig", col=colour, 
       pch=16, type='n', xlim=c(xmin, xmax), ylim=c(-max,max))
  abline(h=0,v=0, lty=2)
  points(d1, d2, col=colour, pch=16)
  options(max.contour.segments=50000)
  contour(kde2d(d1, d2, h=.1), nlevels=8, drawlabels=F, add=T)
  pos <- c(-max/2, max*.9, # ct x, y --TOP
           -max/2, max*.8, # pcc x, y
           -max/2, max*.7, # RMSE
           max/2, -max*.9, # AUROC --bottom
           max/2, -max*.8) # Acc.
  text(pos[1], pos[2], ct, font=2, cex=1.2)
  text(pos[3], pos[4], paste("PCC = ", signif(cor(d1,d2), 3), sep=""), col="navy")
  text(pos[5], pos[6], paste("RMSE = ", signif(rmse(d1,d2), 3), sep = ""), col="navy")
  text(pos[9], pos[10], paste("Acc. = ",signif(100*(sum(apply(xy,1,
                                                              function(x) if(all(x > 0) || all(x<0)) 1 else 0))
                                                    /nrow(xy)),4), sep=""), col="darkgreen")
  text(pos[7], pos[8], paste("AUROC = ", signif(getAUC.gen(xy),3), sep=""), col="darkgreen") 
  par(mar=c(0,5,1,1))
  ## Top density plot:
  plot(density(d1, from=xmin, to=xmax), 
       frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", xlim=c(xmin,xmax))
  polygon(density(d1,from=-max, to=max), 
          col=colour, border=colour, lwd=4) 
  par(mar=c(5,0,1,1))
  # Right-hand side:
  plot(density(d2,from=-max, to=max)$y, density(d2,from=-max, to=max)$x, 
       frame=F, axes=F, type="n", col=colour,
       xlab="", ylab="", main="", ylim=c(-max,max))
  polygon(density(d2, from=-max, to=max)$y, density(d2, from=-max, to=max)$x, 
          col=colour, border=colour, lwd=4)
  par(def.par)
}

plotPredRes <- plotPredRes.ice

procStates <- function(fpath, res=c(1,100), nr=c(7,8)){
  states <- read.table(fpath, stringsAsFactors=F, header=T, 
                       colClasses=c("character", rep("numeric", nr)))
  # missing are near centromeres or summat
  checkdat <- if(res == 1) h.dat else hdh
  missin <- rownames(checkdat)[!rownames(checkdat) %in% states$bin]
  mrows <- data.frame(missin, matrix(rep(0, (ncol(states)-1) * length(missin)), ncol=ncol(states)-1))
  colnames(mrows) <- colnames(states)
  states <- rbind(states, mrows)
  states <- states[order(match(states$bin, rownames(checkdat))),]
  states
}

read.pc <- function(fn)
  as.numeric(t(read.delim(fn, sep=" ", header=F))[,1])

rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))

#' Build summary data.frame
#' 
#' Essentially just wraps a fairly simple \code{plyr} munging call.
#' Better to just learn \code{dplyr} properly.
#' 
#' @export
#' 
#' @references \url{http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/}
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library("plyr")
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
