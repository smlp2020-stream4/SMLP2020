ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  ## http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
  ## re = object of class ranef.mer
  ## Didzis Elferts (17 Dec 2012) + caracal (12 May 2013)
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme_bw() + theme(legend.position="none") 
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue")
    return(p)
  }
  
  lapply(re, f)
}


# Example
# fit <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# ggCaterpillar(ranef(fit, postVar=TRUE))  ## using ggplot2
# qqmath(ranef(fit, postVar=TRUE))         ## for comparison


# http://stackoverflow.com/questions/13493841/extracting-the-number-of-observations-and-the-modes-of-random-effects-from-a-mer
# Sven Hohenstein
# lapply(names(ranef(fit)), function(x) cbind(ranef(fit)[[x]], table(model.frame(fit)[[x]])))