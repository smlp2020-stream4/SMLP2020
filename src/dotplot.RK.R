dotplot.RK <- function(x, data, refvar, ...)
{
    prepanel.ci <- function(x, y, se, subscripts, ...) {
        if (is.null(se)) return(list())
        x <- as.numeric(x)
        hw <- 1.96 * as.numeric(se[subscripts])
        list(xlim = range(x - hw, x + hw, finite = TRUE))
    }
    panel.ci <- function(x, y, se, subscripts, pch = 16,
                         horizontal = TRUE, col = dot.symbol$col,
                         lty = dot.line$lty, lwd = dot.line$lwd,
                         col.line = dot.line$col, levels.fos = unique(y),
                         groups = NULL, ...)
    {
        x <- as.numeric(x)
        y <- as.numeric(y)
        dot.line <- trellis.par.get("dot.line")
        dot.symbol <- trellis.par.get("dot.symbol")
        sup.symbol <- trellis.par.get("superpose.symbol")
        panel.abline(h = levels.fos, col = col.line, lty = lty, lwd = lwd)
        panel.abline(v = 0, col = col.line, lty = lty, lwd = lwd)
        if (!is.null(se)) {
            se <- as.numeric(se[subscripts])
            panel.segments( x - 1.96 * se, y, x + 1.96 * se, y, col = 'black')
        }
        panel.xyplot(x, y, pch = pch, ...)
    }
    f <- function(x, ...) {
        ss <- stack(x)
        ss$ind <- factor(as.character(ss$ind), levels = colnames(x))
        ss$.nn <- rep.int(reorder(factor(rownames(x)), x[[refvar]]), ncol(x))
        se <- NULL
        if (!is.null(pv <- attr(x, "postVar")))
            se <- unlist(lapply(1:(dim(pv)[1]), function(i) sqrt(pv[i, i, ])))
        dotplot(.nn ~ values | ind, ss, se = se,
                prepanel = prepanel.ci, panel = panel.ci,
                xlab = NULL, ...)
    }
    lapply(x, f, ...)
}