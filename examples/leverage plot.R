model_values <- data.frame(leverage = hatvalues(model),
                           std_res = stdres(model))

# Create the leverage plot
plot(std_res ~ leverage, data = model_values)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

cook.levels = c(0.5, 1)
hii <- (infl <- influence(model, do.coef = FALSE))$hat
r.hat <- range(hii, na.rm = TRUE)
p <- model$rank
usr <- par("usr")
hh <- seq.int(min(r.hat[1L], r.hat[2L]/100),
              usr[2L], length.out = 101)
for (crit in cook.levels) {
  cl.h <- sqrt(0.5 * p * (1 - hh)/hh)
  lines(hh, cl.h, lty = 2, col = 2)
  lines(hh, -cl.h, lty = 2, col = 2)
}



ylab5 <- if (isGlm)
      "Std. Pearson resid."
    else "Standardized residuals"
    r.w <- residuals(x, "pearson")
    if (!is.null(w))
      r.w <- r.w[wind]
    rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    ylim <- range(rsp, na.rm = TRUE)
    if (id.n > 0) {
      ylim <- extendrange(r = ylim, f = 0.08)
      show.rsp <- order(-cook)[iid]
    }
    do.plot <- TRUE
    if (isConst.hat) {
      if (missing(caption))
        caption[[5L]] <- "Constant Leverage:\n Residuals vs Factor Levels"
      aterms <- attributes(terms(x))
      dcl <- aterms$dataClasses[-aterms$response]
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- model.frame(x)[facvars]
      if (ncol(mf) > 0) {
        dm <- data.matrix(mf)
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels,
                                                  length))))
        ff <- if (nf == 1)
          1
        else rev(cumprod(c(1, nlev[nf:2])))
        facval <- (dm - 1) %*% ff
        xx <- facval
        dev.hold()
        plot(facval, rsp, xlim = c(-1/2, sum((nlev -
                                                1) * ff) + 1/2), ylim = ylim, xaxt = "n", main = main,
             xlab = "Factor Level Combinations", ylab = ylab5,
             type = "n", ...)
        axis(1, at = ff[1L] * (1L:nlev[1L] - 1/2) - 1/2,
             labels = x$xlevels[[1L]])
        mtext(paste(facvars[1L], ":"), side = 1, line = 0.25,
              adj = -0.05)
        abline(v = ff[1L] * (0:nlev[1L]) - 1/2, col = "gray",
               lty = "F4")
        panel(facval, rsp, ...)
        abline(h = 0, lty = 3, col = "gray")
        dev.flush()
      }
      else {
        message(gettextf("hat values (leverages) are all = %s\n and there are no factor predictors; no plot no. 5",
                         format(mean(r.hat))), domain = NA)
        frame()
        do.plot <- FALSE
      }
    }
    else {
      xx <- hii
      xx[xx >= 1] <- NA
      dev.hold()
      plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)),
           ylim = ylim, main = main, xlab = "Leverage",
           ylab = ylab5, type = "n", ...)
      panel(xx, rsp, ...)
      abline(h = 0, v = 0, lty = 3, col = "gray")
      if (one.fig)
        title(sub = sub.caption, ...)
      if (length(cook.levels)) {
        p <- x$rank
        usr <- par("usr")
        hh <- seq.int(min(r.hat[1L], r.hat[2L]/100),
                      usr[2L], length.out = 101)
        for (crit in cook.levels) {
          cl.h <- sqrt(crit * p * (1 - hh)/hh)
          lines(hh, cl.h, lty = 2, col = 2)
          lines(hh, -cl.h, lty = 2, col = 2)
        }
        legend("bottomleft", legend = "Cook's distance",
               lty = 2, col = 2, bty = "n")
        xmax <- min(0.99, usr[2L])
        ymult <- sqrt(p * (1 - xmax)/xmax)
        aty <- sqrt(cook.levels) * ymult
        axis(4, at = c(-rev(aty), aty), labels = paste(c(rev(cook.levels),
                                                         cook.levels)), mgp = c(0.25, 0.25, 0), las = 2,
             tck = 0, cex.axis = cex.id, col.axis = 2)
      }
      dev.flush()
    }
    if (do.plot) {
      mtext(getCaption(5), 3, 0.25, cex = cex.caption)
      if (id.n > 0) {
        y.id <- rsp[show.rsp]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(xx[show.rsp], y.id, show.rsp)
      }
    }
  }
