model_values <- data.frame(leverage = hatvalues(model),
                           std_res = stdres(model))

plot(std_res ~ leverage, data = model_values,
     xlim = c(0, max(leverage, na.rm = TRUE)),
     ylim = extendrange(range(std_res, na.rm = TRUE), f = 0.08),
     type = 'n')
add.smooth = getOption("add.smooth")
panel = if (add.smooth) panel.smooth else points
panel(hii, model_values$std_res)
abline(h = 0, v = 0, lty = 3, col = "gray")

id.n = 3
cex.id = 0.75
cook.levels = c(0.5, 1)
hii <- (infl <- influence(model, do.coef = FALSE))$hat
r.hat <- range(hii, na.rm = TRUE)
p <- model$rank
iid <- 1L:id.n
show.rsp <- order(-cook)[iid]
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
axis(4, at = c(-rev(aty), aty),
     labels = paste(c(rev(cook.levels), cook.levels)),
     mgp = c(0.25, 0.25, 0), las = 2,
     tck = 0, cex.axis = cex.id, col.axis = 2)

y.id <- model_values$std_res[show.rsp]
y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
text(hii[show.rsp], y.id, show.rsp)

