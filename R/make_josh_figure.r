# Make figure based on linear model with clade and growth form

# Only predict data for combinations that exist in database
store <- data.frame()
for (cd in levels(dat$Clade.Fukami.Huang.2012)) {
  for (gf in levels(dat$Growth.form)) {
    if (sum(!is.na(dat$Growth.rate[dat$Clade.Fukami.Huang.2012==cd & dat$Growth.form==gf]))>0) {

      temp <- predict.lm(mod, list(Clade.Fukami.Huang.2012=cd, Growth.form=gf), se.fit=TRUE)
      store <- rbind(store, data.frame(cd, gf, fit=temp$fit, lwr=temp$fit-temp$se.fit, upr=temp$fit+temp$se.fit))

    }
  }
}

# not great coding, but needed for plot symbols
store$gfn[store$gf=="massive"] <- 1
store$gfn[store$gf=="solitary"] <- 2
store$gfn[store$gf=="encrusting"] <- 3
store$gfn[store$gf=="digitate"] <- 4
store$gfn[store$gf=="columnar"] <- 5
store$gfn[store$gf=="foliaceous"] <- 6
store$gfn[store$gf=="corymbose"] <- 7
store$gfn[store$gf=="tabular"] <- 8
store$gfn[store$gf=="branching"] <- 9
store$gfn[store$gf=="branching_table"] <- 10
store$gfn[store$gf=="caespitose"] <- 11
store$gfn[store$gf=="hispidose"] <- 12
store$gfn[store$gf=="encrusting_branching"] <- 13

store <- store[order(store$fit),]
store <- store[order(store$cd),]

# PLOT
par(mar=c(5, 4, 1, 1))
plot(0, 0, ylab="Linear extension rate (mm/yr)", axes=FALSE, xlab="", xlim=c(0, 45), ylim=c(-0.5, 2.2), type="n")
v <- c(0, cumsum(aggregate(store$cd, list(store$cd), length)$x))
rect(v[seq(1, 15, 2)]+0.5, log10(0.4), v[seq(2, 16, 2)]+0.5, 2, col="lightgrey", border=NA)
points(1:nrow(store), store$fit, pch=store$gfn)
arrows(1:nrow(store), store$lwr, 1:nrow(store), store$upr, code=3, angle=90, length=0.025)
axis(2, at=c(0, 1, 2), labels=c(1, 10, 100), las=2)
axis(2, at=log10(seq(0.4, 1, 0.1)), labels=NA, tck=-0.015)
axis(2, at=log10(seq(1, 10)), labels=NA, tck=-0.015)
axis(2, at=log10(seq(10, 100, 10)), labels=NA, tck=-0.015)
axis(2, at=log10(c(0.4, 4, 40)), labels=c(0.4, 4, 40), tick=FALSE, las=2)
axis(1, at=c(1.5, 4, 6, 12, 18.5, 20, 21, 23, 25.5, 27, 28, 29, 30, 32, 34), labels=unique(store$cd), las=2)
legend(36, 2, legend=as.character(unique(store$gf)), pch=1:13, cex=0.8, bty="n", bg="lightgrey", box.col="white")
mtext("Clade", 1, line=3)

