
qualityset <- quality_mmse
slots <- sum(!is.na(qualityset$unique_id))
#modelnow <- che.model_mmse


png(file = "qual_plot_mmse.png", width = 2800, height = 2400, res = 300)

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
plot.new()
plot.window(xlim=c(0,10), ylim = c(-1,slots*15 +30))
#plot.window(xlim=c(-10,10), ylim = c(-0,slots +4))

text(4:10, slots*15+30, c("1", "2", "3", "5","6", "7", "8"))
text(0, seq(from=slots*15+2, to = 17, by = -15), qualityset$Study, adj = 0)
points( rep(4,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J1], cex=1.6)
text(4, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J1], cex=0.7)
points( rep(5,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J2], cex=1.6)
text(5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J2], cex=0.7)
points( rep(6,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J3], cex=1.6)
text(6, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J3], cex=0.7)
points( rep(7,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J5], cex=1.6)
text(7, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J5], cex=0.7)
points( rep(8,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J6], cex=1.6)
text(8, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J6], cex=0.7)
points( rep(9,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J7], cex=1.6)
text(9, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J7], cex=0.7)
points( rep(10,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J8], cex=1.6)
text(10, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J8], cex=0.7)

dev.off()


png(file = "qual_plot_mmse.png", width = 2800, height = 2400, res = 300)

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
plot.new()
plot.window(xlim=c(3,5.6), ylim = c(-1,slots*15 +30))
text(seq(from = 4, to = 5.5, by =.25), slots*15+30, c("1", "2", "3", "5","6", "7", "8"))
text(3, seq(from=slots*15+2, to = 17, by = -15), qualityset$Study, adj = 0)
points( rep(4,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J1], cex=1.6)
text(4, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J1], cex=0.7)
points( rep(4.25,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J2], cex=1.6)
text(4.25, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J2], cex=0.7)
points( rep(4.5,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J3], cex=1.6)
text(4.5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J3], cex=0.7)
points( rep(4.75,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J5], cex=1.6)
text(4.75, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J5], cex=0.7)
points( rep(5,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J6], cex=1.6)
text(5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J6], cex=0.7)
points( rep(5.25,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J7], cex=1.6)
text(5.25, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J7], cex=0.7)
points( rep(5.5,slots), seq(slots*15,by = -15), pch=19, col=cols[qualityset$J8], cex=1.6)
text(5.5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J8], cex=0.7)


flextablation <- qualityset %>%
  select(Study,J1,J2,J3,J5,J6,J7,J8)
