
# ==========================
# FOREST PLOT
# MAKE GOOD PDF



# Simple
plot.new()
forest(che.model, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
       showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)
dev.print(pdf, file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/Forest_simpleX.pdf" ,
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 

# Rich
plot.new()
forest(
                   che.model, slab=author_final, 
                   annotate=TRUE, addfit=TRUE, addpred=FALSE, 
                   at= seq(-1,5, by =1), xlim=c(-9,8),
                   showweights=FALSE, header=TRUE, 
                   ilab = cbind(cbind.data.frame(cognitive_name), cbind.data.frame(sample_treat_cat)),
                   order = study_id, ilab.xpos = c(-4.2, -3.0), ilab.pos = 4, cex=.8, top = 0)
text(c(-4.2, -3.0), 64, c("Screen", "Condition"), cex=.8, font=2, adj = c(0, NA))
dev.print(pdf, file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/Forest_rich.pdf" ,
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 




forest(simple_moca, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
       showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)



qualityset <- filter(quality_scores_vals, comp ==1)
qualityset <- arrange(qualityset,study_id)
slots <- sum(!is.na(qualityset$unique_id))




cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
plot.new()
plot.window(xlim=c(0,10), ylim = c(-1,slots*15 +30))
#plot.window(xlim=c(-10,10), ylim = c(-0,slots +4))
text(0,slots*15+30,"Author",adj = 0)
text(4:10, slots*15+30, c("1", "2", "3", "5","6", "7", "8"))
abline(h = slots*15+15)
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
dev.print(pdf, file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/quality_simp.pdf" ,
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 



plot.new()
plot.window(xlim=c(0,10), ylim = c(-1,slots*15 +25))
#plot.window(xlim=c(-10,10), ylim = c(-0,slots +4))
text(0,slots*15+30,"Author",adj = 0)
abline(h = slots*15+15)
text(4:10, slots*15+30, c("1", "2", "3", "5","6", "7", "8"))

text(0, seq(from=slots*15+2, to = 17, by = -15), qualityset$Study, adj = 0)
points( rep(4,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J1], cex=2)
text(4, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J1], cex=0.7)
points( rep(5,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J2], cex=2)
text(5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J2], cex=0.7)
points( rep(6,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J3], cex=2)
text(6, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J3], cex=0.7)
points( rep(7,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J5], cex=2)
text(7, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J5], cex=0.7)
points( rep(8,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J6], cex=2)
text(8, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J6], cex=0.7)
points( rep(9,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J7], cex=2)
text(9, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J7], cex=0.7)
points( rep(10,slots), seq(slots*15,by = -15), pch=22, bg=cols[qualityset$J8], cex=2)
text(10, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J8], cex=0.7)
dev.print(pdf, file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/quality_2.pdf" ,
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 

# UNUSED HELPERS
#forestable <- as.ggplot(~
#png(file = "qual_plot_mmse.png", width = 2800, height = 2400, res = 300)

