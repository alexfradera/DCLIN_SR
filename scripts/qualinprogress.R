

# Plotmaking scripts, essentially
# Due to real problems with scaling and quality of outputs, the easiest turned out to be
# Create .pdf file for each plot
# Use Affinity Designer to export each pdf as a png
# Link to the png within the .rmd.

# ==========================
# FOREST PLOT
# MAKE GOOD PDF

forest(che.hi, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
       showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)

# Simple - not ultimately used
plot.new()
forest(che.model, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
       showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)
dev.print(pdf, file=here("graphs", "Forest_simpleX.pdf") ,
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 



# Richer version with a bit more info. Either use plot.new and dev.print or the tiff and dev off.

# tiff(file=here("graphs", "foresttiff.tif"), 
#     units="in", width = 21/2.54, height = 29.7/2.54, res=1200)
plot.new()
forest(
                   che.model, slab=author_final, 
                   annotate=TRUE, addfit=TRUE, addpred=FALSE, 
                   at= seq(-1,5, by =1), xlim=c(-9,8),
                   showweights=FALSE, header=TRUE, 
                   ilab = cbind(cbind.data.frame(cognitive_name), cbind.data.frame(sample_treat_cat)),
                   order = study_id, ilab.xpos = c(-4.6, -3.4), ilab.pos = 4, cex=.8, top = 0)
text(c(-4.2, -3.0), 64, c("Screen", "Condition"), cex=.8, font=2, adj = c(0, NA))
abline(h = 10.5, col='grey', lty=2, lwd=1)
abline(h = 20.5, col='grey', lty=2, lwd=1)
abline(h = 30.5, col='grey', lty=2, lwd=1)
abline(h = 40.5, col='grey', lty=2, lwd=1)
abline(h = 50.5, col='grey', lty=2, lwd=1)
#dev.off()
dev.print(pdf, file=here("graphs", "Forest_rich.pdf"),
        onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 


# ==========================
# Risk of bias/quality traffic light graphic

# prepare dataset
qualityset <- filter(quality_scores_vals, comp ==1)
qualityset <- arrange(qualityset,study_id)
slots <- sum(!is.na(qualityset$unique_id))
qualityset <- qualityset %>%
  mutate(Study = ifelse(toggle==1, paste0(Study,"*"),paste0(Study))) %>%
  arrange(Study) # mark the high-quality studies with *

#

syms <- c( "-", "?","+")

# Simple version - colour
cols <- c("red", "yellow","green")
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
abline(h = 0)
abline(h = 160)
abline(h = 310)
abline(h = 460)
abline(h = 610)
dev.print(pdf, file=here("graphs", "quality_simp_col.pdf"),
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 

# Simple version - bw
cols <- c("dark grey", "light grey","white")
plot.new()
plot.window(xlim=c(0,10), ylim = c(-1,slots*15 +30))
#plot.window(xlim=c(-10,10), ylim = c(-0,slots +4))
text(0,slots*15+30,"Author",adj = 0)
text(4:10, slots*15+30, c("1", "2", "3", "5","6", "7", "8"))
abline(h = slots*15+15)
text(0, seq(from=slots*15+2, to = 17, by = -15), qualityset$Study, adj = 0)
points( rep(4,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J1], cex=1.6)
text(4, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J1], cex=0.7)
points( rep(5,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J2], cex=1.6)
text(5, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J2], cex=0.7)
points( rep(6,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J3], cex=1.6)
text(6, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J3], cex=0.7)
points( rep(7,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J5], cex=1.6)
text(7, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J5], cex=0.7)
points( rep(8,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J6], cex=1.6)
text(8, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J6], cex=0.7)
points( rep(9,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J7], cex=1.6)
text(9, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J7], cex=0.7)
points( rep(10,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J8], cex=1.6)
text(10, seq(from = slots*15+2, to = 17, by = -15), syms[qualityset$J8], cex=0.7)
abline(h = 0)
abline(h = 160)
abline(h = 310)
abline(h = 460)
abline(h = 610)
dev.print(pdf, file=here("graphs", "quality_simp_bw.pdf"),
          onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 

# Simple version - bw - tweaking in progress


# ADDED: Simple version with tiff function for either black and white or colour
tiff(file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/newtiff.tif", 
     units="in", width = 21/2.54, height = 29.7/2.54, res=1200)


#cols <- c("dark grey", "light grey","white")
cols <- c("red", "yellow","green")

plot.new()
plot.window(xlim=c(0,10), ylim = c(-1,slots*15 +30))
#plot.window(xlim=c(-10,10), ylim = c(-0,slots +4))
text(0,slots*15+30,"Author",adj = 0)
text(4:10, slots*15+30, c("1", "2", "3", "5","6", "7", "8"))
abline(h = slots*15+15)
text(0, seq(from=slots*15+2, to = 17, by = -15), qualityset$Study, adj = 0)
points( rep(4,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J1], cex=1.6)
text(4, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J1], cex=0.7)
points( rep(5,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J2], cex=1.6)
text(5, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J2], cex=0.7)
points( rep(6,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J3], cex=1.6)
text(6, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J3], cex=0.7)
points( rep(7,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J5], cex=1.6)
text(7, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J5], cex=0.7)
points( rep(8,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J6], cex=1.6)
text(8, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J6], cex=0.7)
points( rep(9,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J7], cex=1.6)
text(9, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J7], cex=0.7)
points( rep(10,slots), seq(slots*15,by = -15), pch=21, col = "black", bg=cols[qualityset$J8], cex=1.6)
text(10, seq(from = slots*15, to = 15, by = -15), syms[qualityset$J8], cex=0.7)
abline(h = 0)
abline(h = 160)
abline(h = 310)
abline(h = 460)
abline(h = 610)
dev.off()
# dev.print(pdf, file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/quality_simp_bw_tweak.pdf" ,           onefile=T,paper='A4', width = 21/2.54, height = 29.7/2.54) 




# Version with more information - not ultimately used

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

#=====
# FUnnel plot tiff
#tiff(file="C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/graphs/funneltiff.tif", 
#     units="in", width = 5, height = 5, res=1200)
#pdf(file='forestplot.pdf')
#funnel(res_tri, legend= TRUE, main = "Funnel Plot (focused studies)")
#dev.off()
