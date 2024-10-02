## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
oldpar <- graphics::par(no.readonly = TRUE)

## ----Fig1-3D-spectros, fig.align='center', fig.height=3.5, fig.width=7.2------
library(SoundShape)

# Sample data from SoundShape
data(cuvieri)

# Select acoustic unit from sample
cuvieri.cut <- seewave::cutw(cuvieri, f=44100, from = 0.05, to=0.45, output="Wave")

# 3D spectrogram
par(mfrow=c(1,2), mar=c(0,2,1,0))
threeDspectro(cuvieri.cut, flim=c(0, 2.5), 
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=3)

# Semilandmarks from sampled surface
threeDspectro(cuvieri.cut, flim=c(0, 2.5), plot.type="points",
              samp.grid=TRUE, x.length=70, y.length=50, main="Semilandmarks 3D",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)


## ----include = FALSE----------------------------------------------------------
graphics::par(oldpar)

## ----Fig2-2D-spectros, fig.align='center', fig.width=7.2, fig.height=3.5------
# Traditional oscillogram and spectrogram
par(mfrow=c(1,2), mar=c(4,4,2,1)) # view side by side
seewave::oscillo(cuvieri.cut, title="Oscillogram")
seewave::spectro(cuvieri.cut, flim=c(0, 2.5), grid=FALSE, scale=FALSE, main="Spectrogram")

## ----include = FALSE----------------------------------------------------------
graphics::par(oldpar)

## ----citation-----------------------------------------------------------------
citation("SoundShape")

## ----Fig3-centralis-units, fig.align='center', fig.height=3, fig.width=7.2----
# Samples of data from SoundShape package
data(cuvieri)
data(centralis)
data(kroyeri)

# Plot spectro from sample and highlight acoustic units

# centralis
seewave::spectro(centralis, flim = c(0, 4), wl=512, f=44100, ovlp=70, grid=FALSE)
graphics::abline(v=c(0.1, 0.8, 1.08, 1.78, 2.1, 2.8), lty=2)

## ----Fig4-cuvieri-units, fig.align='center', fig.height=3, fig.width=7.2------
# cuvieri
seewave::spectro(cuvieri, flim = c(0,4), wl=512, f=44100, ovlp=70, grid=FALSE)
graphics::abline(v=c(0.05, 0.45, 0.73, 1.13, 1.47, 1.87), lty=2)

## ----Fig5-kroyeri-units, fig.align='center', fig.height=3, fig.width=7.2------
# kroyeri
seewave::spectro(kroyeri, flim = c(0, 4), wl=512, f=44100, ovlp=70, grid=FALSE)
graphics::abline(v=c(0.16, 0.96, 1.55, 2.35, 2.9, 3.8), lty=2)

## ----cut-units----------------------------------------------------------------
# Select acoustic units
cut.centralis <- seewave::cutw(centralis, f=44100, from=0, to=0.9, output = "Wave")
cut.cuvieri <- seewave::cutw(cuvieri, f=44100, from=0, to=0.9, output = "Wave")
cut.kroyeri <- seewave::cutw(kroyeri, f=44100, from=0.2, to=1.1, output = "Wave")

## ----Fig6-window-dimensions, fig.align='center', fig.height=5, fig.width=7.2----
# Spectrogram plots using standardized sound window dimensions
par(mfrow=c(2,2), mar=c(4,4,2,2))
seewave::spectro(cut.centralis, flim=c(0, 4), tlim=c(0, 0.8), main="data(centralis)",
                 wl=512, f=44100, ovlp=70, grid=FALSE, scale=FALSE)
seewave::spectro(cut.cuvieri, flim=c(0, 4), tlim=c(0, 0.8), main="data(cuvieri)", 
                 wl=512, f=44100, ovlp=70, grid=FALSE, scale=FALSE)
seewave::spectro(cut.kroyeri, flim=c(0, 4), tlim=c(0, 0.8), main="data(kroyeri)", 
                 wl=512, f=44100, ovlp=70, grid=FALSE, scale=FALSE)

## ----include = FALSE----------------------------------------------------------
graphics::par(oldpar)

## ----Fig7-dBlevel-curve, fig.align='center', fig.height=3, fig.width=7.2------
# 2D spectrogram with curves of relative amplitude at -25 dB
par(mfrow=c(1,2), mar=c(4,4,1,1))
s.kro <- seewave::spectro(cut.kroyeri, flim=c(0, 4), tlim = c(0, 0.8),  
                          grid=F, scale=F, f=44100, wl=512, ovlp=70, cont=TRUE, 
                          contlevels = seq(-25, -25, 1), collevels = seq(-40, 0, 0.1))

# 3D spectrogram (with a lower dBlevel for illustrative purpuses)
threeDspectro(cut.kroyeri, dBlevel=40, flim=c(0, 4), tlim=c(0, 0.8), main="",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8, resfac=2)

# Set background at -40 dB and remove -Inf values from spectrogram data 
for(i in 1:length(s.kro$amp)){if(s.kro$amp[i] == -Inf |s.kro$amp[i] <= -40)
{s.kro$amp[i] <- -40}}

# Add curve of relative amplitude
plot3D::contour3D(x=s.kro$time, y=s.kro$freq, colvar=t(s.kro$amp), z=-25,
                  plot=T, add=T, addbox=F, col="black", lwd=1.9, nlevels=2, dDepth=0.25)

## ----include = FALSE----------------------------------------------------------
graphics::par(oldpar)

## ----Fig8-sampling-grid, fig.align='center', fig.height=4, fig.width=7.2------
# Using threeDspectro to visualize sampling grid 
par(mfrow=c(1,2), mar=c(1,2,1,0)) 

# As "surface"
threeDspectro(cut.kroyeri, samp.grid=TRUE, x.length=70, y.length=47, plot.type="surface", 
              dBlevel=25, flim=c(0, 4), tlim=c(0, 0.8), f=44100, wl=512, ovlp=70, main="As 'surface'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)

# As "points"
threeDspectro(cut.kroyeri, samp.grid=TRUE, x.length=70, y.length=47, plot.type="points", 
              dBlevel=25, flim=c(0, 4), tlim=c(0, 0.8), f=44100, wl=512, ovlp=70, main="As 'points'",
              colkey=list(plot=FALSE), cex.axis=0.4, cex.lab=0.8)


## ----include = FALSE----------------------------------------------------------
graphics::par(oldpar)

## ----pca----------------------------------------------------------------------
# PCA using three-dimensional semilandmark coordinates embeeded in eig.sample
pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))

# View summary results
summary(pca.eig.sample)

## ----Fig9-mean-surface, fig.align='center', fig.height=3, fig.width=7.2-------
# Create hypothetical sound surfaces using hypo.surf

# Mean shape configuration (consensus)
hypo.surf(eig.sample,  PC="mean", flim=c(0, 4), tlim=c(0, 0.8), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)

## ----Fig10-PC1-surface, fig.align='center', fig.height=3, fig.width=7.2-------
# Minimum and maximum deformations - Principal Component 1
hypo.surf(eig.sample, PC=1, flim=c(0, 4), tlim=c(0, 0.8), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)

## ----Fig11-PC2-surface, fig.align='center', fig.height=3, fig.width=7.2-------
# Minimum and maximum deformations - Principal Component 2
hypo.surf(eig.sample, PC=2, flim=c(0, 4), tlim=c(0, 0.8), x.length=70, y.length=47,
          cex.lab=0.7, cex.axis=0.5, cex.main=1)

## ----Fig12-ordination-plot, fig.align='center', fig.height=5, fig.width=5-----
# PCA using semilandmark coordinates
pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))

# Verify names of acoustic units from sample 
dimnames(eig.sample)[[3]]

# Based on those names, create factor to use as groups in subsequent ordination plot
sample.gr <- factor(c(rep("centralis", 3), rep("cuvieri", 3), rep("kroyeri", 3)))

# Ordination plot
pca.plot(pca.eig.sample, groups=sample.gr, conv.hulls=sample.gr, leg.pos="bottomright", cex=1.2)

