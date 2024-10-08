% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SoundShape.R
\docType{package}
\name{SoundShape}
\alias{SoundShape-package}
\alias{SoundShape}
\title{Sound Waves Onto Morphometric Data}
\description{
\code{SoundShape} package provide the tools required to implement a promising, and yet little explored method for bioacoustical analysis, the so called eigensound protocol developed by MacLeod, Krieger, & Jones (2013). Eigensound was developed for taxonomy-based bioacoustics and focuses on the comparison between acoustic units from different species. The method consists on applying a sampling grid over the 3D representation of sound (i.e. spectrogram data) and then translate the spectrogram into a dataset that can be analyzed similarly to 3D coordinate sets used in Geometric Morphometrics Methods, thus enabling the direct comparison between stereotyped calls from different species. For more information on \code{SoundShape} and the eigensound method, see Rocha & Romano (2021) and MacLeod et al. (2013).

The following set of functions crosses the bridge between Bioacoustics and Geometric Morphometrics:

\itemize{
\item{\code{\link{raven.to.wave}}: Export sample \code{".wav"} files using selections from Raven Pro software.}
\item{\code{\link{align.wave}}: Automatic placement of calls at the beginning of a sound window.}
\item{\code{\link{eigensound}}: Calculate spectrogram data for each \code{".wav"} file on a given folder and acquire semilandmarks using a 3D representation of sound.}
\item{\code{\link{pca.plot}}: Plot ordination of Principal Components with convex hulls.}
\item{\code{\link{hypo.surf}}: Hypothetical 3D plots of sound surfaces representing a sample of sounds submitted to \code{\link{eigensound}}.}
\item{\code{\link{threeDspectro}}: 3D spectrograms from a single object of class \code{"Wave"}.}
}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/p-rocha/SoundShape}
  \item Report bugs at \url{https://github.com/p-rocha/SoundShape/issues}
}

}
\author{
\strong{Maintainer}: Pedro Rocha \email{p.rocha1990@gmail.com}

}
