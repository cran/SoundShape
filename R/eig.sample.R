#' Sample of 3D point coordinates (semilandmarks) acquired with \code{eigensound} function
#'
#' This sample file was acquired using \code{eigensound(analysis.type = "threeDshape")} and features 3D point coordinates (i.e. semilandmarks) from the calls of three frog species: \emph{Physalaemus centralis}, \emph{P. cuvieri} and \emph{P. kroyeri} (Amphibia, Anura, Leptodactylidae).
#'
#'
#' @docType data
#'
#' @usage data(eig.sample)
#'
#' @keywords datasets
#'
#' @format
#' An object of the class \code{"array"} (\code{\link{base}} package).
#'
#' \code{"array"} is a special type of \code{"list"} that can be thought as a filing cabinet, in which the \code{array} is the cabinet and each element is an arquive. This special \code{list} can be used in the subsequent steps of the eigensound protocol (MacLeod et al., 2013; Rocha & Romano \emph{in prep}).
#'
#'
#' @details
#' Each species is represented by three stereotyped acoustic units (i.e. notes from their advertisement calls), which are available as sample data from \code{\link{SoundShape}} (\code{"Wave"} objects: \code{\link{centralis}}, \code{\link{cuvieri}} and \code{\link{kroyeri}}). See Rocha & Romano (\emph{in prep}) for details.
#'
#' Prior to \code{\link{eigensound}} analysis, each of the sample calls had the acoustic units selected, stored as separate \code{".wav"} files, and aligned at beggining of sound window using \code{align.wave} (see \code{Examples} section).
#'
#' \code{eig.sample} is composed of 9 elements (i.e. three species, each represented by three acoustic units). Each element is a matrix with 3200 rows and 3 columns (i.e. X, Y and Z coordinates of 3200 semilandmarks). The number of semilandmarks acquired will depend on the number of cells per side on the sound window (i.e. \code{x.length} and \code{y.length} arguments from \code{\link{eigensound}} function).
#'
#' The analysis itself (\code{\link{eigensound}} function) featured relative amplitude backgroud at 25 dB (\code{dBlevel = 25}), sampling grid of 70 cells on the time (X-axis) and 47 cells on the frequency (Y-axis) (\code{x.length = 70}, \code{y.length = 47}, respectively). Sound window ranged from 0 to 0.8 s (X-axis), and from 0 to 4 kHz (Y-axis) (\code{tlim = c(0, 0.8)}, \code{flim = c(0, 4)}, respectively).
#'
#' Spectrogram parameters were the same as \code{eigensound} default: \code{f = 44100}, \code{wl = 512}, \code{ovlp = 70}.
#'
#' @source
#' Sample data of \code{"Wave"} objects employed on \code{eigensound} analysis:
#' \itemize{
#'   \item{\code{\link{centralis}}: Advertisement call of \emph{Physalaemus centralis}; original recording housed at Fonoteca Neotropical Jacques Vielliard (FNJV-0031188). Recorded by Adão José Cardoso.}
#'   \item{\code{\link{cuvieri}}: Advertisement call of \emph{Physalaemus cuvieri}; original recording housed at Coleção Bioacústica da Universidade Federal de Minas Gerais (CBUFMG-00196). Recorded by Pedro Rocha.}
#'   \item{\code{\link{kroyeri}}: Advertisement call of \emph{Physalaemus kroyeri}; Original recording housed at Fonoteca Neotropical Jacques Vielliard (FNJV-0032047). Recorded by Werner Bokermann.}
#' }
#'
#' @references
#' MacLeod, N., Krieger, J. & Jones, K. E. (2013). Geometric morphometric approaches to acoustic signal analysis in mammalian biology. \emph{Hystrix, the Italian Journal of Mammalogy, 24}(1), 110-125.
#'
#' Rocha, P. & Romano, P. (2021) The shape of sound: A new \code{R} package that crosses the bridge between Bioacoustics and Geometric Morphometrics. \emph{Methods in Ecology and Evolution, 12}(6), 1115-1121.
#'
#'
#'@examples
#' data(eig.sample)
#'
#' # PCA using 3D semilandmark coordinates
#' pca.eig.sample <- stats::prcomp(geomorph::two.d.array(eig.sample))
#'
#' # Verify names for each acoustic unit and the order in which they appear
#' dimnames(eig.sample)[[3]]
#'
#' # Create factor to use as groups in subsequent ordination plot
#' sample.gr <- factor(c(rep("centralis", 3), rep("cuvieri", 3), rep("kroyeri", 3)))
#'
#' # Plot result of Principal Components Analysis
#' pca.plot(PCA.out = pca.eig.sample, groups = sample.gr, conv.hulls = sample.gr,
#'          main="PCA of 3D coordinates", leg=TRUE, leg.pos = "top")
#'
#' # Verify hypothetical sound surfaces for each Principal Component
#' hypo.surf(threeD.out=eig.sample, PC=1, flim=c(0, 4), tlim=c(0, 0.8),
#'           x.length=70, y.length=47, plot.exp = FALSE)
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#' #                      Recreate eig.sample object                        #
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' \donttest{
#' library(seewave)
#' library(tuneR)
#'
#' # Create temporary folder to store ".wav" files
#' wav.at <- file.path(base::tempdir(), "eig.sample")
#' if(!dir.exists(wav.at)) dir.create(wav.at)
#'
#' # Create temporary folder to store results
#' store.at <- file.path(base::tempdir(), "eig.sample-output")
#' if(!dir.exists(store.at)) dir.create(store.at)
#'
#' # Select three acoustic units within each sound data
#' data(cuvieri)
#' spectro(cuvieri, flim = c(0,4))
#' cut.cuvieri1 <- cutw(cuvieri, f=44100, from=0, to=0.5, output = "Wave")
#' cut.cuvieri2 <- cutw(cuvieri, f=44100, from=0.7, to=1.2, output = "Wave")
#' cut.cuvieri3 <- cutw(cuvieri, f=44100, from=1.4, to=1.9, output = "Wave")
#'
#' data("centralis")
#' spectro(centralis, flim = c(0,4))
#' cut.centralis1 <- cutw(centralis, f=44100, from=0.1, to=0.8, output = "Wave")
#' cut.centralis2 <- cutw(centralis, f=44100, from=1.05, to=1.75, output = "Wave")
#' cut.centralis3 <- cutw(centralis, f=44100, from=2.1, to=2.8, output = "Wave")
#'
#' data("kroyeri")
#' spectro(kroyeri, flim = c(0,4))
#' cut.kroyeri1 <- cutw(kroyeri, f=44100, from=0.1, to=1, output = "Wave")
#' cut.kroyeri2 <- cutw(kroyeri, f=44100, from=1.5, to=2.3, output = "Wave")
#' cut.kroyeri3 <- cutw(kroyeri, f=44100, from=2.9, to=3.8, output = "Wave")
#'
#' # Export new wave files containing acoustic units and store on previosly created folder
#' writeWave(cut.cuvieri1, filename = file.path(wav.at, "cut.cuvieri1.wav"), extensible = FALSE)
#' writeWave(cut.cuvieri2, filename = file.path(wav.at, "cut.cuvieri2.wav"), extensible = FALSE)
#' writeWave(cut.cuvieri3, filename = file.path(wav.at, "cut.cuvieri3.wav"), extensible = FALSE)
#' writeWave(cut.centralis1, filename = file.path(wav.at, "cut.centralis1.wav"), extensible = FALSE)
#' writeWave(cut.centralis2, filename = file.path(wav.at, "cut.centralis2.wav"), extensible = FALSE)
#' writeWave(cut.centralis3, filename = file.path(wav.at, "cut.centralis3.wav"), extensible = FALSE)
#' writeWave(cut.kroyeri1, filename = file.path(wav.at, "cut.kroyeri1.wav"), extensible = FALSE)
#' writeWave(cut.kroyeri2, filename = file.path(wav.at, "cut.kroyeri2.wav"), extensible = FALSE)
#' writeWave(cut.kroyeri3, filename = file.path(wav.at, "cut.kroyeri3.wav"), extensible = FALSE)
#'
#' # Place sounds at beggining of sound window before analysis
#' align.wave(wav.at = wav.at, wav.to = "Aligned",
#'            time.length = 0.8, time.perc = 0.005, dBlevel = 25)
#'
#' # Verify alignment using analysis.type = "twoDshape"
#' eigensound(analysis.type = "twoDshape", wav.at = file.path(wav.at, "Aligned"),
#'            store.at = store.at, flim=c(0, 4), tlim=c(0, 0.8),
#'            plot.exp = TRUE, plot.as = "jpeg", dBlevel = 25)
#' # Go to folder specified by store.at and check jpeg files created
#'
#' # Run eigensound function using analysis.type = "threeDshape" on aligned wave files
#' # Store results as R object
#' eig.sample <- eigensound(analysis.type="threeDshape", wav.at = file.path(wav.at, "Aligned"),
#'                          flim=c(0, 4), tlim=c(0, 0.8), dBlevel=25, plot.exp = FALSE,
#'                          x.length=70, y.length = 47, log.scale = TRUE)
#'
#' }
#'
#'
"eig.sample"
