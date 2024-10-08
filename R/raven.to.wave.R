#' Export sample \code{".wav"} files using selections from Raven Pro software.
#'
#' @description Create one \code{".wav"} file for each selection created using \href{https://www.ravensoundsoftware.com/software/raven-pro/}{Raven Pro} software, which is commonplace in bioacoustical analysis. Each selection (i.e. line in table) should represent an acoustic unit from the sample study.
#'
#' @param orig.wav.folder filepath to the folder where original \code{".wav"} files are stored. Should be presented between quotation marks. By default: \code{orig.wav.folder = NULL} (i.e. user must specify the filepath to \code{".wav"} files)
#'
#' @param raven.at filepath to the folder where selection tables from \href{https://www.ravensoundsoftware.com/software/raven-pro/}{Raven Pro} software are stored. Should be presented between quotation marks. File name should end with \code{"selections.txt"}. By default: \code{raven.at = orig.wav.folder} (i.e. raven tables stored in the same folder as original  \code{".wav"} files)
#'
#' @param wav.samples folder where new \code{".wav"} files will be stored. Can be either a filepath to the intended folder, or the name of the folder i.e.(\code{wav.samples = "wav samples"}). In the later case, a new folder will be created within the one specified by \code{orig.wav.folder}. Should be presented between quotation marks. By default: \code{wav.samples = "wav samples"}
#'
#'
#' @author
#' Pedro Rocha
#'
#' @seealso
#' \code{\link{align.wave}}, \code{\link{raven.list}},
#'
#' Useful links:
#' \itemize{
#'   \item{\url{https://github.com/p-rocha/SoundShape}}
#'   \item{Report bugs at \url{https://github.com/p-rocha/SoundShape/issues}}
#'   \item{Raven Pro software \url{https://www.ravensoundsoftware.com/software/raven-pro/}}}
#'
#'
#' @examples
#' \donttest{
#'
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'#  Create folders on your console  #
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'# Create temporary folder to store original ".wav" files containing multiple units
#'orig.wav <- file.path(base::tempdir(), "original wave")
#'if(!dir.exists(orig.wav)) dir.create(orig.wav)
#'
#'# Create temporary folder to store sample ".wav" files from original recordings
#'wav.at <- file.path(base::tempdir(), "wav samples")
#'if(!dir.exists(wav.at)) dir.create(wav.at)
#'
#'# Create temporary folder to store results
#'store.at <- file.path(base::tempdir(), "output")
#'if(!dir.exists(store.at)) dir.create(store.at)
#'
#'
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'#   Select acoustic units based on Raven Pro selections   #
#'#                                                         #
#'#              Using raven.to.wav function                #
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'# Export original sample ".wav" files from SoundShape examples
#'tuneR::writeWave(centralis, extensible = TRUE,
#'                 filename = file.path(orig.wav, "centralis.wav"))
#'tuneR::writeWave(cuvieri, extensible = TRUE,
#'                 filename = file.path(orig.wav, "cuvieri.wav"))
#'tuneR::writeWave(kroyeri, extensible = TRUE,
#'                 filename = file.path(orig.wav, "kroyeri.wav"))
#'
#'# Store Raven Pro selection tables at same folder from original ".wav" files
#'for(i in 1:length(raven.list)){
#'  write.table(raven.list[i], file=file.path(orig.wav, names(raven.list)[i]),
#'                quote=FALSE, sep="\t", row.names = FALSE,
#'                col.names = colnames(raven.list[[i]]))  } # end loop
#'
#'
#'# Verify if folder has both original ".wav" files and Raven's selections
#'dir(orig.wav)
#'
#'###
#'## Start here when using your own recordings
#'
#'# Export a ".wav" sample for each selection made in Raven Pro
#'raven.to.wave(orig.wav.folder = orig.wav, wav.samples = wav.at)
#'
#'# Verify samples
#'dir(wav.at)
#'
#'
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'#  Align acoustic units using align.wave  #
#'#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'# Place sounds at the beginning of a sound window
#'align.wave(wav.at=wav.at, wav.to="Aligned",
#'           time.length = 0.8, time.perc = 0.005, dBlevel = 25)
#'
#'# Verify alignment using analysis.type = "twoDshape"
#'eigensound(analysis.type = "twoDshape", wav.at = file.path(wav.at, "Aligned"),
#'           dBlevel = 25, store.at=store.at, plot.exp=TRUE,
#'           flim=c(0, 4), tlim=c(0, 0.8), add.contour = TRUE)
#'# Go to folder specified by store.at and check jpeg files created
#'# If alignment/window dimensions are not ideal, repeat process with new settings
#'
#' }
#'
#'
#' @references
#' Rocha, P. & Romano, P. (2021) The shape of sound: A new \code{R} package that crosses the bridge between Bioacoustics and Geometric Morphometrics. \emph{Methods in Ecology and Evolution, 12}(6), 1115-1121.
#'
#'
#' @export
#'
raven.to.wave <- function(orig.wav.folder=NULL, raven.at=orig.wav.folder, wav.samples="wav samples"){

  if(is.null(orig.wav.folder)) {stop("Use 'orig.wav.folder' to specify folder path where original '.wav' files are stored")}

  # List ".wav" files
  wav.files <- dir(orig.wav.folder, pattern=".wav")

  # List Raven tables
  raven.tables <- dir(raven.at, pattern="selections.txt")

  if(length(raven.tables)==0){
    stop("There are no tables containing selections from Raven Pro software at current folder. File path can be specified by either 'wav.at' or 'raven.at', and files must end with 'selections.txt'  files. Use 'help(raven.to.wav)' for more information")
  }

  if(length(raven.tables)!=length(wav.files)){
    warning("Number of selection tables from Raven Pro software differ from number of '.wav' files at folder specified by 'wav.at'. Some files may not have been analysed.")
  }

  # Create folder to store sample wav files
  if(!dir.exists(wav.samples) &
     !dir.exists(file.path(orig.wav.folder, wav.samples)))
    dir.create(file.path(orig.wav.folder, wav.samples))


  # For each wav file, use Raven selections to create new files
  for(wav in wav.files){

    if(length(
      grep(stringr::str_sub(wav, start=0, end = -5), raven.tables, value=T)) == 0 ){

      stop(paste("The file '", wav, "' has no Raven selection table associated with it. File names from selection tables must be exactly the same as '.wav' files, and end with 'selections.txt'.", sep=""))
    }

    raven.temp <- utils::read.table(file.path(
      orig.wav.folder,
      grep(stringr::str_sub(wav,start=0, end = -5),
           raven.tables, value=T)), h=T, sep="\t")

    # Calculate duration (delta time)
    raven.temp$Delta.Time <- raven.temp$End.Time..s.-raven.temp$Begin.Time..s.

    # Prevent errors related to multiple "View" levels
    raven.temp <- raven.temp[raven.temp$View==levels(as.factor(raven.temp$View))[1],]


    for(i in 1:length(raven.temp$Selection)){

      wav.temp <- tuneR::readWave(file.path(orig.wav.folder, wav), units="seconds",
                                  from= raven.temp$Begin.Time..s.[i] -
                                    raven.temp$Delta.Time[i]*0.15,
                                  to= raven.temp$Begin.Time..s.[i] +
                                    raven.temp$Delta.Time[i]*1.15)

      if(dir.exists(wav.samples)){
        tuneR::writeWave(wav.temp, extensible = T,
                         filename=file.path(
                           wav.samples,
                           ifelse(
                             i<10, paste(stringr::str_sub(wav,start=0, end = -5),
                                         "_sample-0", i, ".wav", sep=""),
                             paste(stringr::str_sub(wav,start=0, end = -5),
                                   "_sample-", i, ".wav", sep="")))) }

      if(!dir.exists(wav.samples)){
        tuneR::writeWave(wav.temp, extensible = T,
                         filename = file.path(orig.wav.folder, wav.samples,
                                              ifelse(i<10,
                                                     paste(stringr::str_sub(wav,start=0, end = -5),
                                                           "_sample-0", i, ".wav", sep=""),
                                                     paste(stringr::str_sub(wav,start=0, end = -5),
                                                           "_sample-", i, ".wav", sep=""))))  }


      rm(wav.temp)

    } # end loop - for each selection

    rm(raven.temp)

  } # end loop - for each wav file

  rm(wav.files, raven.tables)

} # end function
