##' Yet another progress bar
##' 
##' A progress bar that uses files to store progress counters so that it can be 
##' used together with parallel computations and progress can be monitored
##' in an R independent way.
##' 
##' This progress bar is based on \code{\link{txtProgressBar}}. In addition to 
##' using a local variable associated with the progress bar object to keep track 
##' of the progress, this progress bar also uses files to store the progress. 
##' The intention is that the progress bar works even when computations are 
##' carried out in parallel. It works by setting the total number \code{N}
##' of iterations when creating the \code{progressBar} object. In each 
##' interation a counter is incremented, and the progress bar is updated with 
##' the percentage of completion as measured by the current value of the counter 
##' relative to \code{N}. 
##' 
##' It has only been tested with \code{\link{mclapply}} from the 
##' \code{\link{parallel}} package. Option \code{pid = TRUE} only gives 
##' meaningful results in this case when \code{mc.preschedule = TRUE} for 
##' \code{mclapply} (which is the default). 
##' 
##' With a huge number of interations and little work in each, the file IO 
##' can produce a notable overhead. The \code{interval} argument control how 
##' frequently the files are accessed. 
##' 
##' 
##' @param N a \code{numeric}. The total number of iterations.
##' @param initial a \code{numeric}. The initial value of the counter.
##' @param char a \code{character}. The character used to draw the progress bar.
##' @param width a \code{numeric}. The line width used when printing the progress bar. 
##'        If \code{NA} (the default), it is \code{getOption("width") - 10L}.
##' @param totalBar a \code{logical}. Should the progress bar be drawn for the 
##'        total progress. If \code{FALSE} only the percent completion is reported.
##' @param file a \code{character}. The file name used for writing the progress 
##'        bar. The default value, \code{""}, means \code{\link{stdout}}. 
##' @param pid a \code{logical}. Should the progress for the individual processes, 
##'        according to their pid, be reported (default value \code{FALSE}). 
##'        Only considered if \code{file != ""}. In this case, the individual 
##'        progress bars are written to \code{paste(file, "Pid", sep = "")}.
##' @param interval a \code{numeric}. The time in seconds between file access. 
##'        Default value is \code{0.1}. 
##' @return An object of class \code{progressBar} containing two functions 
##'         \code{up} and \code{kill}. Call \code{up()} to increment the counter 
##'         and update the progress bar. Remember to call \code{kill()} after the 
##'         progress bar is used to clean up.
##' @examples
##' pb <- progressBar(100)
##' for(i in 1:100) {
##'   Sys.sleep(0.1)
##'   pb$up()
##' }
##' pb$kill()  ## Remember this to clean up. Removes files and directories used.
##' rm(pb)
##' 
##' \dontrun{
##' pb <- progressBar(100)
##' library(parallel)
##' tmp <- mclapply(1:100, function(i) {
##'   Sys.sleep(0.1)
##'   pb$up()
##'   }
##' )
##' pb$kill()  ## Remember this to clean up. Removes files and directories used.
##' rm(pb)}
##' @export           

progressBar <- function (N, 
                         initial = 0, 
                         char = "=", 
                         width = NA, 
                         totalBar = TRUE, 
                         file = "", 
                         pid = FALSE,
                         interval = 0.1) {
  .time <- Sys.time()
  .killed <- FALSE
  .val <- initial
  .valread <- numeric()
  M <- N
  pbDir <- paste(".__progressBar", round(runif(1) * 1e6), "/", sep = "")
  if(file.exists(pbDir))
    stop(paste("The progress bar cannot create the '", pbDir, "' directory. The file/directory already exists.", sep = ""))
  dir.create(pbDir)
  pbDir <- paste(getwd(), "/", pbDir, sep = "")
  if (file == "") {
    cr <- "\r "
    nl <- ""
    if(pid) {
      pid <- FALSE
      message("Cannot report progress for individual processes to the console.")
    } 
  } else {
    cr <- ""
    nl <- "\n"
    pidfile <- paste(file, "Pid", sep = "")
  }
  nw <- nchar(char, "w")
  if (is.na(width)) 
    width <- getOption("width") - 10L
  width <- trunc(width/nw)
  
  up <- function(value) {
    if (.killed)
      return(invisible())
    if (missing(value)) {
      .val <<- .val + 1 
    } else {
      .val <<- value
    }
    tmp <- Sys.time()
    if (tmp - .time > interval || .val >= M) {
      .time <<- tmp
      countfile <- paste(pbDir, Sys.getpid(), sep = "")
      cat(.val, "\n", file = countfile)
      
      pids <- dir(pbDir)
      M <<- floor( N / length(pids) )
      val <- sapply(paste(pbDir, pids, sep = ""), 
                    function(file) 
                      scan(file, quiet = TRUE)[1], USE.NAMES = FALSE)    
      value <- sum(val, na.rm = TRUE)
      value <- max(min(value, N), 0)
      nb <- round(width * value / N)
      pc <- round(100 * value / N)
      if (totalBar) {
        cat(paste(c(cr, "|", rep.int(char, nb), 
                    rep.int(" ", nw * (width - nb)), 
                    sprintf("| %3d%%", pc), nl), 
                  collapse = ""), 
            file = file)
      } else {
        cat(cr, pc, "%", nl, sep = "", file = file)
      }
      flush.console()
      if (pid) {
        result <- character(length(val))
        for (i in seq_along(val)) {
          if (is.na(val[i])) {
            if (!is.na(.valread[i])) {
              val[i] <- .valread[i] 
            } else {
              val[i] <- 0
            }
          }
          value <- max(min(val[i], M), 0)
          nb <- round(width * value / M)
          pc <- round(100 * value / M)
          result[i] <- paste(c("pid ", pids[i], ":  |", 
                               rep.int(char, nb), 
                               rep.int(" ", nw * (width - nb)), 
                               sprintf("| %3d%%", pc), "\n"), 
                             collapse = "")
          cat(result, file = pidfile, sep = "")
        }
      }
      .valread <<- val
    }
    invisible()
  }
  
  kill <- function() {
    if (!.killed) {
      pids <- dir(pbDir)
      pbFiles <- paste(pbDir, pids, sep = "")
      if (length(pids) > 0)
        file.remove(pbFiles)
      file.remove(pbDir)
      .killed <<- TRUE
    }
  }
  structure(list(up = up, kill = kill), class = "progressBar")
}