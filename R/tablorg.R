##' Adding a configuration entry 
##' 
##' Adds a configuration entry to the table or creates a new table with the 
##' configuration entry if no table exists.
##' 
##' @param conf a \code{list}.
##' @param script a \code{string}. The filename including the path to the script
##'        that will be executed based on the configuration.
##' @param template a \code{string}. A HTML template for the table. Either the 
##'        template as a text string or the path to a file containing the template.
##'        Default value \code{""} means that no template is used.
##' @param path a \code{string}. The root directory of the computations where 
##'        the table will be stored.
##' @param run a \code{logical}. Should the script be run. Default value is 
##'        \code{TRUE}.
##' @return an object of class 'tablorg'. 
##' @export
addConf <- function(conf, 
                    script = "", 
                    template = "", 
                    path = ".", 
                    run = TRUE) {
  path <- normalizePath(path)
  if (!file.exists(path))
    dir.create(path)
  confID <- digest::digest(conf)  ## Digest used as id for the configuration
  confDir <- paste(path, "/", confID, sep = "")
  tablFile <- paste(path, "/.tablorg.txt", sep = "")
  if (file.exists(tablFile)) {
    tabl <- jsonlite::fromJSON(tablFile)
    if (class(tabl$data) != "data.frame")
      stop(paste("Problem with json table file:", tablFile))
    ## Converting non character columns to character to capute 
    ## pure NA columns that are logical
    notChar <- which(sapply(tabl$data, class) != "character")
    for (i in notChar) 
      tabl$data[, i] <- as.character(tabl$data[, i])      
    if (!confID %in% rownames(tabl$data)){
      entry <- createEntry(conf, path, confID)
      suppressMessages(tabl$data <- dplyr:::full_join(tabl$data, entry))
      active <- nrow(tabl$data)
    } else {
      active <- which(confID == rownames(tabl$data))
    }
  } else {  ## No tablFile. Creating new table
    entry <- createEntry(conf, path, confID)
    tabl <- list(data = entry)
    active <- 1
  }
  createTabl(colnames(tabl$data), path = path, template = template)
  save(list = names(conf), 
       envir = as.environment(conf), 
       file = paste(confDir, "/.conf.RData", sep = ""))
  createScript(script, confDir, confID)
  tabl$path <- path
  tabl$file <- tablFile
  tabl$active <- active
  class(tabl) <- c("tablorg", "list")
  write.tablorg(tabl)
  if (run) 
    runEntry(tabl)
  invisible(tabl)
}

##' Runs a tablorg entry script
##' 
##' Given a tablorg object this function executes the script corresponding to 
##' one entry in the table. 
##'
##' @param tabl a \code{tablorg} object.
##' @param which an \code{integer}. Specifies which of the entries in the tablorg 
##'        object that will be executed. If missing, the active entry will be 
##'        executed.
##' @param view a \code{logical}. Should the results be 
##' @return URL for the path to the resulting output file (invisibly).
##' @export
runEntry <- function(tabl, 
                     which, 
                     view = TRUE) {
  if (missing(which))
    which <- tabl$active
  confID <- rownames(tabl$data)[which]
  confDir <- paste(tabl$path, "/", confID, sep = "")
  file <- paste(confDir, "/", confID, ".Rmd", sep = "")
  result <- paste("file://", confDir, "/", confID, ".html", sep = "")
  tabl$data[which, "Status"] <- "Running"
  write.tablorg(tabl)
  if (file.exists(file)) {
    sink(file = paste(confDir, "/knitr.txt", sep = ""))
    cacheOpts <- knitr::opts_chunk$get("cache")
    knitr::opts_chunk$set(cache = TRUE)
    rmarkdown::render(file, 
                      envir = new.env(),
                      quiet = TRUE)
    knitr::opts_chunk$set(cache = cacheOpts)  
    sink()
    if (view)
      browseURL(result)
  } else {
    warning(paste("File", file, "does not exists"))
  }
  tabl$data[which, "Status"] <- "Done"
  write.tablorg(tabl)  
  invisible(result)
}

##' @method print tablorg
##' @export
print.tablorg <- function(x, ...) {
  cat(x$path)
  cat("\nActive entry:\n")
  print(x$data[x$active, ])
}

write.tablorg <- function(x, ...)
  write(jsonlite::toJSON(x, pretty = TRUE, na = "string"), file = x$file)
 
createEntry <- function(conf, path, confID) {  
  confNames <- names(conf)
  path <- normalizePath(path)
  confDir <- paste(path, "/", confID, sep = "")
  if (!file.exists(confDir))
    dir.create(confDir)
  for (i in seq_along(conf)) {
    if (is.vector(conf[[i]]) && length(conf[[i]]) == 1) {
      conf[[i]] <- as.character(conf[[i]])
    } else {
      entryID <- digest::digest(conf[[i]], algo = "xxhash32")
      file <- paste(confDir, "/", entryID, ".txt", sep = "")
      if (is.matrix(conf[[i]]) || is.data.frame(conf[[i]])) {
        write.table(conf[[i]], file =file) 
      } else {
        write(format(conf[[i]]), file = file)
      }
      conf[[i]] <- paste("<a href=\"", confID, "/", entryID, ".txt\"", ">", 
                         entryID, "</a>", sep = "")
    }
  }
  firstCol <- paste("<a href=\"", confID, "/", confID, ".html\"", ">", 
                    Sys.time(), "</a>", sep = "")
  conf <- c(firstCol, "Not run", conf)
  entry <- as.data.frame(conf, stringsAsFactors = FALSE)
  rownames(entry) <- confID
  colnames(entry) <- c("Date", "Status", confNames)
  entry
}

createTabl <- function(names, path, template = "", title = "Configuration table") {
  dataNames <- character(1)
  tableNames <- "<tr>"
  for (i in seq_along(names)) {
    dataNames <- paste(dataNames, "{ \"data\": \"", names[i], "\" },\n", sep = "")
    tableNames <- paste(tableNames, "<th>", names[i], "</th>", sep = "")
  }
  tableNames <- paste(tableNames, "</tr>")
  tablPage <- paste("
<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"//cdn.datatables.net/1.10.7/css/jquery.dataTables.css\">
<script type=\"text/javascript\" charset=\"utf8\" src=\"//code.jquery.com/jquery-1.11.1.min.js\"></script>
<script type=\"text/javascript\" charset=\"utf8\" src=\"//cdn.datatables.net/1.10.7/js/jquery.dataTables.js\"></script>

<script>    
$(document).ready( function () {
      var table = $('#tablorg').DataTable( {
        \"ajax\": \".tablorg.txt\",
        \"columns\": [
",
        dataNames,
        "
  ]
} );

setInterval( function () {
  table.ajax.reload(); // user paging is not reset on reload
}, 5000 );
} );
</script>
<title>",
        title,
        "
</title>
</head>
<body>",
        template,
        "
<table id=\"tablorg\" class=\"display\">
<thead>",
        tableNames,
        "
</thead>
<tfoot>",
        tableNames,
        "
</tfoot>
</table>
</body>
</html>", sep = "")
  write(tablPage, file = paste(path, "/index.html", sep = ""))
}

createScript <- function(script, confDir, confID) {
  if (file.exists(script)) {
    file.copy(script, 
              paste(confDir, "/", confID, ".Rmd", sep = ""),
              overwrite = TRUE) }
  else {
    warning(paste("No file name", script))
  }
}
  








    
  