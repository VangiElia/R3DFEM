#' Read an output file of the 3DCMCCFEM model
#'
#' \code{read_output} reads a daily, monthly or annual output of
#' \code{\link{run_3DCMCCFEM}} as a data.frame, dropping the model settings
#' written by the model at the end of the file.
#'
#' @param file Character: path to an output file, e.g. one of the paths
#'   returned by \code{\link{run_3DCMCCFEM}}. Several paths can be given (e.g.
#'   the outputs of \code{\link{run_batch}}): the outputs are bound by row and
#'   a column \code{file} identifies them.
#' @param date Logical: add a column DATE (Date class) for daily and monthly
#'   outputs (first day of the month)? Default to TRUE
#'
#' @return A data.frame. The species column is always named SPECIES (the
#'   model writes SPECIE in the annual output). Column names are kept as
#'   written by the model (e.g. "BP/NPP").
#' @seealso \code{\link{run_3DCMCCFEM}}, \code{\link{plot_output_3DFEM}}
#' @export
read_output <- function(file, date = TRUE) {
  stopifnot("file must be a character vector" = is.character(file),
            "date must be logical" = check_log(date))
  miss <- file[!file.exists(file)]
  if (length(miss) != 0) stop("File(s) not found: ", paste(miss, collapse = ", "))

  if (length(file) > 1) {
    out <- lapply(file, function(f) cbind(file = f, read_output(f, date = date)))
    return(as.data.frame(data.table::rbindlist(out, fill = TRUE)))
  }

  lines <- readLines(file, warn = FALSE)
  # the model settings are written after the data, in lines starting with '#'
  end <- which(startsWith(lines, "#"))
  if (length(end) != 0) lines <- lines[seq_len(end[1] - 1)]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2) stop("No data in ", file)

  df <- as.data.frame(data.table::fread(text = lines, sep = ",", header = TRUE, check.names = FALSE))
  # trailing separators produce empty columns
  df <- df[, nzchar(colnames(df)) & !grepl("^V\\d+$", colnames(df)), drop = FALSE]
  colnames(df)[colnames(df) == "SPECIE"] <- "SPECIES"

  if (date && "MONTH" %in% colnames(df)) {
    day <- if ("DAY" %in% colnames(df)) df$DAY else 1
    df$DATE <- as.Date(sprintf("%04d-%02d-%02d", as.integer(df$YEAR), as.integer(df$MONTH), as.integer(day)))
  }
  df
}
