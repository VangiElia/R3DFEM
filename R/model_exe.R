#' Install and locate the 3D-CMCC-FEM executable
#'
#' The model executable is not shipped with the package. \code{install_model}
#' downloads it (or copies a local file) in a user directory, once;
#' \code{model_path} returns the executable that will be used by
#' \code{\link{run_3DCMCCFEM}} and \code{model_available} tells whether one
#' is available.
#'
#' @param source Character: URL or path of a local file of the executable.
#'   Default to the Windows build of the version 5.7 published in the GitHub
#'   releases of R3DFEM (error on other systems). On Linux and macOS the model must be compiled from
#'   the \href{https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM}{source
#'   code} and the path of the binary given here (or with the option below).
#' @param dir Character: directory where the executable is installed. Default
#'   to \code{tools::R_user_dir("R3DFEM", "data")} (option
#'   \code{R3DFEM.model_dir}).
#' @param overwrite Logical: overwrite an executable already installed?
#'   Default to FALSE
#'
#' @details The executable used by the package is searched, in order, in:
#'   \enumerate{
#'   \item the option \code{R3DFEM.exe}, e.g.
#'   \code{options(R3DFEM.exe = "C:/models/3D_CMCC_Forest_Model_5.7.exe")};
#'   \item the environment variable \code{R3DFEM_EXE};
#'   \item the executable installed by \code{install_model} in \code{dir}.
#'   }
#'
#' @return \code{install_model}: invisibly, the path of the installed
#'   executable. \code{model_path}: the path of the executable (error if not
#'   found). \code{model_available}: TRUE or FALSE.
#' @examples
#' model_available()
#' \dontrun{
#' install_model()
#' # or use an executable already on disk
#' install_model("C:/models/3D_CMCC_Forest_Model_5.7.exe")
#' model_path()
#' }
#' @export
install_model <- function(source = .model_url(), dir = .model_dir(), overwrite = FALSE) {
  # the executable of the GitHub release runs only on Windows
  if (missing(source) && !.is_windows()) {
    stop("The executable available for download runs only on Windows. Compile the model from ",
         "https://github.com/Forest-Modelling-Lab/3D-CMCC-FEM and use ",
         "install_model('path/to/the/executable').")
  }
  stopifnot("source must be a character vector of length 1" = check_char(source),
            "overwrite must be logical" = check_log(overwrite))
  dest <- file.path(dir, .exe_name())
  if (file.exists(dest) && !overwrite) {
    message("The model is already installed in ", dest, " (use overwrite = TRUE to replace it)")
    return(invisible(normalizePath(dest, winslash = "/")))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(source)) {
    if (!file.copy(source, dest, overwrite = TRUE)) stop("Unable to copy ", source, " to ", dir)
  } else if (grepl("^https?://", source)) {
    tmp <- tempfile(fileext = ".exe")
    on.exit(unlink(tmp), add = TRUE)
    download <- function(...) {
      tryCatch(suppressWarnings(utils::download.file(source, tmp, mode = "wb", quiet = TRUE, ...)),
               error = function(e) e)
    }
    failed <- function(status) {
      inherits(status, "error") || !identical(as.integer(status), 0L) ||
        !file.exists(tmp) || file.size(tmp) < 1e5
    }
    status <- download()
    # on some networks Windows cannot reach the certificate revocation server
    # (SSL connect error): retry with curl.exe, checking revocation when possible
    if (failed(status) && .Platform$OS.type == "windows" && nzchar(Sys.which("curl"))) {
      status <- download(method = "curl", extra = "-L --ssl-revoke-best-effort")
    }
    if (failed(status)) {
      stop("Unable to download the model from ", source,
           if (inherits(status, "error")) paste0(" (", conditionMessage(status), ")"),
           ".\nDownload it manually and use install_model('path/to/the/executable').")
    }
    file.copy(tmp, dest, overwrite = TRUE)
  } else {
    stop("'source' must be an existing file or an URL")
  }
  if (.Platform$OS.type == "unix") Sys.chmod(dest, "755")
  message("Model installed in ", dest)
  invisible(normalizePath(dest, winslash = "/"))
}

#' @rdname install_model
#' @export
model_path <- function() {
  exe <- .find_model()
  if (is.null(exe)) {
    stop("The 3D-CMCC-FEM executable was not found. Install it with install_model() ",
         "or set options(R3DFEM.exe = 'path/to/the/executable').")
  }
  exe
}

#' @rdname install_model
#' @export
model_available <- function() {
  !is.null(.find_model())
}

.is_windows <- function() {
  .Platform$OS.type == "windows"
}

.exe_name <- function() {
  if (.is_windows()) "3D_CMCC_Forest_Model_5.7.exe" else "3D_CMCC_Forest_Model_5.7"
}

.model_dir <- function() {
  getOption("R3DFEM.model_dir", tools::R_user_dir("R3DFEM", "data"))
}

.model_url <- function() {
  "https://github.com/VangiElia/R3DFEM/releases/download/model-5.7/3D_CMCC_Forest_Model_5.7.exe"
}

.find_model <- function() {
  candidates <- c(getOption("R3DFEM.exe", ""), Sys.getenv("R3DFEM_EXE"), file.path(.model_dir(), .exe_name()))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) return(NULL)
  normalizePath(candidates[1], winslash = "/")
}
