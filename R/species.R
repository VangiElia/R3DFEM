#' Species parameterizations of the version 5.7
#'
#' The package ships the parameterization files of the version 5.7 of the
#' model for the main European forest species. \code{list_species} lists them,
#' \code{species_file} returns their paths and \code{copy_species} copies them
#' in an input directory.
#'
#' @param species Character vector: species names (name of the file without
#'   .txt, e.g. "Piceaabies"). See \code{list_species()}.
#' @param outdir Character: directory where the files are copied (e.g. the
#'   \code{inputdir} of \code{\link{run_3DCMCCFEM}}).
#' @param overwrite Logical: overwrite files already in \code{outdir}?
#'   Default to FALSE, i.e. a site-specific parameterization already in
#'   \code{outdir} is kept.
#'
#' @return \code{list_species}: a character vector of species names.
#'   \code{species_file}: named paths of the files. \code{copy_species}:
#'   invisibly, the paths of the files in \code{outdir}.
#' @seealso \code{\link{read_species}}, \code{\link{set_species_param}},
#'   \code{\link{setup_inputs}}
#' @examples
#' list_species()
#' head(read_species(species_file("Fagussylvatica")))
#' @export
list_species <- function() {
  files <- list.files(.species_dir(), pattern = "\\.txt$")
  sub("\\.txt$", "", files)
}

#' @rdname list_species
#' @export
species_file <- function(species) {
  stopifnot("species must be a character vector" = is.character(species))
  miss <- setdiff(species, list_species())
  if (length(miss) != 0) {
    stop("No parameterization for: ", paste(miss, collapse = ", "),
         ". Available species: ", paste(list_species(), collapse = ", "))
  }
  stats::setNames(file.path(.species_dir(), paste0(species, ".txt")), species)
}

#' @rdname list_species
#' @export
copy_species <- function(species, outdir, overwrite = FALSE) {
  files <- species_file(unique(species))
  check_dir(outdir)
  to <- file.path(outdir, basename(files))
  keep <- file.exists(to) & !overwrite
  if (any(keep)) message("Kept existing file(s): ", paste(basename(to[keep]), collapse = ", "))
  file.copy(files[!keep], to[!keep], overwrite = TRUE)
  invisible(to)
}

.species_dir <- function() {
  system.file("extdata", "species", package = "R3DFEM")
}
