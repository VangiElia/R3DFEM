#' Create topo input file for 3DCMCCFEM model
#'
#' \code{make_topo} create the topo.txt file following the 3D-CMCC-FEM model
#' specification
#'
#' @param site Character: Name of the site. Used to construct output name
#' @param elev Numeric vector: elevation of the site
#' @param outdir Character: path to the output directory.
#'
#' @details The function can accept a vector of elevation and return as many
#'   files as the length of the \code{elev} vector.The output topo.txt file has
#'   the columns: \itemize{ \item \emph{X}: x cell position \item \emph{Y}: y
#'   cell position \item \emph{ELEV}: Elevation of the stand
#'
#'   } For more info on the topo input data needed see the official \href{
#'   http://eprints.bice.rm.cnr.it/22393/1/3D-CMCC-FEM_User_Guide_v.2_July_2023.pdf}{User's
#'    Guide}
#'
#' @return Used for its side effects.It save in \code{outdir} as many topo
#'   input files as the length of \code{elev} provided.
#' @seealso \code{\link[=run_3DFEM]{run_3DFEM}}
#' @export



make_topo <- function(site=NULL,elev,outdir){

  check_dir(outdir)
  stopifnot("elev must be a number " =  check_num_vect(elev))

  if (is.null(site)) {
    warning("Argument 'site' is missing with no default. A temporary name will be given.")
    site <- "tmp"
  }

  if(!is.null(site)&length(site)!=length(elev)){
    stop("If site is not NULL should be the same length of elev")
  }

  topo <- data.frame(X = 0, Y = 0, ELEV = elev)

  for (i in seq_len(nrow(topo))) {
    write.table(
      topo[i,],
      paste0(file.path(outdir, site[i]),"_topo.txt"),
      row.names = F,
      col.names = T,
      quote = FALSE,
      sep = ","
    )
    message("file ",  paste0(file.path(outdir, site[i]),"_topo.txt"), " saved")
  }
}

#make_topo(1:5,"Trento","E:/3DCMCCFEMR/elia/")
