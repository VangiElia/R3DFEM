# Internal engine of make_meteo_EOBS()
#
# version: E-OBS version used in the file names (e.g. "29.0", "30.0")
# last_date: last date covered by that version
# eobs_dir: directory of the E-OBS netCDF files
# file_name: function(site, id) returning the output file name

.make_meteo_EOBS <- function(plot, crs, buffer, id, daterange, convert_rad, parallel,
                             site, outdir, return_df, version, last_date, eobs_dir, file_name) {

  terra::gdalCache(46567)

  check_dir(outdir)

  if (is.null(site)) {
    warning("Argument 'site' is missing with no default. A temporary name will be given.")
    site <- "tmp"
  }

  stopifnot("daterange must be a character vector of length 2"=check_len_char(daterange,2))
  ref_date <- as.Date(c("1950-01-01", last_date))
  tmp <- as.Date(daterange)
  if (anyNA(tmp)) stop("daterange must be in the form yyyy-mm-dd")
  if (tmp[1] < ref_date[1]) stop("starting date must be after 1950-01-01")
  if (tmp[2] > ref_date[2]) stop("ending date must be before ", last_date)
  if (tmp[1] > tmp[2]) stop("starting date must be before ending date")

  if (!inherits(plot, "sf")) {
    if (is.null(crs)) {
      stop("If argument 'plot' is not an sf object, the crs of coordinates must be given")
    }
    if (is.na(sf::st_crs(crs))) {
      stop("Argument 'crs' should be something suitable as input to sf::st_crs")
    }
    plot <- sf::st_as_sf(as.data.frame(plot), coords = c(1, 2), crs = crs)
  }

  if (nrow(plot) != 1 && !check_char(id)) {
    stop("Argument 'id' must be a character vector of length 1")
  }

  plot <- sf::st_transform(plot, 4326)
  plot <- sf::st_buffer(plot, buffer)

  # E-OBS variables: hu = relative humidity, qq = global radiation,
  # rr = precipitation, tg/tn/tx = mean/min/max temperature
  vars <- c("hu", "qq", "rr", "tg", "tn", "tx")
  rst <- vapply(vars, function(v) {
    file.path(eobs_dir, paste0(v, "_ens_mean_0.1deg_reg_v", version, "e.nc"))
  }, "")
  if (any(!file.exists(rst))) {
    stop("E-OBS v", version, " file(s) not found in ", eobs_dir, " for: ",
         paste(vars[!file.exists(rst)], collapse = ", "), ". Set options(R3DFEM.eobs_dir = ...) or the argument eobs_dir")
  }

  if (parallel) {
    if (!requireNamespace("doFuture", quietly = TRUE)) stop("Package 'doFuture' is needed for parallel = TRUE")
    # honor the future plan set by the user, otherwise use 7 workers
    if (inherits(future::plan(), "sequential")) {
      oplan <- future::plan("multisession", workers = 7)
      on.exit(future::plan(oplan), add = TRUE)
      message("Using future multisession with 7 workers")
    }
    `%d%` <- doFuture::`%dofuture%`
  } else {
    message('Executing extraction sequentially')
    `%d%` <- foreach::`%do%`
  }

  extract_var <- function(v, path) {
    eobs <- terra::rast(path)
    time_eobs <- terra::time(eobs)
    layers <- which(time_eobs >= tmp[1] & time_eobs <= tmp[2])
    eobs <- eobs[[layers]]
    ex <- exactextractr::exact_extract(eobs, plot, "mean", append_cols = id, progress = FALSE)
    vals <- as.matrix(ex[, setdiff(colnames(ex), id), drop = FALSE])
    # rows = plots, columns = dates
    out <- data.frame(date = rep(as.character(time_eobs[layers]), each = nrow(ex)))
    if (!is.null(id)) out[[id]] <- rep(ex[[id]], times = length(layers))
    out[[v]] <- as.vector(vals)
    out
  }

  meteo <- foreach::foreach(v = vars, path = rst,
                            .options.future = list(packages = c("terra", "exactextractr"))) %d% {
    extract_var(v, path)
  }
  meteo <- Reduce(function(a, b) merge(a, b, by = c(id, "date")), meteo)
  meteo <- meteo[do.call(order, unname(meteo[c(id, "date")])), ]

  d <- as.Date(meteo$date)
  out <- data.frame(Year = as.integer(format(d, "%Y")),
                    Month = as.integer(format(d, "%m")),
                    n_days = as.integer(format(d, "%d")),
                    Rg_f = meteo$qq,
                    Ta_f = meteo$tg,
                    Tmax = meteo$tx,
                    Tmin = meteo$tn,
                    RH_f = meteo$hu,
                    Ts_f = -9999,
                    Precip = meteo$rr,
                    SWC = -9999,
                    LAI = -9999,
                    ET = -9999,
                    WS_f = -9999)

  if (convert_rad) {
    # W m-2 -> MJ m-2 day-1
    out$Rg_f <- out$Rg_f * 0.0864
  }

  if (!is.null(id)) {
    idd <- unique(meteo[[id]])
    files <- file.path(outdir, vapply(idd, function(x) file_name(site, x), ""))
    for (i in seq_along(idd)) {
      utils::write.table(out[meteo[[id]] == idd[i], ], files[i], row.names = F, col.names = T, quote = FALSE, sep = "\t")
    }
    out <- cbind(meteo[id], out)
  } else {
    files <- file.path(outdir, file_name(site, NULL))
    utils::write.table(out, files, row.names = F, col.names = T, quote = FALSE, sep = "\t")
  }
  rownames(out) <- NULL

  message("file(s) ", paste(files, collapse = ", "), " saved")
  if (return_df) {
    return(out)
  }
  invisible(files)
}
