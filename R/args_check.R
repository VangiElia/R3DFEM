check_len_num  <-  function(x, len=1) {
  return (!is.null(x) && (length(x) == len && is.numeric(x)))
}
check_num  <-  function(x) {
  return (check_len_num(x, 1))
}

check_num_vect  <-  function(x) {
  return (!is.null(x) && is.numeric(x))
}

check_len_char <-  function(x,len) {
  return (!is.null(x) && (length(x) == len && is.character(x)))
}

check_char <-  function(x) {
  return (check_len_char(x,1))
}

check_log <-  function(x) {
  return (!is.null(x) && (length(x) == 1 && is.logical(x)))
}

check_dir <-  function(x) {
  return (if (missing(x) ||
              is.null(x)) {
    stop("outdir must be provided")
  } else if (!dir.exists(x)) {
    message("Outdir does not exist. It will be created")
    dir.create(x)
  })
}

