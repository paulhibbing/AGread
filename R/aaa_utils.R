unzip_zipped_gt3x = function(path, cleanup = TRUE) {
  if (length(path) == 0) return(path)
  stopifnot(length(path) == 1)
  exts = sapply(path, tools::file_ext)
  exts = tolower(exts)
  unzip_these = exts %in% c("gz", "bz", "bz2", "xz")
  # don't decompress if the file doesn't exist
  fe = file.exists(path)
  fe_before = file.exists(sub(paste0("[.]", exts), "", path))
  if (any(unzip_these & fe)) {
    zipped_files = path[unzip_these & fe]
    zip_exts = exts[unzip_these & fe]
    zip_outfiles = mapply(function(x, y) {
      FUN = switch(y,
                   bz = bzfile,
                   bz2 = bzfile,
                   gz = gzfile,
                   xz = xzfile)
      R.utils::decompressFile(
        x,
        ext = y,
        FUN = FUN,
        remove = FALSE,
        overwrite = TRUE,
        temporary = TRUE)
    }, zipped_files, zip_exts)
    path[unzip_these & fe] = zip_outfiles
  }

  attr(path, "remove") = unzip_these & cleanup & !fe_before
  path
}
