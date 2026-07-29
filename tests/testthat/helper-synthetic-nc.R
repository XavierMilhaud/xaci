# ---------------------------------------------------------------------------
# helper-synthetic-nc.R
#
# Generateurs de donnees synthetiques (NetCDF ERA5-like + masque pays),
# extraits du meme pattern que vignette("xaci-components"). Charges
# automatiquement par testthat (prefixe "helper-") avant tous les tests,
# a utiliser dans n'importe quel test-*.R sans le redupliquer.
# ---------------------------------------------------------------------------

#' Construit un fichier NetCDF ERA5-like [lon x lat x time] pour une variable
#'
#' @keywords internal
build_synthetic_nc <- function(path, var, unit, lon, lat, time_vec, origin, vals) {
  time_hours <- as.numeric(difftime(time_vec, origin, units = "hours"))
  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def(
    "time", paste0("hours since ", format(origin, "%Y-%m-%d %H:%M:%S")),
    time_hours, unlim = TRUE
  )
  ncvar <- ncdf4::ncvar_def(var, unit, list(dim_lon, dim_lat, dim_time),
                             missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(ncvar))
  ncdf4::ncvar_put(nc, ncvar, vals)
  ncdf4::nc_close(nc)
  invisible(path)
}

#' Construit un masque pays NetCDF [lon x lat] (variable "country")
#'
#' @keywords internal
build_synthetic_mask <- function(path, lon, lat, mask_vals = NULL) {
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_mask <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(var_mask))
  if (is.null(mask_vals)) mask_vals <- matrix(1, length(lon), length(lat))
  ncdf4::ncvar_put(nc, var_mask, mask_vals)
  ncdf4::nc_close(nc)
  invisible(path)
}
