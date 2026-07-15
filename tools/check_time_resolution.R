# =============================================================================
# Diagnostic : quelle est la resolution temporelle d'un fichier NetCDF ?
# (fonctionne pour t2m, u10, v10, tp, ou n'importe quelle variable ERA5)
# =============================================================================

library(ncdf4)

check_time_resolution <- function(path) {
  nc <- nc_open(path)
  on.exit(nc_close(nc))

  time_raw  <- ncvar_get(nc, "time")
  time_atts <- ncatt_get(nc, "time")
  cat("Attribut 'units' du temps :", time_atts$units, "\n")
  cat("Nombre total de pas de temps :", length(time_raw), "\n")

  # Ecart entre les 2 premieres valeurs, dans l'unite du fichier (heures ou jours)
  step <- time_raw[2] - time_raw[1]
  cat("Ecart entre 2 pas consecutifs (dans l'unite du fichier) :", step, "\n")

  # Reconstruire les vraies dates pour le 1er et le dernier pas
  origin <- sub(".*(since )(.+)", "\\2", time_atts$units)
  unit   <- trimws(sub(" since.*", "", time_atts$units))
  mult   <- if (grepl("hour", unit)) 3600 else if (grepl("day", unit)) 86400 else NA

  if (!is.na(mult)) {
    t_start <- as.POSIXct(origin, tz = "UTC") + time_raw[1]     * mult
    t_second<- as.POSIXct(origin, tz = "UTC") + time_raw[2]     * mult
    t_end   <- as.POSIXct(origin, tz = "UTC") + time_raw[length(time_raw)] * mult
    cat("Premiere date        :", format(t_start),  "\n")
    cat("Deuxieme date        :", format(t_second), "\n")
    cat("Derniere date        :", format(t_end),    "\n")
    cat("-> Resolution reelle :", format(difftime(t_second, t_start, units = "auto")), "\n")
  }
}

# Exemple :
#check_time_resolution("/Users/XM/Library/Mobile Documents/com~apple~CloudDocs/2-Recherche/RPackages-dev/Global-xaci/data/era5/FRA/t2m_2011_2015.nc")
#check_time_resolution("/Users/XM/Library/Mobile Documents/com~apple~CloudDocs/2-Recherche/RPackages-dev/Global-xaci/data/era5/FRA/tp_2011_2015.nc")
#check_time_resolution("/Users/XM/Library/Mobile Documents/com~apple~CloudDocs/2-Recherche/RPackages-dev/Global-xaci/data/era5/FRA/u10_2011_2015.nc")
#check_time_resolution("/Users/XM/Library/Mobile Documents/com~apple~CloudDocs/2-Recherche/RPackages-dev/Global-xaci/data/era5/FRA/v10_2011_2015.nc")
