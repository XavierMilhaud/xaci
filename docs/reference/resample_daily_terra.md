# Resample a SpatRaster to daily resolution (terra version)

Equivalent of [`resample_daily()`](resample_daily.md). Groups layers by
calendar day and applies `fun` via
[`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html).

## Usage

``` r
resample_daily_terra(r, fun = "mean", filename = "", target_chunk_gb = 1)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  set (sub-daily time steps expected), sorted chronologically.

- fun:

  Aggregation function name understood by
  [`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
  (e.g. `"mean"`, `"sum"`, `"max"`, `"min"`).

- filename:

  Optional path to write the final result directly to disk (highly
  recommended for large jobs). Default `""` (terra decides).

- target_chunk_gb:

  Target raw INPUT size (GB) per temporal chunk, used to decide how many
  chunks are needed for memory safety. Conservative default `1` – lower
  it further if you still see
  [`mem.maxVSize()`](https://rdrr.io/r/base/memlimits.html)/OOM crashes
  on your machine; raise it (fewer, larger chunks, faster overall) only
  if you have RAM headroom to spare.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with one layer per day.

## Details

**Memory note :** pour agreger par jour,
[`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
doit lire, PAR BLOC SPATIAL, la serie temporelle COMPLETE de `r`
(`readValues()` recupere `nrows x ncol x nlyr(r)` valeurs d'un coup,
puis [`matrix()`](https://rdrr.io/r/base/matrix.html) en fait une copie
contigue). Sur une grille pourtant petite (quelques milliers de
cellules), `nlyr(r)` horaire sur plusieurs decennies (des centaines de
milliers de couches) suffit a lui seul a depasser la limite memoire d'un
processus R (constate empiriquement : plafond de 16 Go de R sous macOS,
"vector memory limit ... reached", pour ~65 ans horaires sur la France
entiere) – MEME quand le resultat final (agrege par jour) est minuscule.
C'est exactement la meme classe de probleme que celle documentee pour
[`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)
dans `.calculate_percentiles_terra_tiled()` : le pic memoire depend de
la taille de L'ENTREE lue par bloc, pas de la sortie. On applique donc
ici le meme principe, en decoupant TEMPORELLEMENT (et non spatialement,
puisque c'est le nombre de couches qui explose ici, pas le nombre de
cellules) : `r` est scinde en blocs de jours CONSECUTIFS (donc de
couches horaires contigues – l'ordre chronologique de `r` est requis),
chaque bloc est agrege independamment et ecrit sur disque, puis les
blocs journaliers (petits) sont recombines. Le resultat est identique,
bloc par bloc ou en un seul appel : chaque jour est agrege a partir de
ses propres couches horaires, jamais a cheval sur deux blocs.

**Precision numerique :** les blocs intermediaires sont ecrits sur
disque en `datatype = "FLT8S"` (double precision, 64 bits) –
explicitement force pour eviter que `terra` ne bascule par defaut sur du
float 32 bits, ce qui romprait l'invariance du resultat par rapport a
`target_chunk_gb` (chemin non-decoupe = calcul garde en RAM, en double
precision R). Le resultat est ainsi, a l'arithmetique flottante pres
(memes operations, meme ordre par groupe), rigoureusement identique quel
que soit le nombre de blocs.
