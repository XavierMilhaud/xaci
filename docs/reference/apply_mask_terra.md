# Apply a country mask to a SpatRaster

Equivalent of [`apply_mask()`](apply_mask.md): sets cells to `NA` where
the mask value is below `threshold`. Processed block-by-block by terra.

## Usage

``` r
apply_mask_terra(r, mask_path, threshold = 0.8, chunk_size = 20000)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  (e.g. from [`load_netcdf_terra()`](load_netcdf_terra.md)).

- mask_path:

  Path to the mask NetCDF file (variable: `country`).

- threshold:

  Numeric threshold. Default `0.8`.

- chunk_size:

  Nombre max de couches traitees par bloc quand `nlyr(r)` depasse 65535.
  Default `20000` (marge confortable sous la limite, ajustable selon la
  RAM/disque disponibles). Une valeur superieure a 65535 est
  silencieusement plafonnee a 65535, puisque chaque bloc est lui-meme
  ecrit en GeoTIFF et soumis a cette meme limite.

## Value

The masked
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Details

**Note sur la limite de 65535 couches :** le format interne utilise par
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)
pour ecrire son resultat sur disque (fichier temporaire) ne supporte pas
plus de 65535 couches en sortie. Au-dela (cas frequent en donnees
horaires sur plusieurs decennies : ~300k couches pour 35 ans), on ne
peut pas masquer l'objet en un seul appel. Cette fonction bascule donc
automatiquement sur un traitement par blocs temporels (\`chunk_size\`
couches a la fois), ecrits en GeoTIFF (BigTIFF) puis recombines en une
seule source multi-fichiers, sans jamais materialiser l'ensemble en RAM.
Le masque etant purement spatial (identique a chaque pas de temps), le
decoupage en blocs ne change pas le resultat : un pixel exclu par le
masque l'est de la meme facon dans chaque bloc.

**Precision numerique :** `terra` ecrit sur disque en simple precision
(`datatype = "FLT4S"`) PAR DEFAUT (voir
[`terra::terraOptions()`](https://rspatial.github.io/terra/reference/terraOptions.html)),
des que le resultat ne tient pas en RAM – ce qui inclut la branche par
blocs ci-dessous (TOUJOURS ecrite sur disque). Sans le
`datatype = "FLT8S"` explicite ci-dessous, un masquage qui declenche une
ecriture disque perdrait silencieusement de la precision par rapport a
un masquage qui reste en RAM – incoherence potentielle non liee au
chunking lui-meme, mais au meme risque general (voir la note similaire
dans [`resample_daily_terra()`](resample_daily_terra.md)).
