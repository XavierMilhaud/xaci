# Set your CDS Personal Access Token

Wrapper around
[`wf_set_key`](https://rdrr.io/pkg/ecmwfr/man/wf_set_key.html) for the
new CDS API (post-October 2024). The token is stored securely in the
system keyring and never needs to appear in your scripts again.

## Usage

``` r
cds_set_key(token)
```

## Arguments

- token:

  Character. Your CDS Personal Access Token (UUID format).

## Value

Invisibly `NULL`. The token is saved in the system keyring.

## Details

To obtain your token:

1.  Create a free account at <https://cds.climate.copernicus.eu>.

2.  Go to *your profile* → *Personal Access Token*.

3.  Copy the token (a UUID string) and pass it to this function once.

## Examples

``` r
if (FALSE) { # \dontrun{
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
} # }
```
