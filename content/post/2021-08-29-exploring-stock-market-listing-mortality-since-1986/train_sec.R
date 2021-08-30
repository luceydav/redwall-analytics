
# Function factory for secondary axis transforms
train_sec <- function(primary, secondary, na.rm = TRUE) {
  # Thanks Henry Holm for including the na.rm argument!
  from <- range(secondary, na.rm = na.rm)
  to   <- range(primary, na.rm = na.rm)
  # Forward transform for the data
  forward <- function(x) {
    rescale(x, from = from, to = to)
  }
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    rescale(x, from = to, to = from)
  }
  list(fwd = forward, rev = reverse)
}