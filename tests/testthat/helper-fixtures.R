fixture <- function(name) system.file("extdata", name, package = "sciparseR", mustWork = TRUE)
truth <- function(name) sub("\\.(pdf|xml)$", ".truth.json", fixture(name))
metric <- function(ev, m) ev$value[ev$metric == m]
