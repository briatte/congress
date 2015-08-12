library(dplyr)
library(readr)

library(tnet) # loads igraph -- keep first
library(network)
library(sna)

#===============================================================================
# LOAD NETWORKS
#===============================================================================

rm(list = ls())

# load networks
load("data/net_us.rda")

# parameters
n_w = 1:50 # sequence of Walktrap random steps over which to maximize modularity
n_s = 1000 # number of graphs to simulate, keeping the degree distribution fixed
n_p = 1000 # number of permutations of the partition vector to simulate

#===============================================================================
# GET NETWORK OBJECTS
#===============================================================================

# collect cosponsorship networks
m = mget(ls(pattern = "^net_us"))

cat("Generating weighted objects: raw ")

# weighted by raw
raw = sapply(m, as.matrix, attrname = "raw")
raw_i = lapply(raw, graph.adjacency, mode = "directed", weighted = TRUE)
raw_t = sapply(raw, as.tnet, "weighted one-mode tnet") # tnet, assumed directed

cat("nfw ")

# weighted by nfw
nfw = sapply(m, as.matrix, attrname = "nfw")
nfw_i = lapply(nfw, graph.adjacency, mode = "directed", weighted = TRUE)
nfw_t = sapply(nfw, as.tnet, "weighted one-mode tnet") # tnet, assumed directed

cat("gsw ")

# weighted by gsw
gsw = sapply(m, as.matrix, attrname = "gsw")
gsw_i = lapply(gsw, graph.adjacency, mode = "directed", weighted = TRUE)
gsw_t = sapply(gsw, as.tnet, "weighted one-mode tnet") # tnet, assumed directed

cat("done.\n")

#===============================================================================
# CORRELATION MATRIX PLOT
#===============================================================================

panel.cor <- function(x, y, digits = 2, ...) {

  usr = par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  r = abs(cor(x, y, use = "complete.obs", method = "pearson"))
  text(0.5, 0.5, cex = 2, format(c(r, 0.123456789), digits = digits)[1])

}

panel.density <- function(x) {

  usr = par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))

  tryd = try(d <- density(x, na.rm = TRUE, bw = "nrd",
                          adjust = 1.2), silent = TRUE)

  if (class(tryd) != "try-error") {

    d$y = d$y / max(d$y)
    lines(d)

  }

  rug(x)

}

# ==============================================================================
# PARTISAN AND MAXIMIZED SCORES
# ==============================================================================

cat("Computing empirical and maximum modularity...\n")

s = data_frame()

pb = txtProgressBar(0, length(m), style = 3)

for (i in names(m)) {

  # partition vector
  p = factor(m[[i]] %v% "party")
  names(p) = network.vertex.names(m[[i]])

  stopifnot(V(raw_i[[i]])$name == names(p))
  stopifnot(V(nfw_i[[i]])$name == names(p))
  stopifnot(V(gsw_i[[i]])$name == names(p))

  max_unw = lapply(n_w, function(x)
    cluster_walktrap(raw_i[[i]], steps = x, weights = NULL)) %>%
    sapply(modularity)

  max_raw = lapply(n_w, function(x)
    cluster_walktrap(raw_i[[i]], steps = x)) %>%
    sapply(modularity)

  max_nfw = lapply(n_w, function(x)
    cluster_walktrap(nfw_i[[i]], steps = x)) %>%
    sapply(modularity)

  max_gsw = lapply(n_w, function(x)
    cluster_walktrap(gsw_i[[i]], steps = x)) %>%
    sapply(modularity)

  s = rbind(s, data_frame(
    network = i,
    modularity = modularity(raw_i[[i]], p, weights = NULL),
    modularity_max = max_unw[ which.max(max_unw) ],
    modularity_raw = modularity(raw_i[[i]], p, weights = E(raw_i[[i]])$weight),
    modularity_raw_max = max_raw[ which.max(max_raw) ],
    modularity_nfw = modularity(nfw_i[[i]], p, weights = E(nfw_i[[i]])$weight),
    modularity_nfw_max = max_nfw[ which.max(max_nfw) ],
    modularity_gsw = modularity(gsw_i[[i]], p, weights = E(gsw_i[[i]])$weight),
    modularity_gsw_max = max_gsw[ which.max(max_gsw) ]
  ))

  setTxtProgressBar(pb, which(names(m) == i))

}

write_csv(s, paste0("data/modularity_", length(n_w), "_steps.csv"))

cat("\n")

pdf("plots/modularity_corr.pdf", width = 9, height = 9)
pairs(select(s, starts_with("modularity")),
      diag.panel = panel.density, lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off()

# ==============================================================================
# RANDOM GRAPH SCORES
# ==============================================================================

cat("Computing modularity on random graphs...\n")
s = data_frame()

pb = txtProgressBar(0, length(m), style = 3)

for (i in names(m)) {

  # empirical component: partition vector
  p = factor(m[[i]] %v% "party")
  names(p) = network.vertex.names(m[[i]])

  for (j in 1:n_s) {

    # randomized component: rewired graph
    r = cbind(sample(m[[i]] %e% "source"), sample(m[[i]] %e% "target")) %>%
      graph.edgelist

    s = rbind(
      s,
      data_frame(
        network = i, weights = "unw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = NULL)),
      data_frame(
        network = i, weights = "raw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "raw")),
      data_frame(
        network = i, weights = "nfw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "nfw")),
      data_frame(
        network = i, weights = "gsw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "gsw"))
    )

  }

  setTxtProgressBar(pb, which(names(m) == i))

}

write_csv(s, paste0("data/modularity_", n_s, "_null.csv"))

cat("\n")

# ==============================================================================
# RANDOM PERMUTATION SCORES
# ==============================================================================

cat("Computing modularity on permuted partitions...\n")
s = data_frame()

pb = txtProgressBar(0, length(m), style = 3)

for (i in names(m)) {

  # empirical component: original graph
  r = cbind(m[[i]] %e% "source", m[[i]] %e% "target") %>% graph.edgelist

  for (j in 1:n_s) {

    # randomized component: partition vector
    p = factor(m[[i]] %v% "party") %>% sample
    names(p) = network.vertex.names(m[[i]])

    s = rbind(
      s,
      data_frame(
        network = i, weights = "unw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = NULL)),
      data_frame(
        network = i, weights = "raw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "raw")),
      data_frame(
        network = i, weights = "nfw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "nfw")),
      data_frame(
        network = i, weights = "gsw", sim = j,
        modularity = modularity(r, p[ V(r)$name ], weights = m[[i]] %e% "gsw"))
    )

  }

  setTxtProgressBar(pb, which(names(m) == i))

}

write_csv(s, paste0("data/modularity_", n_s, "_perm.csv"))

cat("\n")
