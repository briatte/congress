meta = c(
  "cty" = "United States",
  "hr" = "House of Representatives",
  "se" = "Senate",
  "type-hr" = "Lower",
  "type-se" = "Upper",
  "ipu-hr" = 2339, # Inter-Parliamentary Union identifier
  "ipu-se" = 2340, # Inter-Parliamentary Union identifier
  "seats-hr" = 435, # statutory chamber size
  "seats-se" = 100  # statutory chamber size
)

for (ii in unique(b$congress) %>% as.character %>% as.integer %>% sort) {

  for (jj in unique(b$chamber) %>% sort) {

    cat(meta[ jj ], ii)
    data = filter(b, chamber == jj, congress == ii, n_au > 1)
    sp = filter(s, mandate == jj) %>% data.frame

    # find terms corresponding to congressional session and those before
    sp = filter(sp, start < max(data$date))

    # subset sponsors to those showing up the bills
    m = c(data$sponsor, data$cosponsors %>% strsplit(";") %>% unlist) %>% unique

    # compute seniority and subset to most recent row
    sp = filter(sp, thomas %in% m) %>%
      arrange(thomas) %>%
      group_by(thomas) %>%
      mutate(nyears = substr(min(start), 1, 4) %>% as.integer,
             nyears = substr(start, 1, 4) %>% as.integer - nyears) %>%
      filter(nyears == max(nyears)) %>%
      data.frame

    # sanity check: single row per sponsor
    stopifnot(!duplicated(sp$thomas))

    # build names
    sp$name = sp$first
    sp$name[ !is.na(sp$middle) ] = paste(sp$name[ !is.na(sp$middle) ],
                                         sp$middle[ !is.na(sp$middle) ])
    sp$name = paste(sp$name, sp$last)
    sp$name[ !is.na(sp$suffix) ] = paste(sp$name[ !is.na(sp$suffix) ],
                                         sp$suffix[ !is.na(sp$suffix) ])

    if (any(duplicated(sp$name))) {

      # cat(":", sum(duplicated(sp$name)), "duplicate name(s)")
      sp = group_by(sp, name) %>%
        mutate(n = n(), seq = 1:n()) %>%
        group_by %>%
        mutate(name = ifelse(n > 1, paste0(name, "-", seq), name)) %>%
        data.frame

    }

    # missing sponsors
    m = data$sponsor[ !data$sponsor %in% sp$thomas ]
    if (length(m)) {

      # cat(":", n_distinct(m), "missing sponsor(s)")
      data = filter(data, !sponsor %in% m)

    }

    m = data$cosponsors %>% strsplit(";") %>% unlist
    m = m[ !m %in% sp$thomas ]
    if (length(m)) {

      # cat(": missing", n_distinct(m), "cosponsor(s)")

    }

    cat(":", nrow(data), "cosponsored bills, ")

    # bill dates to years
    data$date = substr(data$date, 1, 4) %>% as.integer

    # ==========================================================================
    # DIRECTED EDGE LIST
    # ==========================================================================

    edges = paste0(data$sponsor, ";", data$cosponsors)
    edges = lapply(edges, function(d) {

      w = unlist(strsplit(d, ";"))

      d = expand.grid(i = sp$name[ sp$thomas %in% w ],
                      j = sp$name[ sp$thomas == w[1]], stringsAsFactors = FALSE)

      return(data.frame(d, w = length(w) - 1)) # number of cosponsors

    }) %>% bind_rows

    # ==========================================================================
    # EDGE WEIGHTS
    # ==========================================================================

    # first author self-loops, with counts of cosponsors
    self = filter(edges, i == j)

    # count number of bills per first author
    n_au = table(self$j)

    # remove self-loops from directed edge list
    edges = filter(edges, i != j)

    # count number of bills cosponsored per sponsor
    n_co = table(edges$i)

    # identify directed ties
    edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

    # raw edge counts
    raw = table(edges$ij)

    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

    # expand to edge list
    edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                       j = gsub("(.*)///(.*)", "\\2", edges$ij),
                       raw = as.vector(raw[ edges$ij ]), # raw edge counts
                       nfw = edges$w)

    # Gross-Shalizi weights (weighted propensity to cosponsor)
    edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
    edges$gsw = edges$nfw / edges$w

    # sanity check
    stopifnot(edges$gsw <= 1)

    # final edge set: cosponsor, first author, weights
    edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]

    cat(nrow(edges), "edges, ")

    # ==========================================================================
    # DIRECTED NETWORK
    # ==========================================================================

    n = network(edges[, 1:2 ], directed = TRUE)

    n %n% "country" = meta[ "cty" ] %>% as.character
    n %n% "years" = paste0(min(data$date, na.rm = TRUE), "-",
                           min(data$date, na.rm = TRUE) + 1) %>% as.character
    n %n% "congress" = ii
    n %n% "chamber" = meta[ jj ] %>% as.character
    n %n% "type" = meta[ paste0("type-", jj) ] %>% as.character
    n %n% "ipu" = meta[ paste0("ipu-", jj) ] %>% as.integer
    n %n% "seats" = meta[ paste0("seats-", jj) ] %>% as.integer
    n %n% "n_cosponsored" = nrow(data)
    n %n% "n_sponsors" = table(filter(b, chamber == jj, congress == ii)$n_au)

    # ==========================================================================
    # VERTEX-LEVEL ATTRIBUTES
    # ==========================================================================

    n_au = as.vector(n_au[ network.vertex.names(n) ])
    n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

    n_co = as.vector(n_co[ network.vertex.names(n) ])
    n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

    n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

    cat(network.size(n), "nodes\n")

    rownames(sp) = sp$name
    n %v% "thomas" = sp[ network.vertex.names(n), "thomas" ]
    n %v% "icpsr" = sp[ network.vertex.names(n), "icpsr" ]
    n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
    n %v% "born" = sp[ network.vertex.names(n), "born" ]
    n %v% "party" = sp[ network.vertex.names(n), "party" ]
    n %v% "state" = sp[ network.vertex.names(n), "state" ]
    n %v% "district" = sp[ network.vertex.names(n), "district" ]
    n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ]

    set.edge.attribute(n, "source", as.character(edges[, 1]))
    set.edge.attribute(n, "target", as.character(edges[, 2]))

    set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
    set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

    set.edge.attribute(n, "raw", edges$raw) # raw edge counts
    set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
    set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

    # ==========================================================================
    # SAVE NETWORK OBJECTS
    # ==========================================================================

    assign(paste0("net_us_",  jj, ii), n)
    assign(paste0("edges_us_", jj, ii), edges)
    assign(paste0("bills_us_", jj, ii), data)

    # ==========================================================================
    # PLOT STRONG TIES
    # ==========================================================================

    if (jj == "hr")
      delete.edges(n, which(n %e% "gsw" < .45))

    if (jj == "se")
      delete.edges(n, which(n %e% "gsw" < .15))

    n %v% "color" = "grey25" # the few other parties
    n %v% "color" = ifelse(n %v% "party" == "REP", "firebrick", n %v% "color")
    n %v% "color" = ifelse(n %v% "party" == "DEM", "royalblue", n %v% "color")
    n %v% "color" = ifelse(n %v% "party" == "IND", "forestgreen", n %v% "color")

    png(paste0("plots/", jj, str_pad(ii, 3, pad = "0"), ".png"),
        width = 600, height = 600)
    plot(n, vertex.col = n %v% "color", vertex.border = n %v% "color",
         edge.col = "grey50",
         main = paste(n %n% "chamber", n %n% "congress",
                      paste0("(", min(data$date, na.rm = TRUE), "–",
                             min(data$date, na.rm = TRUE) + 1, ")"),
                      "\nWPC ≥", ifelse(jj == "hr", "0.45", "0.15")))
    dev.off()

  }

}

save(list = ls(pattern = "^(bills|edges|net)_us"), file = "data/net_us.rda")
