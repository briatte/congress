# ==============================================================================
# BILLS PARSER
# ==============================================================================

for (i in 114:93) {

  cat("Congress", i)
  f = paste0("data/bills-", i, ".zip")

  if (!file.exists(f))
    download.file(paste0("http://unitedstates.sunlightfoundation.com/congress/",
                         "data/", i, ".zip"), f, mode = "wb", quiet = TRUE)

  d = paste0("data/bills-", i, ".csv")
  if (!file.exists(d)) {

    z = unzip(f, list = TRUE)$Name
    z = z[ grepl("bills/", z) ]

    cat(" : parsing", length(z), "bills...\n") # slow but avoids unzipping
    b = data_frame()
    s = c()

    pb = txtProgressBar(0, length(z), style = 3)
    for (j in z) {

      x = fromJSON(unz(f, filename = j), flatten = TRUE)

      if (!is.null(x$sponsor)) {

        # excluding cosponsors who withdrew their signature
        y = x$cosponsors$thomas_id[ is.na(x$cosponsors$withdrawn_at) ]
        y = ifelse(!length(y), NA, paste0(y, collapse = ";"))

        w = x$cosponsors$thomas_id[ !is.na(x$cosponsors$withdrawn_at) ]
        w = ifelse(!length(w), NA, paste0(w, collapse = ";"))

        y = with(x, data_frame(file = j, date = introduced_at, status,
                               sponsor = x$sponsor$thomas_id, cosponsors = y,
                               withdrawn = w))

        b = rbind(b, y)

      } else {

        s = c(j, s)

      }

      setTxtProgressBar(pb, which(z == j))

    }

    if (length(s) > 0)
      cat("\nCongress", i, ": skipped", length(s), "sponsor-less bill(s)\n")

    write.csv(b, d, row.names = FALSE)

  } else {

    b = read.csv(d, stringsAsFactors = FALSE)
    cat(": loaded", nrow(b), "bills...\n")

  }

}

# ==============================================================================
# BILLS MASTER DATASET
# ==============================================================================

b = data_frame()
for (i in list.files("data", pattern = "bills-\\d+\\.csv$", full.names = TRUE)) {

  x = read.csv(i, colClasses = "character", stringsAsFactors = FALSE)
  b = rbind(b, cbind(congress = gsub("\\D", "", i), x))

}

y = data_frame(chamber = gsub("bills/", "", dirname(b$file)))
y$type = dirname(y$chamber)
y$number = gsub("\\D", "", y$chamber)
y$chamber = substr(y$chamber, 1, 1)

b = cbind(y, select(b, -file))
b$chamber = ifelse(b$chamber == "h", "hr", "se")

b$n_au = 2 + str_count(b$cosponsors, ";")
b$n_au[ is.na(b$cosponsors) ] = 1

table(b$n_au > 1)
table(b$n_au > 1) %>% prop.table # ~ 2/3 cosponsored

# ==============================================================================
# SPONSORS PARSER
# ==============================================================================

r = "https://github.com/unitedstates/congress-legislators/blob/master/"

f = "data/legislators-current.yaml"
if (!file.exists(f))
  download.file(paste0(r, "legislators-current.yaml?raw=true"), f, mode = "wb", quiet = TRUE)

d = yaml.load_file(f)
s_c = data_frame()

for (x in d) {

  icpsr = x$id$icpsr # sometimes missing
  icpsr = ifelse(is.null(icpsr), NA, icpsr)

  y = with(x, data_frame(thomas = id$thomas, icpsr,
                         first = name$first,
                         middle = ifelse(is.null(name$middle), NA, name$middle),
                         last = name$last,
                         suffix = ifelse(is.null(name$suffix), NA, name$suffix),
                         born = bio$birthday, sex = bio$gender))

  z = sapply(x$terms, function(x) {
    ifelse(is.null(x$district), NA, x$district)
  })

  z = data_frame(mandate = sapply(x$terms, function(x) x$type), # rep or sen
                 start = sapply(x$terms, function(x) x$start),
                 end = sapply(x$terms, function(x) x$end),
                 party = sapply(x$terms, function(x) x$party),
                 state = sapply(x$terms, function(x) x$state),
                 district = z)

  s_c = rbind(s_c, cbind(y, z, row.names = NULL))

}

f = "data/legislators-historical.yaml"
if (!file.exists(f))
  download.file(paste0(r, "legislators-historical.yaml?raw=true"), f, mode = "wb", quiet = TRUE)

d = yaml.load_file(f)
s_h = data_frame()

for (x in d) {

  if (is.null(x$id$thomas)) # speed up things: ignore older sponsors
    next

  icpsr = x$id$icpsr # sometimes missing
  icpsr = ifelse(is.null(icpsr), NA, icpsr)

  born = x$bio$birthday # sometimes missing (unlike current sponsors file)
  born = ifelse(is.null(born), NA, born)

  y = with(x, data_frame(thomas = id$thomas, icpsr,
                         first = name$first,
                         middle = ifelse(is.null(name$middle), NA, name$middle),
                         last = name$last,
                         suffix = ifelse(is.null(name$suffix), NA, name$suffix),
                         born, sex = bio$gender))

  z = sapply(x$terms, function(x) {
    ifelse(is.null(x$district), NA, x$district)
  })

  z = data_frame(mandate = sapply(x$terms, function(x) x$type), # rep or sen
                 start = sapply(x$terms, function(x) x$start),
                 end = sapply(x$terms, function(x) x$end),
                 party = sapply(x$terms, function(x) x$party),
                 state = sapply(x$terms, function(x) x$state),
                 district = z)

  s_h = rbind(s_h, cbind(y, z, row.names = NULL))

}

s = rbind(s_h, s_c)
s$mandate = ifelse(s$mandate == "rep", "hr", "se")

# ==============================================================================
# FINALIZE VARIABLES
# ==============================================================================

s$party[ s$party %in% c("Democrat", "Democrat-Liberal", "Ind. Democrat") ] = "DEM"
s$party[ s$party %in% c("Republican", "Republican-Conservative", "Conservative") ] = "REP"
s$party[ s$party == "Independent" ] = "IND"
s$party[ !s$party %in% c("DEM", "REP", "IND") ] = "OTH" # Puerto Rico parties
table(s$party, exclude = NULL)

s$born = substr(s$born, 1, 4)
table(s$born, exclude = NULL)
