library(dplyr)
library(readr)

library(ggplot2)
library(tidyr)

#===============================================================================
# PARTISAN VS MAXIMUM MODULARITY
#===============================================================================

s = read_csv("data/modularity_50_steps.csv")
s$chamber = ifelse(grepl("hr", s$network), "House", "Senate")
s$network = gsub("\\D", "", s$network) %>% as.integer

s = gather(s, key, value, -network, -chamber)
s$key = as.character(s$key)
s$key[ s$key == "modularity" ] = "unw"
s$key[ s$key == "modularity_max" ] = "unw_max"
s$key = gsub("modularity_", "", s$key)
s$max = ifelse(grepl("max", s$key), "Maximized modularity (Walktrap 1-50 steps)",
               "Empirical modularity (party-based)")
s$key = gsub("_max", "", s$key)
s$key = factor(s$key, c("unw", "raw", "nfw", "gsw"))

qplot(data = s, x = network, y = value, linetype = max, geom = "line") +
  facet_grid(chamber ~ key) +
  scale_linetype_discrete("") +
  labs(x = "\nCongress", y = "Modularity\n") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"))

ggsave("plots/modularity_max.pdf", width = 10, height = 7)
ggsave("plots/modularity_max.png", width = 10, height = 7)

#===============================================================================
# RANDOM GRAPH TESTS
#===============================================================================

s = read_csv("data/modularity_50_steps.csv")
s = gather(s, weights, empirical, -network)
s$weights = as.character(s$weights)
s$weights[ s$weights == "modularity" ] = "unw"
s$weights = gsub("modularity_", "", s$weights)
s = filter(s, !grepl("max", weights))

r = read_csv("data/modularity_1000_null.csv") %>%
  group_by(network, weights) %>%
  summarise(q01 = quantile(modularity, .01),
            q99 = quantile(modularity, .99))

r = left_join(s, r, by = c("network", "weights"))

r$chamber = ifelse(grepl("hr", r$network), "House", "Senate")
r$network = gsub("\\D", "", r$network) %>% as.integer
r$weights = factor(r$weights, c("unw", "raw", "nfw", "gsw"))

qplot(data = r, x = network, y = empirical, geom = "line") +
  geom_segment(aes(xend = network, y = q01, yend = q99), alpha = .5) +
  facet_grid(chamber ~ weights) +
  labs(x = "\nCongress", y = "Empirical modularity vs. random edge rewiring\n") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"))

ggsave("plots/modularity_null.pdf", width = 10, height = 6)
ggsave("plots/modularity_null.png", width = 10, height = 6)

#===============================================================================
# PARTITION PERMUTATION TESTS
#===============================================================================

s = read_csv("data/modularity_50_steps.csv")
s = gather(s, weights, empirical, -network)
s$weights = as.character(s$weights)
s$weights[ s$weights == "modularity" ] = "unw"
s$weights = gsub("modularity_", "", s$weights)
s = filter(s, !grepl("max", weights))

r = read_csv("data/modularity_1000_perm.csv") %>%
  group_by(network, weights) %>%
  summarise(q01 = quantile(modularity, .01),
            q99 = quantile(modularity, .99))

r = left_join(s, r, by = c("network", "weights"))

r$chamber = ifelse(grepl("hr", r$network), "House", "Senate")
r$network = gsub("\\D", "", r$network) %>% as.integer
r$weights = factor(r$weights, c("unw", "raw", "nfw", "gsw"))

qplot(data = r, x = network, y = empirical, geom = "line") +
  geom_segment(aes(xend = network, y = q01, yend = q99), alpha = .5) +
  facet_grid(chamber ~ weights) +
  labs(x = "\nCongress", y = "Empirical modularity vs. random partition permutations\n") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = rel(1)),
        strip.background = element_rect(fill = "grey90"))

ggsave("plots/modularity_perm.pdf", width = 10, height = 6)
ggsave("plots/modularity_perm.png", width = 10, height = 6)
