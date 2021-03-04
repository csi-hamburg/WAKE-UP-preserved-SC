
nn <- dd %>%
  dplyr::filter(ROI == unique(ROI)[1]) %>%
  group_by(treatment) %>%
  summarise(n = n())

d.treatment <- dd %>%
  group_by(ROI) %>%
  nest() %>%
  mutate(
    wct = map(data, ~ wilcox.test(.$nemoscore[.$treatment == "Placebo"],
      .$nemoscore[.$treatment == "rtPA"],
      paired = FALSE
    )),
    p.treatment = map_dbl(wct, ~ .$p.value),
    e.abs.treatment = map_dbl(data, ~ median(.$nemoscore[.$treatment == "Placebo"])
    - median(.$nemoscore[.$treatment == "rtPA"])),
    e.abs.treatment = map_dbl(wct, ~ .$statistic / (nn$n[[1]] * nn$n[[2]])),
    e.rel.treatment = map_dbl(data, ~ (median(.$nemoscore[.$treatment == "Placebo"])
    - median(.$nemoscore[.$treatment == "rtPA"])) / median(.$nemoscore[.$treatment == "Placebo"]))
  ) %>%
  dplyr::select(ROI, p.treatment, e.abs.treatment, e.rel.treatment)
d.treatment



d.treatment.visit <- dd %>%
  group_by(ROI, visit) %>%
  nest() %>%
  mutate(
    wct = map(data, ~ wilcox.test(.$nemoscore[.$treatment == "Placebo"],
      .$nemoscore[.$treatment == "rtPA"],
      paired = FALSE
    )),
    p = map_dbl(wct, ~ .$p.value),
    e.abs = map_dbl(data, ~ median(.$nemoscore[.$treatment == "Placebo"])
    - median(.$nemoscore[.$treatment == "rtPA"])),
    e.abs = map_dbl(wct, ~ .$statistic / (nn$n[[1]] * nn$n[[2]] / 4)),
    e.rel = map_dbl(data, ~ (median(.$nemoscore[.$treatment == "Placebo"])
    - median(.$nemoscore[.$treatment == "rtPA"])) / median(.$nemoscore[.$treatment == "Placebo"]))
  ) %>%
  dplyr::select(ROI, visit, p, e.abs, e.rel) %>%
  gather(name, stats, p, e.abs, e.rel) %>%
  unite(temp, name, visit, sep = ".") %>%
  spread(temp, stats) %>%
  rename_at(vars(-ROI), ~ paste(., "treatment", sep = "."))
d.treatment.visit


d.visit <- dd %>%
  group_by(ROI) %>%
  nest() %>%
  mutate(
    wct = map(data, ~ wilcox.test(.$nemoscore[.$visit == "V3"],
      .$nemoscore[.$visit == "V0"],
      paired = TRUE
    )),
    p.visit = map_dbl(wct, ~ .$p.value),
    e.abs.visit = map_dbl(data, ~ median(.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"])),
    e.abs.visit = map_dbl(data, ~ mean(.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"] > 0)),
    e.rel.visit = map_dbl(data, ~ median((.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"]) / .$nemoscore[.$visit == "V0"]))
  ) %>%
  dplyr::select(ROI, p.visit, e.abs.visit, e.rel.visit)
d.visit

d.visit.treatment <- dd %>%
  group_by(ROI, treatment) %>%
  nest() %>%
  mutate(
    wct = map(data, ~ wilcox.test(.$nemoscore[.$visit == "V3"],
      .$nemoscore[.$visit == "V0"],
      paired = TRUE
    )),
    p = map_dbl(wct, ~ .$p.value),
    e.abs = map_dbl(data, ~ median(.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"])),
    e.abs = map_dbl(data, ~ mean(.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"] > 0)),
    e.rel = map_dbl(data, ~ median((.$nemoscore[.$visit == "V3"] - .$nemoscore[.$visit == "V0"]) / .$nemoscore[.$visit == "V0"]))
  ) %>%
  dplyr::select(ROI, treatment, p, e.abs, e.rel) %>%
  gather(name, stats, p, e.abs, e.rel) %>%
  unite(temp, name, treatment, sep = ".") %>%
  spread(temp, stats) %>%
  rename_at(vars(-ROI), ~ paste(., "visit", sep = "."))

d.visit.treatment


d.ix <- dd %>%
  group_by(ROI) %>%
  nest() %>%
  mutate(
    wct = map(data, ~ wilcox.test(.$nemoscore[.$visit == "V3" & .$treatment == "Placebo"] - .$nemoscore[.$visit == "V0" & .$treatment == "Placebo"],
      .$nemoscore[.$visit == "V3" & .$treatment == "rtPA"] - .$nemoscore[.$visit == "V0" & .$treatment == "rtPA"],
      paired = FALSE
    )),
    p.ix = map_dbl(wct, ~ .$p.value),
    e.ix = -map_dbl(data, ~ median(.$nemoscore[.$visit == "V3" & .$treatment == "Placebo"] - .$nemoscore[.$visit == "V0" & .$treatment == "Placebo"])
    - median(.$nemoscore[.$visit == "V3" & .$treatment == "rtPA"] - .$nemoscore[.$visit == "V0" & .$treatment == "rtPA"])),
    e.ix = map_dbl(wct, ~ .$statistic / (nn$n[[1]] * nn$n[[2]] / 4))
  ) %>%
  dplyr::select(ROI, p.ix, e.ix) %>%
  arrange(p.ix)
d.ix


# t.ROI <- plyr::join_all(list(d.treatment, d.treatment.visit, d.visit, d.visit.treatment, d.ix))
func <- function(...) {
  df1 <- list(...)[[1]]
  df2 <- list(...)[[2]]
  col1 <- colnames(df1)[1]
  col2 <- colnames(df2)[1]
  xxx <- dplyr::full_join(..., by = setNames(col2, col1))
  return(xxx)
}
t.ROI <- Reduce(func, list(d.treatment, d.treatment.visit, d.visit, d.visit.treatment, d.ix))

require(flextable)
tab.ROI <- t.ROI %>%
  dplyr::select(
    ROI,
    e.abs.treatment, p.treatment, e.abs.V0.treatment, p.V0.treatment, e.abs.V3.treatment, p.V3.treatment,
    e.abs.visit, p.visit, e.abs.Placebo.visit, p.Placebo.visit, e.abs.rtPA.visit, p.rtPA.visit,
    e.ix, p.ix
  ) %>%
  ungroup() %>%
  mutate(ROI = forcats::fct_reorder(ROI, -e.ix)) %>%
  arrange(ROI) %>%
  flextable() %>%
  bold(i = ~ p.treatment < 0.05, j = ~p.treatment) %>%
  bold(i = ~ p.V0.treatment < 0.05, j = ~p.V0.treatment) %>%
  bold(i = ~ p.V3.treatment < 0.05, j = ~p.V3.treatment) %>%
  bold(i = ~ p.visit < 0.05, j = ~p.visit) %>%
  bold(i = ~ p.Placebo.visit < 0.05, j = ~p.Placebo.visit) %>%
  bold(i = ~ p.rtPA.visit < 0.05, j = ~p.rtPA.visit) %>%
  bold(i = ~ p.ix < 0.05, j = ~p.ix) %>%
  set_formatter(
    p.treatment = pvalformatter,
    p.V0.treatment = pvalformatter, p.V3.treatment = pvalformatter,
    p.visit = pvalformatter,
    p.Placebo.visit = pvalformatter, p.rtPA.visit = pvalformatter,
    p.ix = pvalformatter
  ) %>%
  set_formatter(
    e.abs.treatment = pvalformatter,
    e.abs.V0.treatment = pvalformatter, e.abs.V3.treatment = pvalformatter,
    e.abs.visit = pvalformatter,
    e.abs.Placebo.visit = pvalformatter, e.abs.rtPA.visit = pvalformatter,
    e.ix = pvalformatter
  ) %>%
  add_header_row(values = c("", "all", "V0", "V3", "all", "Placebo", "rtPA", ""), colwidths = c(1, 2, 2, 2, 2, 2, 2, 2)) %>%
  add_header_row(values = c("", "treatment", "visit", "Interaction"), colwidths = c(1, 6, 6, 2)) %>%
  set_header_labels(values = list(
    e.abs.treatment = "e.s."
    , p.treatment = "P",
    e.abs.V0.treatment = "e.s."
    , p.V0.treatment = "P",
    e.abs.V3.treatment = "e.s."
    , p.V3.treatment = "P",
    e.abs.visit = "e.s."
    , p.visit = "P",
    e.abs.Placebo.visit = "e.s."
    , p.Placebo.visit = "P",
    e.abs.rtPA.visit = "e.s."
    , p.rtPA.visit = "P",
    p.ix = "P",
    e.ix = "e.s."
  )) %>%
  theme_booktabs() %>%
  align(i = c(1, 2), align = "center", part = "header") %>%
  merge_v(part = "header")
tab.ROI

t.ROI <- t.ROI %>%
  mutate_at(vars(starts_with("p")), function(x) log(-log(x))) %>%
  ungroup()



dom.min.visit.p <- t.ROI %>%
  dplyr::select(starts_with("p")) %>%
  dplyr::select(ends_with("visit")) %>%
  summarise_all(min) %>%
  (function(x) min(x[is.finite(as.numeric(x))], na.rm = T))
dom.max.visit.p <- t.ROI %>%
  dplyr::select(starts_with("p")) %>%
  dplyr::select(ends_with("visit")) %>%
  summarise_all(max) %>%
  (function(x) max(x[is.finite(as.numeric(x))], na.rm = T))

dom.min.visit.e <- t.ROI %>%
  dplyr::select(starts_with("e.abs")) %>%
  dplyr::select(ends_with("visit")) %>%
  summarise_all(min) %>%
  (function(x) min(x[is.finite(as.numeric(x))], na.rm = T))
dom.max.visit.e <- t.ROI %>%
  dplyr::select(starts_with("e.abs")) %>%
  dplyr::select(ends_with("visit")) %>%
  summarise_all(max) %>%
  (function(x) max(x[is.finite(as.numeric(x))], na.rm = T))

dom.min.treatment.p <- t.ROI %>%
  dplyr::select(starts_with("p")) %>%
  dplyr::select(ends_with("treatment")) %>%
  summarise_all(min) %>%
  (function(x) min(x[is.finite(as.numeric(x))], na.rm = T))
dom.max.treatment.p <- t.ROI %>%
  dplyr::select(starts_with("p")) %>%
  dplyr::select(ends_with("treatment")) %>%
  summarise_all(max) %>%
  (function(x) max(x[is.finite(as.numeric(x))], na.rm = T))

dom.min.treatment.e <- t.ROI %>%
  dplyr::select(starts_with("e.abs")) %>%
  dplyr::select(ends_with("treatment")) %>%
  summarise_all(min) %>%
  (function(x) min(x[is.finite(as.numeric(x))], na.rm = T))
dom.max.treatment.e <- t.ROI %>%
  dplyr::select(starts_with("e.abs")) %>%
  dplyr::select(ends_with("treatment")) %>%
  summarise_all(max) %>%
  (function(x) max(x[is.finite(as.numeric(x))], na.rm = T))

for (l in colnames(t.ROI)[2:length(colnames(t.ROI))]) {
  #  for (l in c('e.abs.V0.treatment')){
  dom.min <- NULL
  dom.max <- NULL
  div.flag <- stringr::str_starts(l, "e")
  if (div.flag) { ## effect size
    if (stringr::str_ends(l, "visit")) {
      dom.min <- dom.min.visit.e
      dom.max <- dom.max.visit.e
    } else if (stringr::str_ends(l, "treatment")) {
      dom.min <- dom.min.treatment.e
      dom.max <- dom.max.treatment.e
    }
  } else { ## p values
    if (stringr::str_ends(l, "visit")) {
      dom.min <- dom.min.visit.p
      dom.max <- dom.max.visit.p
    } else {
      dom.min <- dom.min.treatment.p
      dom.max <- dom.max.treatment.p
    }
    dom.min <- min(dom.min.treatment.p, dom.min.visit.p)
    dom.min <- max(dom.min, log(-log(0.05)))
    dom.max <- max(dom.max.treatment.p, dom.max.visit.p)
  }
  t.ROI %>%
    dplyr::select(ROI, {{ l }}) %>%
    mutate(p.value = get(l)) %>%
    mutate(p.value = if_else(p.value < log(-log(0.05)) & stringr::str_starts(l, "p."), -Inf, p.value)) %>%
    createFSoverlay(
      asz = asz, label = l, divergent = div.flag,
      dom.min = dom.min, dom.max = dom.max
    )
}

