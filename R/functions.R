# Shared helpers for the analysis notebooks. Mechanical repetition only:
# theming, palette, figure sizing and export, factor coding, slope reporting,
# the cross-experiment summary rows, participant profiles, beliefs,
# memory-check summaries, and the NASA-TLX task-load figure. Model fitting
# stays inline in the notebooks.

# Model-fitting control shared by all mixed() calls
evl_lmer_control <- lme4::lmerControl(optCtrl = list(maxfun = 1e6))

# Semantic palette: one colour per meaning across every figure. Hedonic
# judgments are warm, monetary judgments blue, and everything that is not a
# judgment (task-load components and levels, control-study marks, profile and
# belief bars) is teal. The data layer (points, lines, error bars, bars) is
# drawn entirely in its series colour and the structure (axes, ticks, text)
# in black. Two series sharing a panel must also differ in shape, fill (open
# vs filled) or linetype so every figure reads in greyscale; `Open` is the
# white fill of the second series.
evl_colours <- c(
  Liking = "#E69F00", Wanting = "#D55E00",
  Price = "#0072B2", Seller = "#0072B2", Buyer = "#579BDB",
  Open = "white",
  Teal = "#009E73", "Teal Dark" = "#007A58", "Teal Light" = "#57BD96"
)

# Monetary axis labels for the 1-5 price scale ($100 to $500), half steps included
label_price <- function(x) paste0("$", x * 100)

# NASA-TLX components in panel order, grouped into the workload block and the
# two affective components
tlx_blocks <- c(
  Mental = "Workload", Physical = "Workload", Temporal = "Workload",
  Performance = "Workload", Effort = "Workload",
  Frustration = "Affective", Pleasure = "Affective"
)
tlx_components <- names(tlx_blocks)

# Order the component column and add its block
tlx_factor <- function(data) {
  data %>%
    mutate(
      component = factor(component, levels = tlx_components),
      block = factor(tlx_blocks[as.character(component)], levels = c("Workload", "Affective"))
    )
}

# Colour scale over the semantic palette (levels are matched by name), plus
# the matching fill scale where open and filled points share a panel
scale_evl <- function(..., fill = FALSE) {
  scales <- list(scale_colour_manual(values = evl_colours, ...))
  if (fill) scales <- c(scales, list(scale_fill_manual(values = evl_colours, ...)))
  scales
}

# House plot theme: cowplot base at 10 pt with thin black axes, transparent
# centered strips, and panels of a fixed physical size, square by default
# (`panel` in mm: one value for a square, width and height for a rectangle).
# Fixed panels give every figure the same scale of text, marks, and data area,
# and let a figure take its own size (figure_size()). The mark sizes (points,
# lines, bar outlines, text labels) and the teal fill for bars left unmapped
# are carried by the theme so that they scope to the plots that use it. The
# legend is hidden unless `legend` names a position.
theme_evl <- function(legend = "none", panel = 40) {
  panel <- rep_len(panel, 2)
  cowplot::theme_cowplot(font_size = 10) +
    theme(
      geom = element_geom(
        pointsize = 2, borderwidth = 0.35, linewidth = 0.55, fontsize = 8,
        fill = evl_colours[["Teal"]]
      ),
      axis.line = element_line(linewidth = 0.4),
      axis.ticks = element_line(linewidth = 0.4),
      legend.position = legend,
      legend.justification = c(0.5, 0.5),
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.background = element_blank(),
      strip.clip = "off",
      panel.spacing = unit(0.8, "lines"),
      # a size per panel (a single unit would size the panel area as a whole)
      panel.widths = unit(rep(panel[1], 2), "mm"),
      panel.heights = unit(rep(panel[2], 2), "mm")
    )
}

# Width and height (mm) of a figure with fixed panels: the sum of its layout,
# widened where a legend below or above is wider than the plot, measured in the
# export device so that text extents are exact
figure_size <- function(plot) {
  layout <- if (is_ggplot(plot)) ggplotGrob(plot) else plot
  current <- grDevices::dev.cur()
  ragg::agg_png(tempfile(fileext = ".png"), res = 300)
  on.exit({
    grDevices::dev.off()
    if (current > 1) grDevices::dev.set(current)
  })
  mm <- \(x) grid::convertWidth(x, "mm", valueOnly = TRUE)
  legends <- layout$grobs[layout$layout$name %in% c("guide-box-bottom", "guide-box-top")]
  margins <- mm(layout$widths[1]) + mm(layout$widths[length(layout$widths)])
  c(
    max(mm(sum(layout$widths)), vapply(legends, \(g) mm(grid::grobWidth(g)), numeric(1)) + margins),
    grid::convertHeight(sum(layout$heights), "mm", valueOnly = TRUE)
  )
}

# Show a figure in the notebook at its own size, embedded at its physical
# width; outside a render it is drawn on the current device
show_figure <- function(plot) {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    grid::grid.newpage()
    grid::grid.draw(plot)
    return(invisible(plot))
  }
  size <- figure_size(plot)
  path <- knitr::fig_path(".png")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(path, plot, width = size[1], height = size[2], units = "mm", dpi = 300,
         device = ragg::agg_png, bg = "white")
  knitr::include_graphics(path, dpi = 300)
}

# Export a figure to output/ as .tiff (the print master: LZW, 600 dpi, which
# meets the usual print floors of 300 dpi for colour art and 600 dpi for
# black-and-white) and .png (a 300-dpi preview), at its own size, on an opaque
# white background (the cowplot theme has no plot background, which ggsave
# would otherwise export as transparent). Both files come from the same
# rasteriser (ragg), so the preview is the print image at half the resolution.
# The on-screen title and subtitle are dropped from the export, since the
# caption carries the title in print; the notebook shows the figure as drawn on
# screen.
save_figure <- function(plot, name) {
  export <- if (is_ggplot(plot)) plot + labs(title = NULL, subtitle = NULL) else plot
  size <- figure_size(export)
  ggsave(
    here::here("output", paste0(name, ".tiff")),
    plot = export, width = size[1], height = size[2], units = "mm", dpi = 600,
    device = ragg::agg_tiff, compression = "lzw", bg = "white"
  )
  ggsave(
    here::here("output", paste0(name, ".png")),
    plot = export, width = size[1], height = size[2], units = "mm", dpi = 300,
    device = ragg::agg_png, bg = "white"
  )
  show_figure(plot)
}

# Stack figures with fixed panels into one, each at its own size and centered
stack_figures <- function(...) {
  layouts <- lapply(list(...), \(p) if (is_ggplot(p)) ggplotGrob(p) else p)
  sizes <- vapply(layouts, figure_size, numeric(2))
  gtable::gtable_col(
    "figure", layouts,
    width = unit(max(sizes[1, ]), "mm"), heights = unit(sizes[2, ], "mm")
  )
}

# Effort levels in order; the numeric level index used by the trend models
# counts them 1 to 3
effort_levels <- c("Low", "Medium", "High")

# Establish the effort-level ordering lost in the CSV round-trip, plus the
# numeric level index
set_effort_levels <- function(data) {
  data %>%
    mutate(
      effort_level = factor(effort_level, levels = effort_levels),
      effort_num = as.numeric(effort_level)
    )
}

# Report effort slopes in the reporting form, one markdown line per judgment
# and per slope difference: b, 95% CI, t(df), p. `slopes` is an emtrends
# frame fitted with infer = TRUE and `differences` its pairs(); `labels`
# recodes judgment names for display, e.g. c(Value = "Price").
report_slopes <- function(slopes, differences = NULL, labels = NULL) {
  format_p <- function(p) {
    ifelse(p < .001, "p < .001", paste0("p = ", sub("^0", "", sprintf("%.3f", p))))
  }
  relabel <- function(x) {
    x <- as.character(x)
    for (name in names(labels)) x <- gsub(name, labels[[name]], x, fixed = TRUE)
    x
  }
  line <- function(name, symbol, estimate, lower, upper, df, t, p) {
    sprintf(
      "- %s: %s = %.2f, 95%% CI [%.2f, %.2f], t(%.0f) = %.2f, %s",
      name, symbol, estimate, lower, upper, df, t, format_p(p)
    )
  }
  key <- function(frame, upto) {
    cols <- names(frame)[seq_len(match(upto, names(frame)) - 1)]
    relabel(do.call(paste, c(frame[cols], sep = ", ")))
  }
  s <- as.data.frame(slopes)
  trend <- grep("\\.trend$", names(s), value = TRUE)
  out <- line(key(s, trend), "b", s[[trend]], s$lower.CL, s$upper.CL, s$df, s$t.ratio, s$p.value)
  if (!is.null(differences)) {
    d <- as.data.frame(differences)
    out <- c(out, line(key(d, "estimate"), "Δb", d$estimate, d$lower.CL, d$upper.CL,
                       d$df, d$t.ratio, d$p.value))
  }
  cat(out, sep = "\n")
}

# One row of the cross-experiment summary: the effort slope of the judgment
# expected to fall with effort, the slope of the judgment expected to rise, and
# their difference, taken from an `emtrends()` frame and its `pairs()`; a study
# without a slope test passes no slopes and contributes its sample size only.
# `subset` names a part of a study's sample (a task, or a moderator level)
# reported beside its main result. Given the numeric-effort model, the row also
# carries each slope and the difference as a standardized effect, d: the change
# from low to high effort that the slope implies (2b), in units of the model's
# total rating SD (participant and residual variance), so that experiments with
# different rating scales compare on one metric. The interval scales the
# slope's CI and treats the SD as known.
key_result <- function(study, section, n, slopes = NULL, differences = NULL,
                       falls = NULL, rises = NULL, model = NULL, subset = NA_character_) {
  row <- tibble(study = study, subset = subset, section = section, n = n)
  if (is.null(slopes)) {
    return(row)
  }
  s <- as.data.frame(slopes)
  d <- as.data.frame(differences)
  trend <- grep("\\.trend$", names(s), value = TRUE)
  a <- s[s[[1]] == falls, ]
  b <- s[s[[1]] == rises, ]
  diff <- d[d$contrast == paste(rises, "-", falls), ]
  stopifnot(nrow(a) == 1, nrow(b) == 1, nrow(diff) == 1)
  row <- mutate(
    row,
    falls = falls, falls_b = a[[trend]], falls_lower = a$lower.CL, falls_upper = a$upper.CL,
    rises = rises, rises_b = b[[trend]], rises_lower = b$lower.CL, rises_upper = b$upper.CL,
    delta = diff$estimate, delta_lower = diff$lower.CL, delta_upper = diff$upper.CL,
    delta_df = diff$df, delta_t = diff$t.ratio, delta_p = diff$p.value
  )
  if (is.null(model)) {
    return(row)
  }
  sd_total <- sqrt(sum(as.data.frame(lme4::VarCorr(model$full_model))$vcov))
  row %>%
    mutate(
      sd_total = sd_total,
      across(c(falls_b:falls_upper, rises_b:rises_upper, delta:delta_upper),
             ~ 2 * .x / sd_total, .names = "d_{.col}")
    ) %>%
    rename_with(~ sub("_b$", "", .x), starts_with("d_"))
}

# Per-participant rating direction by group: the mean rating at each effort
# level classifies a participant as Increasing or Decreasing from Low to High;
# participants whose High and Low means tie drop out, so proportions are over
# the participants who moved. Descriptive classifier shared by every profile
# section.
derive_effort_profiles <- function(data, id = "id", groups = "condition",
                                   rating = "rating") {
  data %>%
    pivot_wider(
      id_cols = all_of(c(id, groups)),
      names_from = effort_level,
      values_from = all_of(rating),
      values_fn = mean
    ) %>%
    mutate(
      profile = case_when(
        High > Low ~ "Increasing",
        High < Low ~ "Decreasing",
        TRUE ~ "Flat"
      ),
      profile = factor(profile, levels = c("Decreasing", "Increasing"))
    ) %>%
    select(all_of(c(id, groups)), profile) %>%
    drop_na(profile)
}

# Profile counts and percentages per group, one row per group
summarize_effort_profiles <- function(profiles, groups = "condition") {
  profiles %>%
    count(across(all_of(c(groups, "profile")))) %>%
    group_by(across(all_of(groups))) %>%
    mutate(
      proportion = n / sum(n),
      display = paste0(round(proportion * 100, 1), "% (", n, ")")
    ) %>%
    ungroup() %>%
    select(all_of(c(groups, "profile")), display) %>%
    pivot_wider(names_from = profile, values_from = display)
}

# Profile fills: decreasing profiles open (white), increasing filled (teal),
# both outlined in teal
profile_fills <- c(Decreasing = "white", Increasing = evl_colours[["Teal"]])

# Cross each participant's liking profile with their price profile, for the
# design in which one participant supplies both judgments; a participant
# without both directions (High == Low on either scale) drops out
crossed_profile_counts <- function(profiles) {
  profiles %>%
    select(id, condition, profile) %>%
    pivot_wider(id_cols = id, names_from = condition, values_from = profile) %>%
    rename(Liking_Profile = Liking, Price_Profile = Value) %>%
    drop_na(Liking_Profile, Price_Profile) %>%
    mutate(
      Liking_Profile = forcats::fct_relevel(Liking_Profile, "Decreasing", "Increasing"),
      Price_Profile = forcats::fct_relevel(Price_Profile, "Increasing", "Decreasing")
    ) %>%
    xtabs(~ Price_Profile + Liking_Profile, data = .) %>%
    as.data.frame() %>%
    rename(count = Freq) %>%
    mutate(proportion = count / sum(count))
}

# Bar chart of the crossed profiles: liking profile on x, price profile as
# filled (increasing) or open (decreasing) bars, count and share as labels
plot_crossed_profiles <- function(counts) {
  dodge <- position_dodge2(width = 0.9, preserve = "single")
  ggplot(counts, aes(x = Liking_Profile, y = proportion, fill = Price_Profile)) +
    geom_col(position = dodge, colour = "black") +
    geom_text(
      aes(label = sprintf("%d\n(%.0f%%)", count, 100 * proportion)),
      position = dodge, vjust = -0.25, lineheight = 0.9, colour = "black"
    ) +
    scale_fill_manual(values = profile_fills, name = "Price Profile") +
    labs(x = "Liking Profile", y = "Proportion") +
    coord_cartesian(ylim = c(0, 0.5)) +
    theme_evl(legend = "bottom")
}

# Post-survey belief counts and proportions (one row per person), split by
# `group` where the belief items were answered per condition; NULL pools the
# sample
belief_summary <- function(data, id = "id", group = "condition") {
  data %>%
    group_by(.data[[id]]) %>%
    slice(1) %>%
    ungroup() %>%
    select(all_of(c(id, group)), belief_liking, belief_value) %>%
    pivot_longer(c(belief_liking, belief_value),
                 names_to = "belief", values_to = "value") %>%
    mutate(belief = str_to_title(str_remove(belief, "belief_"))) %>%
    count(across(all_of(group)), belief, value) %>%
    group_by(across(all_of(group)), belief) %>%
    mutate(prop = round(n / sum(n), 3)) %>%
    ungroup()
}

# Recognition-memory summary (d-prime is computed in data prep)
memory_summary <- function(data) {
  data %>%
    summarize(
      n = n(),
      mean_hits = mean(mem_hits), sd_hits = sd(mem_hits),
      mean_false_alarms = mean(mem_false_alarms), sd_false_alarms = sd(mem_false_alarms),
      mean_correct_rejections = mean(mem_correct_rejections),
      sd_correct_rejections = sd(mem_correct_rejections),
      mean_dprime = mean(dprime), sd_dprime = sd(dprime), min_dprime = min(dprime),
      mean_rt = mean(mem_rt), sd_rt = sd(mem_rt)
    )
}

# Task-load component ratings by effort level from a frame of means with
# lower.CL / upper.CL (an emmeans frame or a mean_cl_normal summary): the
# workload block (five panels) above the affective block (two panels), three
# panels per row, with one shared axis title on each side. The two blocks are
# stacked as gtables, returned as one, so that the panel columns align. The
# figure carries no title (the section heading names it).
plot_taskload_by_effort <- function(means, ylim = c(0, 21)) {
  teal <- evl_colours[["Teal"]]
  means <- tlx_factor(means)

  block_plot <- function(name) {
    means %>%
      filter(block == name) %>%
      ggplot(aes(x = effort_level, y = emmean, group = component)) +
      geom_errorbar(
        aes(ymin = lower.CL, ymax = upper.CL),
        width = 0.15, linewidth = 0.3, colour = teal
      ) +
      geom_line(colour = teal) +
      geom_point(colour = teal, shape = 19) +
      coord_cartesian(ylim = ylim) +
      ggh4x::facet_wrap2(
        ~component, ncol = 3, trim_blank = FALSE, axes = "all", remove_labels = "all"
      ) +
      labs(title = name, x = "Effort Level", y = "Mean Rating") +
      theme_evl() +
      theme(plot.title = element_text(size = 10, face = "bold", hjust = 0))
  }
  workload <- block_plot("Workload")
  affective <- block_plot("Affective")

  y_title <- cowplot::get_plot_component(workload, "ylab-l")
  x_title <- cowplot::get_plot_component(affective, "xlab-b")
  top <- ggplotGrob(workload + labs(x = NULL, y = NULL))
  bottom <- ggplotGrob(affective + labs(x = NULL, y = NULL))
  # ggh4x leaves out the axis columns of the empty third slot and leaves its
  # panel column unsized, so the affective block is given the panel size and
  # padded to the workload block's column structure
  empty <- grid::unitType(bottom$widths) == "null"
  bottom$widths[empty] <- top$widths[top$layout$l[top$layout$name == "panel-1"]]
  while (ncol(bottom) < ncol(top)) {
    i <- which(as.character(bottom$widths) != as.character(top$widths[seq_len(ncol(bottom))]))[1]
    if (is.na(i)) i <- ncol(bottom) + 1
    bottom <- gtable::gtable_add_cols(bottom, top$widths[i], pos = i - 1)
  }
  stopifnot(identical(as.character(bottom$widths), as.character(top$widths)))
  body <- rbind(top, bottom, size = "max")
  body <- gtable::gtable_add_cols(body, grid::unit(1, "grobwidth", y_title), pos = 0)
  body <- gtable::gtable_add_grob(body, y_title, t = 1, b = nrow(body), l = 1, name = "ylab")
  # The shared x title goes above the affective block's legend row
  guide_row <- max(body$layout$t[body$layout$name == "guide-box-bottom"])
  body <- gtable::gtable_add_rows(body, grid::unit(1, "grobheight", x_title), pos = guide_row - 1)
  body <- gtable::gtable_add_grob(
    body, x_title, t = guide_row, l = 2, r = ncol(body), name = "xlab"
  )
  # the plot margin the shared y title sits outside of
  gtable::gtable_add_cols(body, grid::unit(5, "pt"), pos = 0)
}
