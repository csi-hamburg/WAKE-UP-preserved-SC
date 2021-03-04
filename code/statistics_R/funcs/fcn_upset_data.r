upset_data <- function (data, intersect, min_size = 0, max_size = Inf, min_degree = 0, 
                        max_degree = Inf, keep_empty_groups = FALSE, warn_when_dropping_groups = TRUE, 
                        sort_sets = "descending", sort_intersections = "descending", 
                        sort_intersections_by = "cardinality", min_max_early = TRUE, 
                        union_count_column = "union_size", intersection_count_column = "intersection_size") 
{
  if ("tbl" %in% class(data)) {
    data = as.data.frame(data)
  }
  ComplexUpset:::check_sort(sort_sets)
  ComplexUpset:::check_sort(sort_intersections)
  ComplexUpset:::check_sort(sort_intersections_by, allowed = c("cardinality", "degree", "ratio"), what = "method")
  intersect = unlist(intersect)
  if (length(intersect) == 1) {
    stop("Needs at least two indicator variables")
  }
  is_column_logical = sapply(data[, intersect], is.logical)
  if (any(!is_column_logical)) {
    non_logical = names(is_column_logical[is_column_logical == FALSE])
    print(paste("Converting non-logical columns to binary:", 
                paste(non_logical, collapse = ", ")))
    data[, non_logical] = sapply(data[, non_logical], as.logical)
  }
  intersect_in_order_of_data = colnames(data)[colnames(data) %in% intersect]
  colnames(data)[colnames(data) %in% intersect] <- ComplexUpset:::sanitize_names(intersect_in_order_of_data)
  non_sanitized_labels = intersect
  intersect = ComplexUpset:::sanitize_names(intersect)
  names(non_sanitized_labels) = intersect
  
  
  data$intersection = apply(data[intersect], 1, ComplexUpset:::names_of_members)
  data$intersection[data$intersection == ""] = "NOT_IN_EITHER_GROUP"
  intersections_by_size = table(data$intersection)
  plot_intersections_subset = names(intersections_by_size)
  plot_sets_subset = intersect
  
  
  if (min_size > 0 || max_size != Inf || min_degree > 0 || max_degree != Inf) {
    intersections_by_size_trimmed = ComplexUpset:::trim_intersections(intersections_by_size, 
                                                       min_size = min_size, max_size = max_size, min_degree = min_degree, 
                                                       max_degree = max_degree)
    data_subset = data[data$intersection %in% names(intersections_by_size_trimmed), 
                       ]
    if (!keep_empty_groups) {
      itersect_data = data_subset[, intersect]
      is_non_empty = sapply(itersect_data, any)
      empty_groups = names(itersect_data[!is_non_empty])
      if (length(empty_groups) != 0 && warn_when_dropping_groups) {
        to_display = ifelse(length(empty_groups) <= 5, 
                            paste("Dropping empty groups:", paste(empty_groups, 
                                                                  collapse = ", ")), paste("Dropping", length(empty_groups), 
                                                                                           "empty groups"))
        print(to_display)
      }
      intersect_subset = intersect[!(intersect %in% empty_groups)]
    }
    else {
      intersect_subset = intersect
    }
    if (min_max_early == TRUE) {
      intersections_by_size = intersections_by_size_trimmed
      if (!keep_empty_groups) {
        intersect = intersect_subset
        data = data_subset
      }
    }
    plot_intersections_subset = names(intersections_by_size_trimmed)
    plot_sets_subset = intersect_subset
  }
  
  
  
  stacked = stack(data, intersect)
  stacked$id = rep(1:nrow(data), length(intersect))
  stacked = stacked[stacked$values == TRUE, ]
  stacked$group = stacked$ind
  groups_by_size = table(stacked$group)
  if (sort_sets != FALSE) {
    groups_by_size = groups_by_size[ComplexUpset:::get_sort_order(groups_by_size, sort_sets)]
  }
  else {
    groups_by_size = groups_by_size[names(groups_by_size)]
  }
  sorted_groups = names(groups_by_size)
  if (sort_intersections != FALSE) {
    if (sort_intersections_by == "cardinality") {
      sort_value = intersections_by_size
    }
    else if (sort_intersections_by == "degree") {
      original_intersections_names = names(intersections_by_size)
      sort_value = ComplexUpset:::count_occurrences(original_intersections_names, "-")
      names(sort_value) = original_intersections_names
    }
    else if (sort_intersections_by == "ratio") {
      unsorted_union_sizes = ComplexUpset:::compute_unions(stacked, names(intersections_by_size))
      sort_value = intersections_by_size
      sort_value = sort_value/unsorted_union_sizes
    }
    intersections_by_size = intersections_by_size[ComplexUpset:::get_sort_order(sort_value, sort_intersections)]
  }
  
  
  
  sorted_intersections = names(intersections_by_size)
  matrix_data = ComplexUpset:::compute_matrix(sorted_intersections, sorted_groups)
  group = rownames(matrix_data)
  
  
  matrix_frame = ComplexUpset:::gather(cbind(group, matrix_data), "group", "intersection", 'value')
  

  union_sizes = ComplexUpset:::compute_unions(stacked, sorted_intersections)
  with_sizes = data.frame(data)
  
  
  with_sizes[[union_count_column]] = sapply(data$intersection, function(intersection) {
                                              union_sizes[intersection]
                                            })
  
  with_sizes[[intersection_count_column]] = sapply(data$intersection, function(intersection) {
                                                     intersections_by_size[intersection]
                                                   })
  
  
  list(with_sizes = with_sizes, sets_ordering_in_ids = intersect, 
       intersected = data, presence = stacked, matrix = matrix_data, 
       matrix_frame = matrix_frame, sorted = list(groups = sorted_groups, 
                                                  intersections = with_sizes %>% dplyr::select(est, intersection) %>% distinct() %>% arrange(est) %>% pull("intersection")), union_sizes = union_sizes, 
       plot_intersections_subset = plot_intersections_subset, 
       plot_sets_subset = plot_sets_subset, non_sanitized_labels = non_sanitized_labels)
}


upset <- function (data, intersect, base_annotations = list(`Intersection size` = intersection_size(counts = TRUE)), 
                   name = "group", annotations = list(), themes = upset_themes, 
                   stripes = upset_stripes, labeller = identity, height_ratio = 0.5, 
                   width_ratio = 0.3, wrap = FALSE, set_sizes = upset_set_size(width = 0.6), 
                   queries = list(), dot_size = 3, ...) 
{
  
  
  annotations = c(annotations, base_annotations)
  data = upset_data(data, intersect, ...)
  
  intersections_sorted = rev(data$sorted$intersections)
  intersections_limits = intersections_sorted[intersections_sorted %in% data$plot_intersections_subset]
  scale_intersections = scale_x_discrete(limits = intersections_limits, labels = 1:length(intersections_sorted))
  sets_limits = data$sorted$groups[data$sorted$groups %in% data$plot_sets_subset]
  show_overall_sizes = !(inherits(set_sizes, "logical") && set_sizes == FALSE)
  matrix_intersect_queries = ComplexUpset:::intersect_queries(ComplexUpset:::queries_for(queries, "intersections_matrix"), data)
  
  
  
  query_matrix = ComplexUpset:::get_highlights_data(data$matrix_frame, "intersection", matrix_intersect_queries)
  

  query_matrix = query_matrix[query_matrix$value == TRUE, ]
  

  intersections_matrix = (ggplot(data$matrix_frame, aes(x = intersection, y = group)) + ComplexUpset:::matrix_background_stripes(data, stripes) + 
                            geom_point(color = ifelse(data$matrix_frame$value, "black", 
                                                      "grey70"), size = dot_size * 7/6) + geom_point(aes(color = value), 
                                                                                                     size = dot_size) + ComplexUpset:::highlight_layer(geom_point, query_matrix, 
                                                                                                                                        args = list(size = dot_size)) + geom_segment(aes(x = intersection, 
                                                                                                                                                                                         xend = intersection, y = ComplexUpset:::segment_end(data$matrix_frame, 
                                                                                                                                                                                                                              data, intersection, head), yend = ComplexUpset:::segment_end(data$matrix_frame, 
                                                                                                                                                                                                                                                                            data, intersection, tail)), na.rm = TRUE) + ComplexUpset:::highlight_layer(geom_segment, 
                                                                                                                                                                                                                                                                                                                                     query_matrix, args = list(aes(x = intersection, xend = intersection, 
                                                                                                                                                                                                                                                                                                                                                                   y = ComplexUpset:::segment_end(query_matrix, data, intersection, 
                                                                                                                                                                                                                                                                                                                                                                                   head), yend = ComplexUpset:::segment_end(query_matrix, data, 
                                                                                                                                                                                                                                                                                                                                                                                                             intersection, tail)), na.rm = TRUE)) + xlab(name) + 
                            scale_y_discrete(limits = sets_limits, labels = function(sets) {
                              labeller(data$non_sanitized_labels[sets])
                            }, expand = expansion(mult = 0, add = c(0.1, 0.1))) + scale_intersections + themes$intersections_matrix)
  rows = list()
  for (name in names(annotations)) {
    annotation = annotations[[name]]
    geoms = annotation$geom
    if (!inherits(geoms, "list")) {
      geoms = list(geoms)
    }
    annotation_queries = intersect_queries(ComplexUpset:::queries_for(queries, 
                                                       name), data)
    if (nrow(annotation_queries) != 0) {
      highlight_data = merge(data$with_sizes, annotation_queries, 
                             by.x = "intersection", by.y = "intersect", all.y = TRUE)
      if (is.null(annotation$highlight_geom)) {
        highlight_geom = geoms
      }
      else {
        highlight_geom = annotation$highlight_geom
        if (!inherits(highlight_geom, "list")) {
          highlight_geom = list(highlight_geom)
        }
      }
      geoms_plus_highlights = add_highlights_to_geoms(geoms, 
                                                      highlight_geom, highlight_data, annotation_queries)
    }
    else {
      geoms_plus_highlights = geoms
    }
    if (name %in% names(themes)) {
      theme = themes[[name]]
    }
    else {
      theme = themes[["default"]]
    }
    if (show_overall_sizes) {
      rows[[length(rows) + 1]] = plot_spacer()
    }
    rows[[length(rows) + 1]] = (ggplot(data$with_sizes, annotation$aes) + 
                                  geoms_plus_highlights + scale_intersections + xlab(name) + 
                                  ylab(name) + theme)
  }
  if (show_overall_sizes) {
    overall_sizes_queries = set_queries(ComplexUpset:::queries_for(queries, 
                                                    "overall_sizes"))
    overall_sizes_highlights_data = get_highlights_data(data$presence, 
                                                        "group", overall_sizes_queries)
    overall_sizes = (ggplot(data$presence, aes(x = group)) + 
                       ComplexUpset:::matrix_background_stripes(data, stripes, "vertical") + 
                       coord_flip() + eval_if_needed(set_sizes, overall_sizes_highlights_data = overall_sizes_highlights_data) + 
                       scale_x_discrete(limits = sets_limits) + themes$overall_sizes)
    matrix_row = list(overall_sizes, intersections_matrix)
  }
  else {
    matrix_row = list(intersections_matrix)
  }
  if (length(rows)) {
    annotations_plots = Reduce(f = "+", rows)
    matrix_row = c(list(annotations_plots), matrix_row)
  }
  else {
    annotations_plots = list()
  }
  plot = Reduce(f = "+", matrix_row)
  if (show_overall_sizes) {
    width_ratios = c(width_ratio, 1 - width_ratio)
  }
  else {
    width_ratios = 1
  }
  plot = plot + patchwork::plot_layout(widths = width_ratios, ncol = 1 + 
                              ifelse(show_overall_sizes, 1, 0), nrow = length(annotations) + 
                              1, heights = c(rep(1, length(annotations)), height_ratio))
  if (wrap) {
    wrap_elements(plot)
  }
  else {
    plot
  }
}

