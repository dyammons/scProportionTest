permutation_coord_plot <- function(
	sc_utils_obj,
	FDR_threshold = 0.05,
	log2FD_threshold = log2(1.5),
	order_clusters = TRUE
) {

	## Retrieve results.
	plot_data <- copy(sc_utils_obj@results$permutation)

	## Mark the significant results.
	plot_data[, significance := ifelse(
		FDR < FDR_threshold & abs(obs_log2FD) > log2FD_threshold,
		paste("FDR <", FDR_threshold, "& abs(Log2FD) >", round(log2FD_threshold, 2)),
		"n.s."
	)]

	plot_data[, significance := factor(significance, levels = c(
		paste("FDR <", FDR_threshold, "& abs(Log2FD) >", round(log2FD_threshold, 2)),
		"n.s."
	))]

	## Order the clusters by observed log2FD if requested.
	if (order_clusters) {
		plot_data[, clusters := fct_reorder(factor(clusters), desc(obs_log2FD))]
	}
  ## Coordinate plot of results
  cxc <- ggplot(plot_data, aes(x = clusters, y = obs_log2FD)) +
  		geom_bar(width = 1, colour = "black")
		cxc + coord_polar()
		
	return(p)
}
