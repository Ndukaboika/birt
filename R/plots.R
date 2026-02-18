#' Plot Item Characteristic Curves (ICCs)
#'
#' An ICC shows the probability of getting an item correct
#' as a function of ability. Each item gets one curve.
#' Harder items have curves shifted to the right.
#'
#' @param object A birt_fit object.
#' @param items Which items to plot (integer vector). Default: all.
#' @param theta_range Range of ability values to show. Default c(-4, 4).
#' @param ci Show 95% credible bands? Default TRUE.
#' @param ... Ignored.
#'
#' @return A ggplot2 object.
#' @export
plot_icc <- function(object, items = NULL, theta_range = c(-4, 4),
                     ci = TRUE, ...) {
  checkmate::assert_class(object, "birt_fit")

  K <- object$K
  if (is.null(items)) items <- seq_len(K)

  # Get posterior draws for beta
  b_draws <- posterior::as_draws_matrix(object$fit$draws("beta"))

  # Posterior mean and 95% interval for each item
  b_mean <- apply(b_draws, 2, mean)
  b_lower <- apply(b_draws, 2, stats::quantile, probs = 0.025)
  b_upper <- apply(b_draws, 2, stats::quantile, probs = 0.975)

  # Fine grid of ability values for smooth curves
  theta_seq <- seq(theta_range[1], theta_range[2], length.out = 200)

  # Build plot data: one row per (theta, item) combination
  plot_data <- do.call(rbind, lapply(items, function(i) {
    # ICC: P(correct) = plogis(ability - difficulty)
    p_mean <- stats::plogis(theta_seq - b_mean[i])
    # Note the "flip": higher beta = lower P, so bounds swap
    p_lower <- stats::plogis(theta_seq - b_upper[i])
    p_upper <- stats::plogis(theta_seq - b_lower[i])

    data.frame(
      theta = theta_seq,
      p = p_mean,
      p_lower = p_lower,
      p_upper = p_upper,
      item = object$item_names[i],
      stringsAsFactors = FALSE
    )
  }))

  # Build the ggplot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$theta, y = .data$p, color = .data$item
  )) +
    ggplot2::geom_line(linewidth = 0.8)

  if (ci) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$p_lower, ymax = .data$p_upper, fill = .data$item
      ),
      alpha = 0.15, colour = NA
    )
  }

  p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.4) +
    ggplot2::labs(
      x = "Person Ability (logits)",
      y = "P(Correct)",
      title = "Item Characteristic Curves",
      color = "Item", fill = "Item"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::ylim(0, 1)
}


#' Wright Map (Item-Person Map)
#'
#' Shows person abilities (histogram) and item difficulties (triangles)
#' on the same logit scale. Lets you see if items cover the ability range.
#'
#' @param object A birt_fit object.
#' @param ... Ignored.
#'
#' @return A ggplot2 object.
#' @export
plot_wright_map <- function(object, ...) {
  checkmate::assert_class(object, "birt_fit")

  # Person total abilities (alpha + delta)
  persons <- person_params(object)
  theta_mean <- persons$mean

  # Item difficulties (posterior means)
  items <- item_params(object)
  b_mean <- items$mean

  person_df <- data.frame(value = theta_mean)
  item_df <- data.frame(
    value = b_mean,
    label = object$item_names,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    # Blue histogram of person abilities
    ggplot2::geom_histogram(
      data = person_df,
      ggplot2::aes(x = .data$value),
      binwidth = 0.3, fill = "#4A90D9", alpha = 0.7, color = "white"
    ) +
    # Red triangles for item difficulties
    ggplot2::geom_point(
      data = item_df,
      ggplot2::aes(x = .data$value, y = -0.5),
      shape = 17, size = 3, color = "#D94A4A"
    ) +
    # Item labels
    ggplot2::geom_text(
      data = item_df,
      ggplot2::aes(x = .data$value, y = -1.2, label = .data$label),
      angle = 45, hjust = 1, size = 3, color = "#D94A4A"
    ) +
    ggplot2::labs(
      x = "Logits",
      y = "Count (Persons)",
      title = "Wright Map",
      subtitle = "Blue = person abilities | Red = item difficulties"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}


#' Test Information Function
#'
#' Shows where on the ability scale the test is most precise.
#' In Rasch, item info = P(1-P), which peaks at ability = difficulty.
#' Test info = sum of all item infos.
#'
#' @param object A birt_fit object.
#' @param items Which items to include. Default: all.
#' @param theta_range Range of ability values. Default c(-4, 4).
#' @param show_items Show individual item curves? Default FALSE.
#' @param ... Ignored.
#'
#' @return A ggplot2 object.
#' @export
plot_info <- function(object, items = NULL, theta_range = c(-4, 4),
                      show_items = FALSE, ...) {
  checkmate::assert_class(object, "birt_fit")

  K <- object$K
  if (is.null(items)) items <- seq_len(K)

  b_draws <- posterior::as_draws_matrix(object$fit$draws("beta"))
  b_mean <- apply(b_draws, 2, mean)

  theta_seq <- seq(theta_range[1], theta_range[2], length.out = 200)

  # Item information: I(theta) = P(theta) * (1 - P(theta))
  info_matrix <- sapply(items, function(i) {
    p <- stats::plogis(theta_seq - b_mean[i])
    p * (1 - p)
  })

  # Test information = sum across items
  test_info <- rowSums(info_matrix)
  plot_df <- data.frame(theta = theta_seq, info = test_info)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = .data$theta, y = .data$info
  )) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2C3E50")

  if (show_items) {
    item_info_df <- do.call(rbind, lapply(seq_along(items), function(j) {
      data.frame(
        theta = theta_seq,
        info = info_matrix[, j],
        item = object$item_names[items[j]],
        stringsAsFactors = FALSE
      )
    }))

    p <- p + ggplot2::geom_line(
      data = item_info_df,
      ggplot2::aes(
        x = .data$theta, y = .data$info, color = .data$item
      ),
      linewidth = 0.5, alpha = 0.7
    ) +
      ggplot2::labs(color = "Item")
  }

  p + ggplot2::labs(
    x = "Person Ability (logits)",
    y = "Information",
    title = "Test Information Function",
    subtitle = "Higher = more precise measurement"
  ) +
    ggplot2::theme_minimal(base_size = 12)
}
