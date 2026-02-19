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
#' @export
plot_icc <- function(object, items = NULL, theta_range = c(-4, 4),
                     ci = TRUE, ...) {

  assert_birt_fit(object)
  model <- get_model_name(object)

  K <- object$K
  if (is.null(items)) items <- seq_len(K)

  b_draws <- posterior::as_draws_matrix(object$fit$draws("beta"))
  b_mean  <- apply(b_draws, 2, mean)
  b_lower <- apply(b_draws, 2, stats::quantile, probs = 0.025)
  b_upper <- apply(b_draws, 2, stats::quantile, probs = 0.975)

  # Get discrimination if 2PL or 3PL
  if (model %in% c("2PL", "3PL")) {
    a_draws <- posterior::as_draws_matrix(object$fit$draws("a"))
    a_mean  <- apply(a_draws, 2, mean)
  } else {
    a_mean <- rep(1, K)  # Rasch: all discriminations = 1
  }

  # Get guessing if 3PL
  if (model == "3PL") {
    c_draws <- posterior::as_draws_matrix(object$fit$draws("c"))
    c_mean  <- apply(c_draws, 2, mean)
  } else {
    c_mean <- rep(0, K)  # No guessing
  }

  theta_seq <- seq(theta_range[1], theta_range[2], length.out = 200)

  plot_data <- do.call(rbind, lapply(items, function(i) {
    p_mean <- c_mean[i] + (1 - c_mean[i]) * stats::plogis(a_mean[i] * (theta_seq - b_mean[i]))

    # Simplified CI (based on difficulty uncertainty only)
    p_lower <- c_mean[i] + (1 - c_mean[i]) * stats::plogis(a_mean[i] * (theta_seq - b_upper[i]))
    p_upper <- c_mean[i] + (1 - c_mean[i]) * stats::plogis(a_mean[i] * (theta_seq - b_lower[i]))

    data.frame(
      theta = theta_seq, p = p_mean,
      p_lower = p_lower, p_upper = p_upper,
      item = object$item_names[i],
      stringsAsFactors = FALSE
    )
  }))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$theta, y = .data$p, color = .data$item
  )) +
    ggplot2::geom_line(linewidth = 0.8)

  if (ci) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$p_lower, ymax = .data$p_upper, fill = .data$item),
      alpha = 0.15, colour = NA
    )
  }

  p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.4) +
    ggplot2::labs(
      x = "Person Ability (logits)", y = "P(Correct)",
      title = paste(model, "Item Characteristic Curves"),
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
  assert_birt_fit(object)

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
#' @export
plot_info <- function(object, items = NULL, theta_range = c(-4, 4),
                      show_items = FALSE, ...) {

  assert_birt_fit(object)
  model <- get_model_name(object)

  K <- object$K
  if (is.null(items)) items <- seq_len(K)

  b_draws <- posterior::as_draws_matrix(object$fit$draws("beta"))
  b_mean  <- apply(b_draws, 2, mean)

  if (model %in% c("2PL", "3PL")) {
    a_draws <- posterior::as_draws_matrix(object$fit$draws("a"))
    a_mean  <- apply(a_draws, 2, mean)
  } else {
    a_mean <- rep(1, K)
  }

  if (model == "3PL") {
    c_draws <- posterior::as_draws_matrix(object$fit$draws("c"))
    c_mean  <- apply(c_draws, 2, mean)
  } else {
    c_mean <- rep(0, K)
  }

  theta_seq <- seq(theta_range[1], theta_range[2], length.out = 200)

  # Item information formula:
  # Rasch: I = P * Q
  # 2PL:   I = a^2 * P * Q
  # 3PL:   I = a^2 * (P - c)^2 / ((1 - c)^2 * P * Q)
  info_matrix <- sapply(items, function(i) {
    p <- c_mean[i] + (1 - c_mean[i]) * stats::plogis(a_mean[i] * (theta_seq - b_mean[i]))
    q <- 1 - p

    if (model == "3PL") {
      a_mean[i]^2 * ((p - c_mean[i])^2) / ((1 - c_mean[i])^2 * p * q)
    } else {
      a_mean[i]^2 * p * q
    }
  })

  test_info <- rowSums(info_matrix)
  plot_df <- data.frame(theta = theta_seq, info = test_info)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = .data$theta, y = .data$info
  )) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2C3E50")

  if (show_items) {
    item_info_df <- do.call(rbind, lapply(seq_along(items), function(j) {
      data.frame(
        theta = theta_seq, info = info_matrix[, j],
        item = object$item_names[items[j]],
        stringsAsFactors = FALSE
      )
    }))

    p <- p + ggplot2::geom_line(
      data = item_info_df,
      ggplot2::aes(x = .data$theta, y = .data$info, color = .data$item),
      linewidth = 0.5, alpha = 0.7
    ) + ggplot2::labs(color = "Item")
  }

  p + ggplot2::labs(
    x = "Person Ability (logits)", y = "Information",
    title = paste(model, "Test Information Function")
  ) + ggplot2::theme_minimal(base_size = 12)
}
