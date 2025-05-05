#' Reproduce Example 3.1
#'
#' Reproduces Example 3.1 from the book.  If you specify any options, your results may look different.
#'
#' @param N      Number of simulated realizations
#' @param n      Sample size
#' @param p      Number of features
#' @param p1     Number of non-null features
#' @param snr    Vector of signal-to-noise ratio values
#' @param seed   Seed for reproducibility
#'
#' @return A list of two arrays, `MCP`, containing the MSE for MCP, and `SCAD`, containing the MSE for SCAD.
#'
#' @examples
#' Ex3.1(N=2)
#' @export

Ex3.1 <- function(N = 500, n = 50, p = 100, p1 = 6, snr = c(1, 2, 4), seed = 2) {
  original_seed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- original_seed)
  set.seed(seed)

  gam <- 2^seq(0.5, 5, len=19)
  opt <- list(
    penalty = c('MCP', 'SCAD'),
    rep = 1:N,
    gam = gam,
    snr = snr)
  out <- expand.grid(opt, stringsAsFactors = FALSE) |> data.table::as.data.table()
  out$est_err <- NA

  pb <- progress::progress_bar$new(total = nrow(out))
  for (i in 1:nrow(out)) {
    o <- out[i]
    if (i == 1 || o$rep != out$rep[i-1]) {
      # Generate
      dat <- gen_data(n=n, p=p, p1=p1, SNR=o$snr)
      tst <- gen_data(n=n, p=p, p1=p1, SNR=o$snr)
    }

    # Analyze
    if (o$penalty == 'SCAD' & o$gam <= 2) next
    fit <- ncvreg(dat$X, dat$y, gamma=o$gam, penalty=o$penalty)
    prd <- predict(fit, tst$X)
    lam <- fit$lambda[which.min(apply(prd-tst$y, 2, crossprod))]

    # Summarize
    out$est_err[i] <- crossprod(dat$beta - coef(fit, lambda=lam)[-1])
    pb$tick()
  }
  out
}

#' Reproduce Figure 3.5
#'
#' Reproduces Figure 3.5 from the book; if you specify any options, your results may look different.
#'
#' @param out       Output of Ex3.1()
#'
#' @examples
#' out <- Ex3.1(N=3)
#' Fig3.5(out)
#' @export

Fig3.5 <- function(out) {
  SNR <- snr <- est_err <- NULL  # NSE Rcheck

  if (missing(out)) stop("You need to run the code in Ex3.1() first and pass it to Fig3.5()")
  out[!is.na(est_err)] |>
    _[, SNR := factor(snr)] |>
    ggplot2::ggplot(
      ggplot2::aes(.data$gam, .data$est_err, group = .data$SNR, color = .data$SNR)
    ) +
    ggplot2::geom_smooth(method = lm, formula = y ~ splines::ns(x, 3), se = FALSE) +
    ggplot2::facet_wrap(~ .data$penalty) +
    ggplot2::scale_x_log10(breaks = 2^(1:5)) +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::ylab('Mean squared error')
}
