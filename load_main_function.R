## Generation of Synthetic Samples

syn_samples <- function(original_sample, method, num_samples) {
  n <- length(original_sample)
  synthetic_samples <- list()
  if (method == 1) {
    for (i in 1:num_samples) {
      synthetic_samples[[i]] <- sample(original_sample, size = n, replace = TRUE)
    }
  } else if (method == 2) {
    for (i in 1:num_samples) {
      noise <- rnorm(n, mean = 0, sd = sd(original_sample)*0.05)
      synthetic_samples[[i]] <- original_sample + noise
    }
  } else {
    stop("Invalid method. Use 1 for Bootstrap Resampling or 2 for Perturbation.")
  }
  return(synthetic_samples)
}

## Function to calculate statistical parameters
stat_para <- function(synthetic_samples) {
  results <- list()
  for (i in seq_along(synthetic_samples)) {
    sample <- synthetic_samples[[i]]
    sample <- sample[!is.na(sample)]
    # Central Tendency Measures
    mean_val <- mean(sample)
    median_val <- median(sample)
    mode_val <- statip::mfv1(sample, na_rm =TRUE)
    geometric_mean_val <- (prod(sample))^(1/length(sample))
    harmonic_mean_val <- length(sample) / sum(1 / sample)
    # Dispersion Measures
    variance_val <- var(sample)
    sd_val <- sd(sample)
    iqr_val <- IQR(sample)
    range_val <- diff(range(sample))
    mad_val <- mean(abs(sample - mean_val))
    # Shape Measures
    skewness_val <- moments::skewness(sample)
    kurtosis_val <- moments::kurtosis(sample )
    moment_order_3_val <- mean((sample - mean_val)^3)
    moment_order_4_val <- mean((sample - mean_val)^4)
    moment_order_5_val <- mean((sample - mean_val)^5)
    # Position Measures
    min_val <- min(sample)
    max_val <- max(sample)
    quartiles_val_25 <- quantile(sample, probs = 0.25)[[1]]
    quartiles_val_75 <- quantile(sample, probs = 0.75)[[1]]
    cv_val <- sd_val / mean_val
    # Robustness Measures
    mad_robust_val <- median(abs(sample - median(sample)))
    trimean_val <- (quartiles_val_25 + 2 * median_val + quartiles_val_75) / 4
    trimmed_mean_val <- mean(sample, trim = 0.05)
    winsorized_mean_val <- mean(DescTools::Winsorize(sample,na.rm = TRUE))
    # Relative Variability Measures
    dispersion_coefficient_val <- iqr_val / median_val
    # Concentration Measures
    gini <- function(x, na.rm = TRUE) {
      if (na.rm) x <- x[!is.na(x)]
      n <- length(x)
      x <- sort(x)
      G <- sum((2 * (1:n) - n - 1) * x)
      return(G / (n * sum(x)))
    }
    gini_val <- gini(sample)
    # Entropy
    hist_sample <- hist(sample, breaks = "Scott", plot = FALSE)
    entry_val <- entropy::entropy(hist_sample$counts, method="ML")
    # Store the results in a list
    results[[i]] <- data.frame(
      mean = mean_val,
      median = median_val,
      mode = mode_val,
      #geom = geometric_mean_val,
      harm = harmonic_mean_val,
      var = variance_val,
      sd = sd_val,
      IQR = iqr_val,
      range = range_val,
      mean_abs = mad_val,
      skewness = skewness_val,
      kurtosis = kurtosis_val,
      mom_3 = moment_order_3_val,
      mom_4 = moment_order_4_val,
      mom_5 = moment_order_5_val,
      min = min_val,
      max = max_val,
      Q_25 = quartiles_val_25,
      Q_75 = quartiles_val_75,
      cv = cv_val,
      med_abs = mad_robust_val,
      trimean = trimean_val,
      trimmed = trimmed_mean_val,
      winsorized = winsorized_mean_val,
      disp_coeff = dispersion_coefficient_val,
      gini = gini_val,
      entropy = entry_val
    )
  }
  results_df <- dplyr::bind_rows(results)
  results_df <- data.table::data.table(results_df)
  return(results_df)
}

## function to perform rule of thumb 'Scott (1992)' for bandwidth selection

MISE_bw <- function(x,kernel)
{
  KER <- c("gaussian","epanechnikov","triangular","biweight","uniform",
           "triweight","tricube","cosine")
  if (missing(kernel)) {
    kernel <- "gaussian"
  }
  if (!kernel %in% KER) {
    stop("Unsupported kernel type, see help(h.ucv).")
  }
  if (!is.numeric(x) || length(dim(x)) >=1 || length(x) < 2L) 
    stop("argument 'x' must be numeric and need at least 2 data points.") 
  mu_K <-function(kernel)
  {
    if (kernel=="gaussian")          {xKr <- 1}
    else if (kernel=="epanechnikov") {xKr <- 1/5 }
    else if (kernel=="uniform")      {xKr <- 1/3 }
    else if (kernel=="triangular")   {xKr <- 1/6 }
    else if (kernel=="triweight")    {xKr <- 1/9}
    else if (kernel=="tricube")      {xKr <- 35/243}
    else if (kernel=="biweight")     {xKr <- 1/7}
    else if (kernel=="cosine")       {xKr <- (-8+pi^2)/pi^2}
    return(xKr)
  }
  R_K <-function(kernel)
  {
    if (kernel=="gaussian")          {xKr <- 1/(2*sqrt(pi))}
    else if (kernel=="epanechnikov") {xKr <- 3/5}
    else if (kernel=="uniform")      {xKr <- 1/2}
    else if (kernel=="triangular")   {xKr <- 2/3}
    else if (kernel=="triweight")    {xKr <- 350/429}
    else if (kernel=="tricube")      {xKr <- 175/247}
    else if (kernel=="biweight")     {xKr <- 5/7}
    else if (kernel=="cosine")       {xKr <- (1/16)*pi^2}
    return(xKr)
  }
  n <- length(x)
  Rff <- 3/(8*sqrt(pi)*sd(x)^5)
  r <- quantile(x, c(0.25, 0.75))
  h <- ((8*sqrt(pi)*R_K(kernel))/(3*mu_K(kernel)^2))^(1/5) * min(sd(x),(r[2L] - r[1L])/1.34) * n^(-1/5)
  #MISE_h <- R_K(kernel)/(n*h) + 0.25 * h^4 * Rff * mu_K(kernel)^2
  return(h)
}
