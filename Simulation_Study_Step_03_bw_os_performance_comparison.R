############################
## Algorithm 6: Prediction of Optimal Bandwidth bw_os for Original Sample

stat_original_sample <- stat_para(list(original_sample))
Input <- stat_original_sample[,important_features, with = FALSE]
Input
bw_os <- predict(best_model, Input)[[1]]
bw_os

#######################################################
## Performance Comparison of Bandwidth Selection Methods
#######################################################

bw_ucv  <- kedd::h.ucv(original_sample,kernel=Ker)$h
bw_bcv  <- kedd::h.bcv(original_sample,kernel=Ker)$h
bw_ccv  <- kedd::h.ccv(original_sample,kernel=Ker)$h
bw_mcv  <- kedd::h.mcv(original_sample,kernel=Ker)$h
bw_tcv  <- kedd::h.tcv(original_sample,kernel=Ker)$h

comparison <- data.table(
  Method = c("UnML","UCV", "BCV", "CCV", "MCV","TCV"),
  Bandwidth = c(bw_os,bw_ucv, bw_bcv, bw_ccv, bw_mcv,bw_tcv)
)

calculate_mse <- function(bw, sample) {
  density_est <- kedd::dkde(x=sample, h = bw, kernel = Ker)
  true_density <- dweibull(density_est$eval.points, shape = 2, scale = 4)
  mse <- mean((density_est$est.fx - true_density)^2)
  return(mse)
}
mse_bw <- sapply(1:length(comparison$Bandwidth), function(i)
  calculate_mse(comparison$Bandwidth[i], original_sample))
performance_comparison <- data.table(
  Method = c("UnML","UCV", "BCV", "CCV", "MCV","TCV"),
  Bandwidth = c(bw_os,bw_ucv, bw_bcv, bw_ccv, bw_mcv,bw_tcv),
  MSE = mse_bw
)
performance_comparison