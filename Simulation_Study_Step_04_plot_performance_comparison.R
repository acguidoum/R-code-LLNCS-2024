
# plot performance comparison with different kernel 

data <- data.frame(
  Kernel = rep(c("Gaussian", "Triweight"), each = 7),
  Resampling = c("Bootstrap", "Perturbation", rep("", 5), "Bootstrap", "Perturbation", rep("", 5)),
  Model = c("avNNet", "GBM", rep("", 5), "avNNet", "GBM", rep("", 5)),
  Method = c("bw_os(B)", "bw_os(P)", "UCV", "BCV", "CCV", "MCV", "TCV",
             "bw_os(B)", "bw_os(P)", "UCV", "BCV", "CCV", "MCV", "TCV"),
  Bandwidth = c(0.55773, 0.55215, 0.64228, 0.59398, 0.57901, 0.67707, 0.70682,
                1.49692, 1.64482, 1.89557, 1.74767, 0.70188, 2.29543, 2.06435),
  MSE = c(9.8666e-05, 9.8423e-05, 1.0519e-04, 1.0103e-04, 9.9927e-05, 1.0898e-04, 1.1275e-04,
          6.3158e-05, 6.2940e-05, 6.4856e-05, 6.3451e-05, 1.0714e-04, 7.2597e-05, 6.7452e-05)
)
data$Method <- as.character(data$Method)
data$Method <- ifelse(data$Method == "bw_os(B)", "bw[os](B)",
                      ifelse(data$Method == "bw_os(P)", "bw[os](P)", data$Method))
data_gaussian <- subset(data, Kernel == "Gaussian") %>%
  arrange(MSE)
data_triweight <- subset(data, Kernel == "Triweight") %>%
  arrange(MSE)
plot_gaussian <- ggplot(data_gaussian, aes(x = reorder(Method, MSE), y = Bandwidth)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  geom_segment(aes(x = reorder(Method, MSE), xend = reorder(Method, MSE), y = 0, yend = Bandwidth), color = "steelblue", size=3) +
  geom_point(size = 4, color = "steelblue") +
  geom_text(aes(label = sprintf("MSE = %.2e", MSE)),angle = 10, vjust = -1, hjust = 0.5, color = "black", size = 5,fontface = "bold") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Method",
       y = "Bandwidth") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14,angle = 0, hjust = 0.5,vjust= 5,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"))

plot_triweight <- ggplot(data_triweight, aes(x = reorder(Method, MSE), y = Bandwidth)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  geom_segment(aes(x = reorder(Method, MSE), xend = reorder(Method, MSE), y = 0, yend = Bandwidth), color = "steelblue", size=3) +
  geom_point(size = 4, color = "steelblue") +
  geom_text(aes(label = sprintf("MSE = %.2e", MSE)),angle = 10, vjust = -1, hjust = 0.5, color = "black", size = 5,fontface = "bold") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Method",
       y = "Bandwidth") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14,angle = 0, hjust = 0.5,vjust= 5,face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"))

plot_gaussian
plot_triweight