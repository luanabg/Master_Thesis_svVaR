# example for y_2
#y_t <- y_list[[1]]

#maybe for simplicity, a more explicit y:
y_t <- c(0.05, 0.05)

df_X <- data.frame(x1 = xs[,1], x2 = xs[,2])
df_X_shifted <- data.frame(x1 = xs[,1] + y_t[1], x2 = xs[,2] + y_t[2])
df_points <- data.frame(
  x1 = c(y_t[1], -y_t[1]),
  x2 = c(y_t[2], -y_t[2]),
  label = c("y_t", "-y_t")
)

# Plot
ggplot() +
  # Original X points
  geom_point(data = df_X, aes(x = x1, y = x2), color = "grey60", alpha = 0.3, size = 0.7) +
  
  # y_t and -y_t points
  geom_point(data = df_points, aes(x = x1, y = x2, color = label), size = 3) +
  
  # Positive quadrant lines centered at y_t
  geom_segment(aes(x = y_t[1], y = y_t[2], 
                   xend = min(y_t[1] + 0.4, 0.5), 
                   yend = y_t[2]), 
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = y_t[1], y = y_t[2], 
                   xend = y_t[1], 
                   yend = min(y_t[2] + 0.4, 0.5)), 
               linetype = "dashed", color = "blue") +
  
  # Positive quadrant lines centered at -y_t
  #geom_segment(aes(x = -y_t[1], y = -y_t[2], xend = -y_t[1] + 3, yend = -y_t[2]), linetype = "dashed", color = "red") +
  #geom_segment(aes(x = -y_t[1], y = -y_t[2], xend = -y_t[1], yend = -y_t[2] + 3), linetype = "dashed", color = "red") +
  
  # Shifted X points: x_i + y_t
  geom_point(data = df_X_shifted, aes(x = x1, y = x2), color = "darkgreen", alpha = 0.3, size = 0.7) +
  
  scale_color_manual(values = c("y_t" = "blue", "-y_t" = "red")) +
  labs(title = "Multivariate Normal Draws and Set-Valued VaR Geometry",
       x = "x1", y = "x2") +
  coord_fixed() +
  xlim(-0.2, 0.5) +
  ylim(-0.2, 0.5) +
  theme_minimal()
