# Load necessary libraries
if (!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load(plotly, magrittr)

# Input (number of points from -3 to 3)
n <- 60

# Create a matrix of probabilities of joint standard normals
values <- seq(-3, 3, length.out = n)
sin_vector <- sin(values) + 1

joint_sins_matrix <- matrix(NA, n, n)
for (x in 1:nrow(joint_sins_matrix)) {
  for (y in 1:ncol(joint_sins_matrix)) {
    joint_sins_matrix[x,y] <- sin_vector[x] * sin_vector[y]
  }
}

joint_sins_matrix <- joint_sins_matrix / sum(joint_sins_matrix)

# Create a 3D-plot with plotly and highlight a contour with red
plotly::plot_ly(
    x = ~values,
    y = ~values,
    z = ~joint_sins_matrix,
    type = "surface",
    alpha = 0.6) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)", range = list(0, 9^-3)))) %>%
  plotly::hide_colorbar()
