# Load necessary libraries
if (!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load(plotly, magrittr)

# Input (number of points from -3 to 3)
n <- 60

# Create a matrix of probabilities of joint standard normals
values <- seq(-3, 3, length.out = n)
normal_vector <- dnorm(values)
sin_vector <- sin(values) + 1

joint_matrix <- matrix(NA, n, n)
for (x in 1:nrow(joint_matrix)) {
  for (y in 1:ncol(joint_matrix)) {
    joint_matrix[x,y] <- normal_vector[x] * sin_vector[y]
  }
}

joint_matrix <- joint_matrix / sum(joint_matrix)

# Create a 3D-plot with plotly and highlight a contour with red
plotly::plot_ly(
    x = ~values,
    y = ~values,
    z = ~joint_matrix,
    type = "surface",
    contours = list(
      x = list(
        show = TRUE,
        start = 0.5,
        end = 1.5,
        size = 1,
        color = "red"))) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "P(X = x, Y = y)", range = list(0, 1.3)))) %>%
  plotly::hide_colorbar()
