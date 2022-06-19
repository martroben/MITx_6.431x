
if (!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load(plotly, magrittr)

n <- 60
values <- seq(-3, 3, length.out = n)

joint_normals_matrix <- matrix(NA, n, n)
for (x in 1:nrow(joint_normals_matrix)) {
  random_variable_X_density <- dnorm(values[x])
  for (y in 1:ncol(joint_normals_matrix)) {
    random_variable_Y_density <- dnorm(values[x] + values[y])
    joint_normals_matrix[x,y] <- random_variable_X_density * random_variable_Y_density
  }
}

plotly::plot_ly(
    x = ~values,
    y = ~values,
    z = ~joint_normals_matrix,
    type = "surface",
    contours = list(
      z = list(
        show = TRUE,
        width = 1,
        color = "lightgray",
        start = 0,
        end = 0.2,
        size = 0.03))) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "P(X = x, Y = y)"))) %>%
  plotly::hide_colorbar()
