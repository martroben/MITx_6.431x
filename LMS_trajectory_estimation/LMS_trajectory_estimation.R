# load packages
rm(list = ls(all.names = TRUE))

if (!require(pacman, quietly = TRUE)) {install.packages(pacman)}
pacman::p_load(
  tibble,
  magrittr,
  dplyr,
  ggplot2)



####################
# custom functions #
####################

x_position <- function(t, theta_0, theta_1, theta_2) {
  # Calculates x_position of the thrown object at time t
  return(theta_0 + theta_1 * t + theta_2 * t^2)
}


estimate_theta_0 <- function(t, x, theta_2) {
  # Use the expression from system of linear equations for theta_0 (with theta_1 replaced)
  A <- (1 + sum(t^2)) * (200 + sum(x) + theta_2 * sum(t^2))
  B <- sum(t) * (50 + sum(x * t) + theta_2 * sum(t^3))
  C <- (1 + length(t)) * (1 + sum(t^2)) - sum(t)^2
  theta_0 <- (A - B) / C
}


estimate_theta_1 <- function(t, x, theta_0, theta_2) {
  # Use the expression from system of linear equations for theta_1
  theta_1 <- (50 + sum(x*t) + theta_2 * sum(t^3) - theta_0 * sum(t)) / (1 + sum(t^2))
  return (theta_1)
}



#################################
# true values of the parameters #
#################################

true_theta_0 <- 200
true_theta_1 <- 40
true_theta_2 <- -9.81



#########################################################################
# estimation in case of measurements spread over a narrow time interval #
#########################################################################

# simulate observations
# setting seed to get reproducible random numbers
set.seed(0)
t_narrow <- 1:10/10
x_narrow <- x_position(t_narrow, true_theta_0, true_theta_1, true_theta_2) + rnorm(n = 10, mean = 0, sd = 50)

# estimate for thetas
estimated_theta_0_narrow <- estimate_theta_0(t_narrow, x_narrow, true_theta_2)
estimated_theta_1_narrow <- estimate_theta_1(t_narrow, x_narrow, estimated_theta_0_narrow, true_theta_2)

# check if the derivative is 0 at calculated theta values
estimated_theta_0_narrow - 200 - sum(x_narrow - estimated_theta_0_narrow - estimated_theta_1_narrow * t_narrow + true_theta_2 * t_narrow^2)
estimated_theta_1_narrow - 50 - sum(t_narrow * (x_narrow - estimated_theta_0_narrow - estimated_theta_1_narrow * t_narrow + true_theta_2 * t_narrow^2))

# comparison fit from glm function
glm_result_narrow <- glm(x_narrow ~ t_narrow + offset(true_theta_2 * t_narrow^2))



#######################################################################
# estimation in case of measurements spread over a wide time interval #
#######################################################################

set.seed(1)
t_wide <- c(seq(0.1, 1, by = 0.2), seq(9.1, 10, by = 0.2))
x_wide <- x_position(t_wide, true_theta_0, true_theta_1, true_theta_2) + rnorm(n = 10, mean = 0, sd = 50)

estimated_theta_0_wide <- estimate_theta_0(t_wide, x_wide, true_theta_2)
estimated_theta_1_wide <- estimate_theta_1(t_wide, x_wide, estimated_theta_0_wide, true_theta_2)

# check if the derivative is 0 at calculated theta values
estimated_theta_0_wide - 200 - sum(x_wide - estimated_theta_0_wide - estimated_theta_1_wide * t_wide + true_theta_2 * t_wide^2)
estimated_theta_1_wide - 50 - sum(t_wide * (x_wide - estimated_theta_0_wide - estimated_theta_1_wide * t_wide + true_theta_2 * t_wide^2))

# fit from glm function
glm_result_wide <- glm(x_wide ~ t_wide + offset(true_theta_2 * t_wide^2))



################
# plot results #
################

narrow_data <- tibble::tibble(x = x_narrow, t = t_narrow, type = "narrow data")
wide_data <- tibble::tibble(x = x_wide, t = t_wide, type = "wide data")

# dummy variables to create a legend like I want (got to love ggplot...)
legend <- tibble::tibble(a = rep(-1, 2), b = rep(-1, 2), text = c("calculated from model", "glm function"))
legend2 <- tibble::tibble(a = -1, b = -1, text = c("true trajectory"), text2 = c("measurements"))

dplyr::bind_rows(narrow_data, wide_data) %>%
  ggplot2::ggplot() +
    geom_point(aes(t, x)) +
    geom_function(data = narrow_data, fun = ~x_position(.x, estimated_theta_0_narrow, estimated_theta_1_narrow, true_theta_2)) +
    geom_function(data = narrow_data, fun = ~x_position(.x, true_theta_0, true_theta_1, true_theta_2), colour = "blue", size = 2, alpha = 0.5) +
    geom_function(data = narrow_data, fun = ~x_position(.x, glm_result_narrow$coefficients[["(Intercept)"]], glm_result_narrow$coefficients[["t_narrow"]], true_theta_2), linetype = "dashed") +
    geom_function(data = wide_data, fun = ~x_position(.x, estimated_theta_0_wide, estimated_theta_1_wide, true_theta_2)) +
    geom_function(data = wide_data, fun = ~x_position(.x, true_theta_0, true_theta_1, true_theta_2), colour = "blue", size = 2, alpha = 0.5) +
    geom_function(data = wide_data, fun = ~x_position(.x, glm_result_wide$coefficients[["(Intercept)"]], glm_result_wide$coefficients[["t_wide"]], true_theta_2), linetype = "dashed") +
    geom_line(data = legend, aes(a, b, linetype = text)) +
    geom_line(data = legend2, aes(a, b, color = text), size = 2) +
    geom_point(data = legend2, aes(a, b, shape = text2)) +
    theme(
      legend.title=element_blank(),
      legend.margin = margin(t = -9, b = -8, unit = "pt")) +
    scale_color_manual(values=c("blue")) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 1),
      limits = c(0, 10)) +
    ylim(-500, 400) +
    facet_wrap(~type)
