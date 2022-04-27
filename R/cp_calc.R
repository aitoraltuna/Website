library(tidyverse)
library(minpack.lm)
library(glue)

.data <- tibble(
  "PowerO" = 1:3,
  "TimeD" = 1:3
)

.data["PowerO"][.data["PowerO"] == 1] <- 420
.data["PowerO"][.data["PowerO"] == 2] <- 390
.data["PowerO"][.data["PowerO"] == 3] <- 350
.data["TimeD"][.data["TimeD"] == 1] <- 180
.data["TimeD"][.data["TimeD"] == 2] <- 360
.data["TimeD"][.data["TimeD"] == 3] <- 720

.data

power_column <- "PowerO"
time_column <- "TimeD"

a_start <- max(.data[[{{ power_column }}]]) * min(.data[[{{ time_column }}]])
b_start <- min(.data[[{{ power_column }}]]) * 0.9


cp_formula <- glue::glue("{time_column} ~ AWC / ({power_column} - CP)")


model <- minpack.lm::nlsLM(
  formula = cp_formula,
  data = .data,
  start = list(AWC = a_start, CP = b_start),
  control = list(maxiter = 1000),
  lower = c(AWC = 0, CP = 0)
)

coeff_cp <- summary(model)$coeff[2,1]
coeff_cp_se <- summary(model)$coeff[2,2]
coeff_awc <- summary(model)$coeff[1,1]
coeff_awc_se <- summary(model)$coeff[1,2]
r2 <- cor(.data[[{{ time_column }}]], predict(model)) ^ 2
print(coeff_cp)

plot_cp <- function(
    .data,
    power_column,
    time_column
) {

new_data_seq <- 1
intensity_label <- "Power (W)"
linear_label <- "Work (J)"

min_power_output <- coeff_cp
max_power_output <- 1500

new_data <- dplyr::tibble(
  !!{{ power_column }} := seq(min_power_output, max_power_output, new_data_seq)
)

data_plot <- broom::augment(model, newdata = new_data)
names(data_plot) <- c({{ power_column }}, {{ time_column }})

cp_plot <- data_plot %>%
  ggplot2::ggplot(ggplot2::aes(!!rlang::sym(power_column), !!rlang::sym(time_column))) +
  ggplot2::geom_line(colour = "black", size = 1) +
  ggplot2::geom_point(data = .data, shape = 16, size = 4, colour = "black") +
  ggplot2::geom_vline(xintercept=coeff_cp,linetype=2) +
  ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  ggplot2::labs(x = intensity_label,
                y = "Time (s)",
                title = bquote("Power-Duration Curve")) +
  ggplot2::theme_light() +
  ggplot2::coord_flip()
 cp_plot + scale_x_continuous(limits = c(0, 1200)) +
   scale_y_continuous(limits = c(0, 1500)) +
   theme_classic() +
   theme(plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = (coeff_cp-40), y = 1250, label = glue("CP = {round(coeff_cp, digits=0)} W"))
}
save_plot <- function() {
  plot_cp(.data, power_column, time_column)
  ggsave("cp1.png", last_plot(), dpi = 500)
}

