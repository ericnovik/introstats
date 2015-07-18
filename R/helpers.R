#' Updates the introstats pacakge from github
#'
#' @examples
#' update_package()
update_package <- function() {
  if (suppressMessages(suppressWarnings(require("introstats")))) {
    remove.packages("introstats")
  }
  if (!suppressMessages(suppressWarnings(require("devtools")))) {
    install.packages("devtools")
  }
  devtools::install_github("ericnovik/introstats")
}

#' Plots normal densities
#'
#' @param n_dens Number of densities to draw with standard deviations from 1 to n_dens
#' @return ggplot2 plot object
#' @examples
#' plot_normals()
#' plot_normals(5)
plot_normals <- function (n_dens = 3) {
  n <- 1000
  x <- seq(-10, 10, length.out = n)
  dens <- matrix(ncol = n_dens, nrow = n)

  for (i in 1:n_dens) {
    dens[, i] <- dnorm(x, mean = 0, sd = i)
  }

  colnames(dens) <- paste0("sd_", 1:n_dens)
  dens <- as.data.frame(dens)
  dens <- dplyr::mutate(dens, z_score = x)
  dens <- tidyr::gather(dens, sd, obs, -z_score)
  ggplot2::qplot(z_score, obs, data = dens, colour = sd, geom = "line") +
    ggplot2::ylab("Density") +
    ggplot2::xlab("Z Score") +
    ggplot2::theme_bw()
}

#' Plots the distributions of the sample means from the population.
#'
#' @param pop Vector containing the population (anything over 10,000 will take a long time to run on most ocmputers)
#' @param n Number of replicas of each sample
#' @param size Vector containing sizes for each sampling distribution
#' @return ggplot2 plot object
#' @examples
#' x <- runif(10000)
#' plot_sampling_dist(x)
#' plot_sampling_dist(x, n = 1e6, size = c(2, 10, 50, 100))
plot_sampling_dist <- function (pop,   n = 1e5, size = c(2, 5, 30)) {
  dens <- matrix(ncol = length(size), nrow = n)

  for (i in seq_along(size)) {
    dens[, i] <- replicate(n, mean(sample(pop, size = size[i])))
  }

  colnames(dens) <- paste0("n_", size)
  dens <- as.data.frame(dens)
  dens <- tidyr::gather(dens, size, obs)

  ggplot2::qplot(obs, data = dens, fill = size, color = size,
                 geom = "density", alpha = I(1/3)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab("") +
    ggplot2::theme_bw()
}

#table_1_1 <- read.csv("~/Google Drive/Statistics with R Book/prod/01-chapter/table_1-1.csv", stringsAsFactors = FALSE)
#table_1_4 <- read.csv("~/Google Drive/Statistics with R Book/prod/01-chapter/table_1-4.csv", stringsAsFactors = FALSE)
#devtools::use_data(table_1_4, table_1_1)

#tv_hours <- read.csv("~/Documents/Rfiles/tvhrs.csv", stringsAsFactors = FALSE)
#devtools::use_data(tv_hours)

#students <- read.csv("~/Documents/Rfiles/student.csv", stringsAsFactors = FALSE)
#devtools::use_data(students)

# poverty <- read.csv("~/Documents/Rfiles/USdata.csv", stringsAsFactors = FALSE)
# devtools::use_data(poverty)

# exit <- read.csv("~/Downloads/exit.csv", stringsAsFactors = FALSE)
# colnames(exit) <- 'obama'
# exit$obama <- sample(exit$obama)
# devtools::use_data(exit, overwrite = TRUE)
