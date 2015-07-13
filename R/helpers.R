#' Prints hello
#'
#' @return Names of the authors.
#' @examples
#' hello()
hello <- function() {
  print("Hello, world, it is Stinerock and Novik!")
}

#' Plots a normal histogram
#'
#' @param n Number of random draws.
#' @return Plots a histogram of n standard normals.
#' @examples
#' ggplot_it()
#' ggplot_it(1000)
ggplot_it <- function(n = 100) {
  x <- rnorm(n)
  ggplot2::qplot(x)
}

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

#table_1_1 <- read.csv("~/Google Drive/Statistics with R Book/prod/01-chapter/table_1-1.csv", stringsAsFactors = FALSE)
#table_1_4 <- read.csv("~/Google Drive/Statistics with R Book/prod/01-chapter/table_1-4.csv", stringsAsFactors = FALSE)
#devtools::use_data(table_1_4, table_1_1)

#tv_hours <- read.csv("~/Documents/Rfiles/tvhrs.csv", stringsAsFactors = FALSE)
#devtools::use_data(tv_hours)

#students <- read.csv("~/Documents/Rfiles/student.csv", stringsAsFactors = FALSE)
#devtools::use_data(students)

# poverty <- read.csv("~/Documents/Rfiles/USdata.csv", stringsAsFactors = FALSE)
# devtools::use_data(poverty)

