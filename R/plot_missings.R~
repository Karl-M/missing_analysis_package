#' plot_missings
#'
#' Function for plotting number of missing data,
#'    can be used in Conjunction with count_missings
#'
#'@param data Dataframe in which missings are stored, columns have to be named
#'    "mi" and "p"
#' @param grid whether gridlines should be plotted, requieres package("cowplot")
#' @param lbs Labelsize of variable names, shoul be smaller, if more variables are plotted
#' @keywords missings, plot
#' @export
#' @examples
#' df <- as.data.frame(cbind(
#'    name = c("paul", "daniel", "paula", "beate"),
#'    sex = c("m", "m", "w", NA),
#'    income = c(1000, NA, 500, NA),
#'    weight = c(NA, NA, 70, NA),
#'    vote = c(NA, NA, NA, NA)
#'    ))
#'
#' num.mis <- count_missings(df)
#' plot_missings(num.mis, "none", 10)


plot_missings <- function(data, grid, lbs) {
    ggplot(data, aes_string("mi", "p")) + geom_point() +
      xlab("Anzahl Missings") + ylab("") +
      background_grid(grid) +
      theme(
          axis.text.y = element_text(size = lbs)
          )
}


