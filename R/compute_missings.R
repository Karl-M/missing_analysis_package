#' Count Missings
#'
#' Function which counts Missings in Dataframe and stores them with the Column Names
#' in Dataframe
#' @param data Dataframe in which missings will be counted
#' @keywords missings
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
#' count_missings(df)
#'

count_missings <- function(data) {
    mi <- apply(data, 2, function(x) sum(is.na(x)))
    p <- 1:ncol(data)
    df.mi <- as.data.frame(cbind(p, mi), row.names = F)
    df.mi$mi <- as.numeric(df.mi$mi)
    df.mi$p <- names(data)
    df.mi$p <- factor(df.mi$p, levels = unique(as.character(df.mi$p)))
    return(df.mi)
}
















