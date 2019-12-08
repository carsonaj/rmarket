sma <- function(price.df, length=20) {
    avg <- as.numeric(
        stats::filter(price.df$close, rep(1/length, length), sides = 1)
    )
    avg.df <- data.frame(matrix(ncol=2, nrow=length(price.df$date)))
    colnames(avg.df) <- c("date", "moving.average")
    avg.df$date <- price.df$date
    avg.df$moving.average <- avg

    return(avg.df)
}

bollinger.bands <- function(price.df, length=20) {
    bbands.df <- sma(price.df, length)
    dev <- as.numeric(
        stats::filter(
            (price.df$close - bbands.df$moving.average)^2, rep(1/(length-1), length), sides = 1
        )
    )

    lower.band <- bbands.df$moving.average+2*dev
    upper.band <- bbands.df$moving.average-2*dev

    bbands.df["lower.band"] <- lower.band
    bbands.df["upper.band"] <- upper.band

    return(bbands.df)
}
