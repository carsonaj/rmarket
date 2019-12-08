moving.average <- function(price.df, length=20) {
    avg <- as.numeric(
        stats::filter(price.df$close, rep(1/length, length), sides = 1)
    )
    avg.df <- data.frame(matrix(ncol=3, nrow=length(price.df$date)))
    colnames(avg.df) <- c("date", "close", "moving.average")
    avg.df$date <- price.df$date
    avg.df$close <- price.df$close
    avg.df$moving.average <- avg

    return(avg.df)
}

bollinger.bands <- function(price.df, length=20) {
    bbands.df <- movin.average(price.df, length)
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

bollinger.bands.plot <- function(bbands.df) {
    ggplot() +
    geom_line(data=bbands.df[c("date", "close")], aes(x=date, y=close), color="black") +
    geom_line(data=bbands.df[c("date", "moving.average")], aes(x=date, y=moving.average), color="red") +
    geom_line(data=bbands.df[c("date", "upper.band")], aes(x=date, y=upper.band), color="red", linetype="dashed") +
    geom_line(data=bbands.df[c("date", "lower.band")], aes(x=date, y=lower.band), color="red", linetype="dashed")+
    xlab("date") + ylab("price") 
}
