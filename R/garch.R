get.cvar <- function(return.df) {
    fit <- auto.arima(return.df$return, seasonal=FALSE)
    order <- arimaorder(fit)

    garchSpec <- ugarchspec(
        variance.model=list(model="sGARCH", garchOrder=c(1,1)),
        mean.model=list(armaOrder=c(order[1],order[3])),
        distribution.model="std"
    )

    garchFit <- ugarchfit(spec=garchSpec, data=return.df$return)

    date <- return.df$date
    conditional.variance <- garchFit@fit$sigma
    cvar.df <- data.frame(date, conditional.variance)

    return(cvar.df)
}
