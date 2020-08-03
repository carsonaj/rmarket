#' @export
get.price.df <- function(ticker, length=200, log=FALSE) {
    api_key <- "pk_bad2e64778134e27a0e02addcaed2405"
    url <- paste(
        "https://cloud.iexapis.com/stable/stock/",
        ticker, "/chart/max?token=",
        api_key, sep=""
    )
    query <- list(chartLast=length)
    request <- httr::GET(url, query=query)
    response <- httr::content(request, as="text")
    price.data <- jsonlite::fromJSON(response, flatten = TRUE)
    price.df <- price.data[c("date", "close")]
    colnames(price.df) <- c("date", "price")
    price.df$date <- as.Date(price.data$date, format = "%Y-%m-%d")

    if (log==TRUE) {
        price.df$price <- log(price.df$price)
    }

    return(price.df)
}

#' @export
get.return.df <-function(price.df) {
    return.df <- data.frame(
        price.df$date[-1],
        diff(price.df$close)/price.df$close[-length(price.df$close)]
    )
colnames(return.df) <- c("date", "return")

return return.df
}
