#' @export
get.price.data <- function(ticker, length=200) {
    api_key <- "pk_bad2e64778134e27a0e02addcaed2405"
    url <- paste(
        "https://cloud.iexapis.com/stable/stock/",
        ticker, "/chart/max?token=",
        api_key, sep=""
    )
    query <- list(chartLast=length)
    request <- httr::GET(url, query=query)
    response <- httr::content(request, as="text")
    price.df <- jsonlite::fromJSON(response, flatten = TRUE)
    price.data <- df[c("date", "close")]

    return(price.data)

}
