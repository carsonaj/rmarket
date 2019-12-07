library("httr")
library("jsonlite")

get.price.data <- function(ticker, length=200) {
    api_key <- "pk_06ea5c94527b4754a9196efb576df932"
    url <- paste(
        "https://cloud.iexapis.com/stable/stock/",
        ticker, "/chart/max?token=",
        api_key, sep=""
    )
    query <- list(chartLast=length)
    request <- GET(url, query=query)
    response <- content(request, as="text")
    price.df <- fromJSON(response, flatten = TRUE)
    price.data <- df[c("date", "close")]

}
