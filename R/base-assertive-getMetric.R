# @seealso `assertive.properties:::get_metric()`.
getMetric <- function(metric) {
    switch(
        EXPR = metric,
        length = isOfLength,
        elements = hasElements,
        stop("The metric", metric, "is not valid.", domain = NA)
    )
}
