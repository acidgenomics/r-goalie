# @seealso `base::stopifnot()`.
abbrev <- function(ae, n = 3L) {
    head <- function(x, n = 6L) {
        x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
    }
    paste(c(head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")
}
