# @seealso `base::stopifnot()`.
.Dparse <-  # nolint
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            paste(ch[[1L]], "....")
        } else {
            ch
        }
    }
