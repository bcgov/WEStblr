#'rounded
#'
#'Rounds positive numbers to integers (i.e., zero decimal places).
#'
#'The base R round function "rounds to even" when rounding off a 5 (e.g., 0.5 rounds to 0 and 1.5 rounds to 2). To ensure that 0.5 is rounded up,
#'\code{rounded} adds 0.5 to the input and truncates. Note that this function only works for positive numbers.
#'
#'@param x A numeric vector
#'@examples
#'rounded(.5 + 0:4) # 1  2  3  4  5
#' ## different from IEEE / IEC rounding: 0  2  2  4  4
rounded <- function(x) { trunc(x+0.5) }

