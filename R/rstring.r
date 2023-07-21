#" Make a nearly-guaranteed unique string
#"
#" `rstring()` makes a string that is statically extremely likely to be unique.
#"
#" @param n Numeric integer: How many strings to make (default is 1).
#" 
#" @param x Numeric integer: Number of letters and digits to use to make the string. Default is 10, leading to a probability of two matching random strings of <1E-38.
#"
#" @returns Character.
#"
#" @examples
#"
#" rstring(1)
#" rstring(5)
#" rstring(5, 2)
#"
#" @export
rstring <- function(n, x = 10) {

	n <- as.integer(n)
	x <- as.integer(x)
	if (n < 0L | x < 0L) stop(sQuote("n"), " and ", sQuote("x"), " must be > 0.")
	from <- c(letters, LETTERS, 0L:9L)

	out <- rep(NA_character_, n)
	for (i in seq_len(n)) out[i] <- paste(sample(from, x, TRUE), collapse="")
	out
	
}
