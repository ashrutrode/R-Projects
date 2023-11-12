NUMBER_TO_PRIME_FACTORIZE <- 10

prime_factorization <- function(num, factor_vector) {

	# set divisor to -1
	divisor <- -1

	# seeing if num is divisible by 2 thru num-1 
	for (i in 2:(num-1)) {

		# when we find a match, save divisor and break for loop
		if (num%%i == 0) {
			divisor <- i
			break
		}
	}

	# if there was no divisor, we're done
	if (divisor == -1) {

		# if length of factor_vector is 0, 
		# that means the number is prime
		# so show that to user
		if (length(factor_vector) == 0) {
			factor_vector = c(1, num)
		}

		# otherwise, add the num (the last prime factor) to factor_vector
		else {
			factor_vector <- append(factor_vector, num)
		}

		# show result
		return(factor_vector)

	}

	# if there was a divisor...
	else if (divisor != -1) {

		# save this divisor, which is prime
		factor_vector <- append(factor_vector, divisor)

		# create a new num
		num <- num/divisor

		# call func again
		prime_factorization(num, factor_vector)

	}

}

prime_factorization(NUMBER_TO_PRIME_FACTORIZE, c())
