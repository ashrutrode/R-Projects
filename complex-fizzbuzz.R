final_positive_int <- 100
fizzbuzz <- function(num) {
for (i in 1:num) {
if (i%%3 == 0 & i%%5 == 0) {print("FizzBuzz")}
else if (i%%3 == 0) {print("Fizz")}
else if (i%%5 == 0) {print("Buzz")}
else {print(i)}
}
}
fizzbuzz(final_positive_int)
