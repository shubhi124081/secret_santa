# Load libs 
library(R.utils)

# Some functions I need 
# Timeout function
timeoutCatch <- function(func, timeout = 10, ...) {
  val <- tryCatch({
    withTimeout({
      func(...)
    }, timeout = timeout)
  }, TimeoutException = function(ex) {
    # If evaluation of the function reaches threshold, return 'Timeout!'
    return('Timeout!')
  })
  return(val)
}

# Assign a secret santee to every person 
# Also, make sure no one is assigned themselves - there's an elegant solution 
# out there, this is not it 

getNames <- function(x) {
  n <- length(x)
  ind1 <- sample(1:n, n)
  ind2 <- sample(1:n, n)
  df1 <- data.frame("Person1" = x[ind1], "Person2" = x[ind2])
  while (sum(df1[, 1] == df1[, 2]) != 0) {
    df1 <- getNames(x)
  }
  return(df1)
}

# Function to write out everyone's names 
writeNameFn <- function(m1, m2, path1) {
  init1 <- file(paste0(path1, "For_", m1, ".txt"))
  writeLines(paste0("Your person is ", m2), init1)
  close(init1)
}

# Generate the names, write out files with each person's secret santee 
# Note: This function doesn't return any values!

secret_santa <- function(name_vec, path = "~/Downloads/") {
# Get rid of spaces in between names 
name_vec <- gsub(" ", "", name_vec)

# Run assigned names within in the timeout function 
# (just in case it takes forever)
df <- timeoutCatch(getNames, x = name_vec)

apply(df, 1, function(x) writeNameFn(m1 = x["Person1"], m2 = x["Person2"],
                                     path1 = path))
}

secret_santa(name_vec = c("Fabiola", "John", "Leila", "Tamara", "Scott", 
                          "Kevin", "Richard", "Shubhi", "Jeremey"),
             path = "~/Downloads/")

