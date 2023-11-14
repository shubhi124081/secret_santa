# Script to assign Secret Santa considering not_allowed pairs

# Some functions I need
# Worker fucntion to process list of "illegal" combinations
illegal_process <- function(names_vec, illegal) {
  # create a data frame containing the illegal combinations per name and
  # blanks for names that don't have illegal combinations
  # bind
  df <- do.call(rbind, illegal)
  colnames(df) <- c("gifter", "giftee")
  df2 <- data.frame(
    "gifter" = names_vec,
    "giftee" = rep("", length(names_vec))
  )
  # match illegal combinations with the names in name vec
  df2[, "giftee"] <- df[match(names_vec, df[, "gifter"]), "giftee"]
  df2 <- replace(df2, is.na(df2), "")
  return(df2)
}

# Assign everybody a valid secret santa
get_names <- function(names_vec, illegal) {
  n <- length(names_vec)
  repeat {
    assigned <- sample(names_vec)
    valid_assignment <- all(sapply(1:n, function(i) {
      paste0(names_vec[i], "-", assigned[i]) %in%
        paste0(illegal[i, ], collapse = "-")
    })) == FALSE
    # Check that no one has got themself
    if (sum(assigned == names_vec) != 0) valid_assignment <- FALSE

    if (valid_assignment) {
      break
    }
  }
  return(assigned)
}

# Function to write out everyone's names
write_name_fn <- function(m1, m2, path1) {
  init1 <- file(paste0(path1, "For_", m1, ".txt"))
  writeLines(paste0("Your person is ", m2), init1)
  close(init1)
}

# Workflow
assign_secret_santa <- function(names_vec, illegal) {
  # Make sure not_allowed pairs are sorted to handle order-independent input
  illegal <- illegal_process(names_vec, illegal)

  # Randomly shuffle the names until a valid assignment is found
  assigned <- get_names(names_vec, illegal)
  # Create a data frame with names and their assigned Secret Santas
  result <- data.frame(Name = names_vec, SecretSanta = assigned)
  return(result)
}

# Data
# Pool
names_vec <- c(
  "Name 1", "Name 2", "Name 3", "Name 4", "Name 5", "Name 6", "Name 7",
  "Name 8", "Name 9", "Name 10"
)

# Illegal pairs
# On the right is the secret santa and on the left is the santee
illegal <- list(
  c("Name 1", "Name 2"),
  c("Name 3", "Name 4"),
  c("Name 5", "Name 6")
)

# Result
result <- assign_secret_santa(names_vec, illegal)

# Write out
apply(result, 1, function(x) {
  write_name_fn(
    m2 = x["Name"], m1 = x["SecretSanta"],
    path1 = "~/Downloads"
  )
})
