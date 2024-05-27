# Функція для розбиття чисельного ряду на інтервали, використовуючи рівномірний розподіл
split_into_intervals <- function(x, n) {
  sorted <- sort(x)
  min_val <- min(sorted)
  max_val <- max(sorted)
  interval_size <- (max_val - min_val) / n
  intervals <- lapply(0:(n-1), function(i) c(min_val + i * interval_size, min_val + (i+1) * interval_size))
  return(intervals)
}

# Функція для визначення, до якого інтервалу належить число
assign_interval <- function(x, intervals) {
  index <- which(sapply(intervals, function(interval) x >= interval[1] & x <= interval[2]))
  if (length(index) > 0) {
    return(index)
  } else {
    return(-1)
  }
}

# Функція для перетворення числового ряду в лінгвістичний ряд
to_linguistic <- function(x, alphabet) {
  n <- length(alphabet)
  intervals <- split_into_intervals(x, n)
  interval_indices <- sapply(x, function(val) assign_interval(val, intervals))
  result <- sapply(interval_indices, function(index) if (index == -1) "?" else alphabet[index])
  return(result)
}

# Функція для побудови матриці передування
build_precedence_matrix <- function(linguistic, alphabet) {
  n <- length(alphabet)
  matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(alphabet, alphabet))
  
  for (i in 1:(length(linguistic)-1)) {
    row <- match(linguistic[i], alphabet)
    col <- match(linguistic[i+1], alphabet)
    matrix[row, col] <- matrix[row, col] + 1
  }
  
  return(matrix)
}

# Функція для читання чисельного ряду з файлу
read_series <- function(file_path) {
  series <- scan(file_path)
  return(series)
}

# Функція для читання параметрів з файлу
read_params <- function(file_path) {
  lines <- readLines(file_path)
  alphabet_size <- as.integer(lines[1])
  alphabet <- unlist(strsplit(lines[2], " "))
  return(list(alphabet_size = alphabet_size, alphabet = alphabet))
}

# Функція для запису результатів у консоль та у файл
write_results <- function(output_path, alphabet, linguistic, precedence_matrix) {
  # Виведення у консоль
  cat("Linguistic series: \n")
  cat(linguistic)
  cat("\n\nPrecedence matrix:\n")
  print(precedence_matrix)
  
  # Запис у файл
  cat("Linguistic series: \n", file = output_path)
  cat(linguistic, file = output_path, append = TRUE)
  cat("\n\nPrecedence matrix:\n", file = output_path, append = TRUE)
  write.table(precedence_matrix, file = output_path, append = TRUE, quote = FALSE, col.names = NA, row.names = TRUE, sep = "\t")
}


# Функція процесу повної обробки ряду
process_series <- function(series_path, params_path, output_path) {
  series <- read_series(series_path)
  params <- read_params(params_path)
  
  linguistic <- to_linguistic(series, params$alphabet)
  precedence_matrix <- build_precedence_matrix(linguistic, params$alphabet)
  
  write_results(output_path, params$alphabet, linguistic, precedence_matrix)
}

# Головна функція
main <- function() {
  start_time <- Sys.time()
  process_series("number series.txt", "params.txt", "output.txt")
  end_time <- Sys.time()
  
  duration <- end_time - start_time
  units <- attr(duration, "units")
  print(paste("Data written to output.txt. Duration:", duration, units))
}

# Виклик головної функції
main()
