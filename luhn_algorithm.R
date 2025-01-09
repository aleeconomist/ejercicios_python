# Define la función verify_card_number
verify_card_number <- function(card_number) {
  sum_of_odd_digits <- 0
  card_number_reversed <- rev(strsplit(card_number, NULL)[[1]])
  odd_digits <- card_number_reversed[seq(1, length(card_number_reversed), by = 2)]
  for (digit in odd_digits) {
    sum_of_odd_digits <- sum_of_odd_digits + as.numeric(digit)
  }
  
  sum_of_even_digits <- 0
  even_digits <- card_number_reversed[seq(2, length(card_number_reversed), by = 2)]
  for (digit in even_digits) {
    number <- as.numeric(digit) * 2
    if (number >= 10) {
      number <- sum(as.numeric(strsplit(as.character(number), NULL)[[1]]))
    }
    sum_of_even_digits <- sum_of_even_digits + number
  }
  
  total <- sum_of_odd_digits + sum_of_even_digits
  return(total %% 10 == 0)
}

# Añade el número a comprobar.
my_card_number <- '4111-1111-4555-1142'

# Elimina guiones y espacios
translated_card_number <- gsub("[- ]", "", my_card_number)

# Llama a la función y muestra el resultado
if (verify_card_number(translated_card_number)) {
  print('VALID!')
} else {
  print('INVALID!')
}

