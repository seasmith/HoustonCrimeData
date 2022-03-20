index <- function(x) {
  min_x <- unique(min(x))
  max_x <- unique(max(x))
  vapply(X = x,
         FUN = function(value) (value - min_x) / (max_x - min_x),
         FUN.VALUE = numeric(1))
}

odd  <- function(x) x %% 2 == 1
even <- function(x) x %% 2 == 0

not_one_of <- compose(`-`, one_of)
not <- `!`


month_end <- function (x) {
  ceiling_date(x, "month") - 1
}

month_start <- function (x) {
  floor_date(x, "month")
}
