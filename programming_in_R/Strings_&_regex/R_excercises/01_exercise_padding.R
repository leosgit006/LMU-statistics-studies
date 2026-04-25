# Write a function that pads non-negative integer numbers to the same length by
# preprending 0s when necessary.
#
# Input:
# - `numbers`: A `numeric` containing non-negative integer numbers.
#
# Return: A `character` with the same length as `numbers`, containing strings
# that each have as many digits as the longest number in `numbers`.
#
# > ex01PadNumbers(c(1, 2, 3))  # c("1", "2", "3")
# > ex01PadNumbers(c(1, 2, 30))  # c("01", "02", "30")
# > ex01PadNumbers(c(100, 2, 30))  # c("100", "002", "030")
# > ex01PadNumbers(numeric(0))  # character(0)
# > ex01PadNumbers(c(0, 0, 0))  # c("0", "0", "0")
# > ex01PadNumbers(NA)  # NA
# > ex01PadNumbers(c(1, NA, 100))  # c("001", NA, "100")
#
# `numbers` may contain missing values, which should result in `NA` values. Make
# sure that your function does not return a string containing "NA" here.
#
# Your function should assert that numbers are non-negative values.
#
# `?spintf()` could help you here.
ex01PadNumbers <- function(numbers) {
  # your code
  assertIntegerish(numbers, lower = 0)
  if (!length(numbers)) return(character(0))

  max.char <- max(nchar(numbers), 1, na.rm = TRUE)
  ifelse(is.na(numbers),
         NA_character_,
         sprintf(paste0("%0", max.char, "d"), numbers)
  )
}

# Write a function that pads numbers in file-names.
#
# Your function is given a vector of file names that may each contain at most
# one number. The numbers that occur in all files should be padded to the same
# length, as done in ex01PadNumbers.
#
# Padding numbers like this can be useful when sorting files alphabetically, to
# make sure that the same files that differ by a number are sorted correctly.
#
# Input:
# - `filenames`: A `character` containing file-names.
#
# Return: A `character` with the same length as `filenames`, containing filenames
# with padded numbers, if any.
#
# > ex02PadFiles("file.pdf")  # "file.pdf"
# > ex02PadFiles(c("file1.pdf", "file10.pdf"))  # c("file01.pdf", "file10.pdf")
# > ex02PadFiles(c("file.pdf", "file10.pdf"))  # c("file.pdf", "file10.pdf")
# > ex02PadFiles(c("100-music.pdf", "file-10.pdf", "help.txt"))
#   # c("100-music.pdf", "file-010.pdf", "help.txt")
# > ex02PadFiles(character(0))  # character(0)
#
# Your function should not accept missing values.
ex02PadFiles <- function(filenames) {
  # your code
  assertCharacter(filenames, any.missing = FALSE)
  if (!length(filenames)) return(character(0))

  pattern <- "[0-9]+"
  matches <- regexpr(pattern, filenames)
  nums <- regmatches(filenames, matches)
  filled.nums <- ex01PadNumbers(as.numeric(nums))
  regmatches(filenames, matches) <- filled.nums
  filenames
}
