
# Write a function that extracts all words from a text that are capitalized but
# not at the beginning of a sentence. Words can be returned in any order, but
# each word should only be returned at most once.
#
# Your function should take one argument:
#  - `text`: `character(1)` scalar string, the text from which to extract
#    capitalized words.
# Your function should return a `character` vector: The capitalized words found
# in the string.
#
# Example results:
#
#> ex01ProperNoun("Proper Nouns are usually Capitalized.")
# --> "Nouns"  "Capitalized"
#> ex01ProperNoun("proper nouns are usually Capitalized. This is, Proper for proper nouns.")
# --> "Capitalized" "Proper"
#> ex01ProperNoun("The IBM5100 Portable Computer was one of the first portable computers.")
# --> "IBM5100" "Portable" "Computer"
#> ex01ProperNoun("IBM5100 is the name of one of the first portable computers.")
# --> character(0)
# You can assume that the text only contains uppercase and lowercase letters
# from the latin alphabet, as well as digits, points (".") and commas (",").
# Words are separated from each other by exactly one space, and are possibly
# followed by a point (which starts a new sentence, so capitalization of the
# following word is ignored) or a comma (which does *not* start a new sentence).
ex01ProperNoun <- function(text) {
  assertString(text)

  pattern <- "\\b(?<!^|\\. )[A-Z][[:alnum:]]*"
  found.words <- unlist(regmatches(text, gregexpr(pattern, text, perl = TRUE)))
  unique(found.words)
}
