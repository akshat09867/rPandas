library(testthat)

# Load the function (devtools::load_all())

# Test 1: AND operator (&)
translate_filter(carat > 1 & cut == "Ideal")
#> [1] "(carat > 1) and (cut == 'Ideal')"

# Test 2: OR operator (|)
translate_filter(clarity == "VVS1" | clarity == "VVS2")
#> [1] "(clarity == 'VVS1') or (clarity == 'VVS2')"

# Test 3: NOT operator (!)
translate_filter(!(cut == "Ideal"))
#> [1] "(not (cut == 'Ideal'))"

# Test 4: %in% operator
allowed_colors <- c("G", "H", "I")
translate_filter(color %in% allowed_colors)
#> [1] "(color in @allowed_colors)"

# Test 5: %notin% operator (newly added)
# Define a placeholder function for the test
`%notin%` <- function(x, y) !x %in% y
translate_filter(color %notin% c("D", "E", "F"))
#> [1] "(color not in ['D', 'F', 'F'])"

# Test 6: Complex nested expression
translate_filter(
  (carat > 0.9 & price < 10000) | (cut == "Premium" & !(color %in% c("J", "I")))
)
#> [1] "((carat > 0.9) and (price < 10000)) or ((cut == 'Premium') and (not (color in ['J', 'I'])))"