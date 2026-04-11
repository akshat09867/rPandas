library(ggplot2)

# Test 1: AND operator
result1 <- rp_filter(diamonds, carat > 1 & cut == "Ideal", return.as = "code")
print(head(result1))

# Test 2: OR operator
result2 <- rp_filter(diamonds, clarity == "VVS1" | clarity == "VVS2")
print(head(result2))

# Test 3: NOT operator
result3 <- rp_filter(diamonds, !(cut == "Ideal"))
print(head(result3))

# Test 4: %in% with an external variable
allowed_colors <- c("G", "H", "I")
result4 <- rp_filter(diamonds, color %in% allowed_colors)
print(head(result4))

# Test 5: %notin% operator
result5 <- rp_filter(diamonds, color %notin% c("D", "E", "F"))
print(head(result5))

# Test 6: Complex nested expression
result6 <- rp_filter(
  diamonds,
  (carat > 0.9 & price < 10000) | (cut == "Premium" & !(color %in% c("J", "I")))
)
print(head(result6))


result7 <- rp_select(diamonds,carat,color,cut,x,y,z)
print(head(result7))

result8 <- rp_sort(diamonds, "carat", desc(table),return.as='code')
print(head(result8))

result9 <- rp_mutate(diamonds, price_per_carat=price/carat, depth_pct = depth / 100, )
print(head(result9))

result10 <- rp_summarize(
    ggplot2::diamonds, 
    avg_price = mean(price), 
    std_carat = sd(carat),
    count = n(),
    .by = c(cut, color)
  )
print(head(result10))

result11 <- rp_calculate(
  ggplot2::diamonds,
  price, carat,
  the.functions = c("mean", "sd"),
  .by = cut
)

print(head(result11))

result12 <- rp_first_k_rows(
  ggplot2::diamonds,
  1,
 .by = c(cut, clarity)
  )

print(result12)

result13 <- rp_last_k_rows(
  ggplot2::diamonds,
  7,
 .by = c(cut, color)
  )

print(result13)

result14 <- rp_mutate(ggplot2::diamonds, 
                    to_remove = c("cut","price"),
                    price_per_carat = price / carat)
print(head(result14))

result15 <- rp_count(ggplot2::diamonds, .by = c(cut, color))
print("rp_count------------------")
print(head(result15))


result16 <- diamonds %>%
  rp_filter(price > 15000) %>%
  rp_count(.by = cut)

  print(result16)



result12 <- rp_first_k_rows(
  ggplot2::diamonds,
  3,
 .by = c(cut, clarity)
  )

print(result12)



# print(head(rp_filter(.data = diamonds, filter_expression = price >= 18500, return.as = "code")))
# print(head(rp_select(.data = diamonds, price, carat, return.as = "all")))
# print(head(rp_mutate(.data = diamonds, expensive = (price >= 1000), return.as = "code")))
# print(head(diamonds <- rp_mutate(.data = diamonds, expensive = price >= 1000)))
# print(head(rp_first_k_rows(.data = diamonds, .by = cut, k = 3)))
# print(head(rp_first_k_rows(.data = diamonds, .by = "cut", k = 3, return.as = "code")))
# print(head(rp_last_k_rows(.data = diamonds, k = 3, .by = c("cut", "clarity"))))
# print(head(rp_last_k_rows(.data = diamonds, k = 3)))
# print(head(rp_count(.data = diamonds, .by = cut, return.as = "result")))
# print(head(rp_calculate(.data = diamonds, x, y, z, price, .by = cut, the.functions = c("mean", "median", "sd"))))




