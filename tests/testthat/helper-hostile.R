hostile_values <- function() {
  c(
    "",
    " ",
    "\t\n\r",
    "+",
    "++",
    "---",
    "abc",
    "phone: none",
    "0",
    "1",
    paste(rep("9", 300), collapse = ""),
    paste(rep("1", 10000), collapse = ""),
    "+99999999999999999999",
    "tel:;phone-context=",
    "tel:1234567;phone-context=bad domain!",
    "<script>alert(1)</script>",
    "'; DROP TABLE users; --",
    "\u2603\u2764\u2728"
  )
}
