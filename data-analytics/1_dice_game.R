dice <- strsplit(intToUtf8(0x2680L:0x2685), split = "")[[1]]

roll  <- function() {
  die = 1:6
  d = sample(x = die, size = 2, replace = T)
  cat(dice[d], '\n')
  glue::glue("{d[1]} + {d[2]} = {sum(d)}")
}

roll()
