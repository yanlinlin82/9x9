library(tidyverse)
library(emojifont) # show Chinese in PDF

#------------------------------------------------#

a <- "一二三四五六七八九十"
a <- strsplit(a, "")[[1]]

ch <- function(x) if (x < 10) a[[x]] else paste0(a[[x %/% 10]], a[[10]], if (x %% 10 > 0) a[[x %% 10]])
ch <- Vectorize(ch)

#------------------------------------------------#
# PNG or show plot directly

g <- expand.grid(A = 1:9, B = 1:9) %>%
  filter(A <= B) %>%
  ggplot(aes(A, 9 - B)) +
  geom_tile(color = "black", fill = NA, alpha = 0, size = .5) +
  geom_text(aes(label = sprintf("%d x %d = %d\n%s%s%s%s",
                                A, B, A * B, ch(A), ch(B),
                                ifelse(A*B<10,"得",""), ch(A*B))), size = 15, lineheight = 0.5) +
  theme_void()

print(g)

# write to png file
g %>% ggsave(filename = "9x9.png", width = 279, height = 210, units = "mm")

#------------------------------------------------#
# PDF (landscape layout)

g <- expand.grid(A = 1:9, B = 1:9) %>%
  filter(A <= B) %>%
  ggplot(aes(A, 9 - B)) +
  geom_tile(color = "black", fill = NA, alpha = 0, size = .5) +
  geom_text(aes(label = sprintf("%d x %d = %d\n%s%s%s%s",
                                A, B, A * B, ch(A), ch(B),
                                ifelse(A*B<10,"得",""), ch(A*B))), size = 5) +
  theme_void()

# write to pdf file (A4 size)
g %>% ggsave(filename = "9x9.pdf", width = 279, height = 210, units = "mm")

#------------------------------------------------#
# PDF (portrait layout)

g <- expand.grid(A = 1:9, B = 1:9) %>%
  filter(A <= B) %>%
  ggplot(aes(A, B)) +
  geom_tile(color = "black", fill = NA, alpha = 0, size = .5) +
  geom_text(aes(label = sprintf("%d x %d = %d\n%s%s%s%s",
                                A, B, A * B, ch(A), ch(B),
                                ifelse(A*B<10,"得",""), ch(A*B))), size = 5, angle = 90) +
  theme_void() +
  coord_flip()

# write to pdf file (A4 size)
g %>% ggsave(filename = "9x9_.pdf", width = 210, height = 279, units = "mm")
