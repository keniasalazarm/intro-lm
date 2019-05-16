library(kableExtra)
library(showtext)
showtext_auto()
PrettyTable <- function(data, cap) {
  data %>% 
    kable(caption = cap) %>% 
    kable_styling(bootstrap_options = c("striped", "hover",
                                        "condensed", "responsive"),
                  full_width = FALSE)
}
