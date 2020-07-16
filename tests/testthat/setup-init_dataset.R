
Sys.setenv(LANG = "en")

options(warn = 2)
options(warn = 1)
options(stringsAsFactors = FALSE)
options(width = 200)
# options(tidyselect_verbosity = "verbose")
# getOption("tidyselect_verbosity")

library(dplyr, warn.conflicts = FALSE)
library(survival, warn.conflicts = FALSE)


mtcars3 = as_tibble(mtcars2)
mtcars3$cyl[1:5] = NA
mtcars3$vs[5:12] = NA
mtcars3$cyl3 = mtcars3$cyl==3
mtcars3$cyl6 = mtcars3$cyl==6
mtcars3$dummy = "dummy"
mtcars3$dummy_na = NA
mtcars3$dummy_num_vs = ifelse(mtcars3$vs=="vshaped", 0, rnorm(15))
mtcars3$dummy2 = mtcars3$dummy
mtcars3$dummy2[5:12] = NA
mtcars3$test = rbinom(nrow(mtcars3), 1, 0.5) %>% factor(labels = c("A","B"))
mtcars3$surv = Surv(mtcars3$disp, mtcars3$am=="manual") %>% set_label("Dummy survival")
mtcars3$my_date = as.Date(mtcars2$hp , origin="2010-01-01") %>% set_label("Some nonsense date")
mtcars3$my_posix = as.POSIXct(mtcars2$qsec*3600*24 , origin="2010-01-01") %>% set_label("Date+time")


