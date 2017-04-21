# inst --------------------------------------------------------------------

tk <- "58c3725a83a8c53f8a8883bfd747442867279173"
devtools::install_github("forestgeo/ctfs", auth_token = tk)
devtools::install_github("forestgeo/bci@dev", auth_token = tk)



# pkg ---------------------------------------------------------------------

library(testthat)
library(bci)
library(ctfs)
library(tibble)
library(dplyr)



# dat ---------------------------------------------------------------------

cns6 <- bci::bci12full6
cns6split <- ctfs::split.data(cns6)
spptable <- bci::bci12spptable %>% filter(sp == "poular")



# tst --------------------------------------------------------------------



head(cns6)

cns6 %>% 
  as.tibble() %>% 
  transmute(x1 = gx, y1 = gy)


complete()


bci_elevation %>% 
  transmute(x1 = x, y1 = y) %>% 
  unique



, x2 = sample(x1), y2 = sample(y1), 
    htdiff = rnorm(length(x1))) %>% 
  rearrangeSurveyData() %>% 
  


purrr::map(bci_elevation, range)
purrr::map(bci_elevation, unique)

library(tidyr)
complete()
tibble(
  x1 = seq(0, 1000, 10), y1 = seq(0, 500, 5),
  x2 = seq(0, 1000, 10), y2 = seq(0, 500, 5),
  htdiff = rnorm(length(x1))
  ) %>% 
  rearrangeSurveyData() %>% 
  .$IDtable %>% 
  solve.topo()



df <- tibble(
  pt1 = c(1, 1, 2, 2, 3, 4),
  pt2 = c(730, 31, 731, 3, 4, 5),
  dtdiff = c(-0.17, 0, -.7, -1.4, -.35, 0.17)
)
  

solve.topo()



# solve.topo
# rearrangeSurveyData 


