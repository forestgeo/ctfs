# # Install bci ----
# tk <- "58c3725a83a8c53f8a8883bfd747442867279173"
# devtools::install_github("forestgeo/bci@dev", auth_token = tk)


cns1 <- bci::bci12full1
cns2 <- bci::bci12full2


growth.flexbin(
  growthtable,
  sizecol = "dbh",
  nobin = 2,
  start = NULL,
  startsd = NULL,
  sdmodel = linear.model.ctr,
  badsdfunc = NULL,
  method = "Gibbs",
  rep = 1200,
  show = 100,
  burn = 200,
  ...
)
