
# Default STRing
dstr_zlab_depth  <- "depth"
dstr_zlab_time   <- "two-way travel time"
dstr_zunit_time  <- "ns"
dstr_zunit_depth <- "m"
dstr_xunit       <- "m"
dstr_xlab        <- "position"
dstr_yunit       <- "m"
dstr_ylab        <- "position"

dstr_version     <- "0.3"

dstr_dlab        <- "amplitude"
dstr_dunit       <- "mV"

msg_set_antsep <- c("You must first set the antenna separation distances with\n",
                    "'antsep(x) <- ...")
msg_do_shiftToTime0  <- c("You must first shift the traces to time-zero with\n",
                          "'shiftToTime0()'")
msg_set_zunitToDepth <- c("The signal is a function of depth and not time.\n",
                          "If you absolutely want to apply 'velSpectrum()',\n",
                          "change the unit with zunit(x) <- 'm', for example.")