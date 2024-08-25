# survival POC

    Code
      df = data.frame(time = c(71.1, 75.7, 78.7, 79, 95.1, 108, 120.1, 120.3, 121,
        140.8, 145, 146.7, 160, 160, 167.6, 167.6, 225, 258, 275.8, 275.8, 275.8, 301,
        304, 318, 350, 351, 360, 360, 400, 440, 460, 472), event = rep(rep(c(1, 0), 6),
      c(6L, 1L, 2L, 1L, 1L, 1L, 2L, 7L, 1L, 3L, 1L, 6L)), group = rep(c("B", "A", "B",
        "A", "B", "A", "B", "A", "B", "A", "B"), c(9L, 1L, 1L, 1L, 2L, 3L, 1L, 1L, 7L,
        2L, 4L)))
      df$time = round(df$time)
      df$surv = survival::Surv(df$time, df$event)
      fit = survival::survfit(surv ~ group, data = df)
      times = sort(fit$time)
      x = summary(fit, times = times, extend = TRUE)
      a = data.frame(strata = x$strata, time = x$time, n.risk = x$n.risk, n.event = x$
        n.event, surv = x$surv)
      a %>% dplyr::filter(strata == "group=B") %>% dplyr::filter(time == 141)
    Output
         strata time n.risk n.event      surv
      1 group=B  141     15       1 0.6640625

