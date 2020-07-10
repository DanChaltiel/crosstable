

mtcars2 = mtcars %>% 
    mutate(vs=ifelse(vs==0, "vshaped", "straight"),
           am=ifelse(am==0, "auto", "manual")) %>% 
    mutate_at(c("cyl", "gear"), factor) %>% 
    expss::apply_labels( #I also could have used `Hmisc::label`
        mpg="Miles/(US) gallon",
        cyl="Number of cylinders",
        disp="Displacement (cu.in.)",
        hp="Gross horsepower",
        drat="Rear axle ratio",
        wt="Weight (1000 lbs)",
        qsec="1/4 mile time",
        vs="Engine",
        am="Transmission",
        gear="Number of forward gears",
        carb="Number of carburetors"
    ) %>% 
    mutate(
        hp_date = as.Date(hp , origin="2010-01-01") %>% set_label("Some nonsense date"),
        qsec_posix = as.POSIXct(qsec*3600*24 , origin="2010-01-01") %>% set_label("Date+time")
    )
as_tibble()

usethis::use_data(mtcars2, overwrite=TRUE)
