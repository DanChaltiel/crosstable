

mtcars2 = mtcars %>% 
    mutate(vs=ifelse(vs==0, "vshaped", "straight"),
           am=ifelse(am==0, "auto", "manual")) %>% 
    mutate_at(c("cyl", "gear"), factor) %>% 
    apply_labels( #I also could have used `Hmisc::label`
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
    as_tibble()

usethis::use_data(mtcars2)
