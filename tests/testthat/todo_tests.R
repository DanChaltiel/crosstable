

# library(survival)
# aml$surv = Surv(aml$time, aml$status)
# expss::var_lab(aml$surv) = "survival"
# aml$dummy = "dummy"
# aml$dummy = factor("dummy")
# aml$dummy2 = c(rep("A",7), rep("B",8), rep(NA,8))
# # crosstable(aml, surv, times=c(0,15,30,150), followup=TRUE, test=T, effect=T)
# crosstable(aml, surv, by=x, times=c(0,15,30,150), followup=TRUE, total=T, showNA = "always", test=T, effect=T) %>% print
# crosstable(aml, surv, by=dummy, times=c(0,15,30,150), followup=TRUE, total=T, showNA = "always", test=T, effect=T)
# crosstable(aml, surv, by=dummy2, times=c(0,15,30,150), followup=TRUE, total=T, showNA = "always", test=T, effect=T)

#TODO faire des tests avec by=dummy partout !
# mtcars3$dummy = "dummy"



# crosstable(mtcars3, mpg, cyl, by=disp)
# crosstable(mtcars2, disp + hp + cyl + am ~ vs)
# crosstable(mtcars2, mpg, cyl, by=vs)
# crosstable(mtcars2, disp+hp+am~vs, funs_arg = list(dig=9))
# library(survival)
# mtcars3=mtcars2
# mtcars3$surv = Surv(mtcars3$disp, mtcars2$am=="manual")
# mtcars3$dummy = "prout"
# mtcars3$cyl3 = mtcars3$cyl==3
# crosstable(mtcars3, disp+hp+cyl+am+Surv(disp, am=="manual")~vs, times=c(100,200,400), followup=TRUE) %>% as_flextable()
# crosstable(mtcars3, disp+hp+cyl3+am+surv~vs, times=c(100,200,400), followup=TRUE, funs_arg = list(dig=9)) %>% as_flextable()
# crosstable(mtcars3, disp+hp+(cyl==3)+am+surv~vs, times=c(100,200,400), followup=TRUE, funs_arg = list(dig=9), test=T, effect=T) %>% as_flextable()
# crosstable(mtcars3, disp+hp+cyl+am+surv~vs, times=c(100,200,400), followup=TRUE, funs_arg = list(dig=9), test=T, effect=T) %>% as_flextable()
# crosstable(mtcars3, disp+hp~vs, funs_arg = list(dig=9)) %>% as_flextable()
# biostat2::cross(dummy~vs, mtcars3)


# crosstable(mtcars2, c(mpg,disp), by=NULL, label=T, test=T) %>% as_flextable()
# crosstable(mtcars2, c(mpg,disp), by=vs, label=T, test=T) %>% as_flextable()
# crosstable(mtcars2, c(mpg,disp), by=hp, label=T, test=T) %>% as_flextable()
# crosstable(mtcars3, c(mpg,surv), by=hp, label=T, test=T) %>% as_flextable(T)



# crosstable(mtcars2, c(mpg,disp), by=vs, funs=c(mean,sd), funs_arg=list(dig=0, f=5), label=T, test=T, old=F) %>% as_flextable(T)
#
# crosstable(mtcars2, c(mpg,disp), by=NULL, funs=c(mean,sd),funs_arg=list(dig=0), label=T, test=T, old=T) %>% as_flextable()
# crosstable(mtcars2, c(mpg,disp), by=vs, funs=c(mean,sd), funs_arg=list(dig=0), label=T, test=T, effect=T, total="all", old=F) %>% as_flextable(T)
# crosstable(mtcars2, c(mpg,disp), by=NULL, funs=c(mean,sd),funs_arg=list(dig=0), label=T, test=T) %>% as_flextable(T)
# crosstable(mtcars2, c(mpg,disp), by=vs, label=T, test=T) %>% as_flextable(T)
# crosstable(mtcars2, c(mpg,disp), by=hp, label=T, test=T) %>% as_flextable(T)
# crosstable(mtcars3, c(mpg,surv), by=hp, label=T, test=T) %>% as_flextable(T)
# crosstable(mtcars2, c(mpg,disp), by=hp, label=T, test=T) %>% as_flextable(T)




# crosstable(mtcars3, surv, times=c(100,200,400), followup=TRUE) %>% as_flextable()
# crosstable(mtcars3, surv~vs, times=c(100,200,400), followup=TRUE, total="row") %>% as_flextable()
# crosstable(mtcars3, surv~vs, times=c(100,200,400), followup=TRUE, total="col") %>% as_flextable()


# crosstable_effect_args
# crosstable(mtcars2, cyl)
# biostat2::cross(cyl~., data=mtcars2, total=2)
# crosstable(mtcars3, cyl, by=vs, showNA="ifany", margin=c("row","col"), total="all") %>% ctf
# crosstable(mtcars3, cyl, by=vs, showNA="ifany", margin=c("col","row"), total="all") %>% ctf
# biostat2::cross(cyl~vs, data=mtcars3, showNA="ifany", margin=1:2, total=1:2)

# mtcars3=mtcars2
# mtcars3$cyl[1:5]=NA
# mtcars3$vs[5:12]=NA
# crosstable(mtcars3, cyl, by=vs, showNA="no", margin="col", total="all")
# crosstable(mtcars3, cyl, by=vs, showNA="ifany", margin="col", total="all")


# crosstable(mtcars3, cyl, by=vs, showNA="no", margin="col", total="row")
# biostat2::cross(cyl~vs, data=mtcars3, showNA="ifany", margin=2, total=1)
#
# crosstable(mtcars3, cyl, by=vs, showNA="no", margin="col", total="col")
# biostat2::cross(cyl~vs, data=mtcars3, showNA="ifany", margin=2, total=2)
#
# crosstable(mtcars3, cyl, by=vs, showNA="no", margin="col", total="all")
# biostat2::cross(cyl~vs, data=mtcars3, showNA="ifany", margin=2, total=TRUE)


# crosstable(iris2)
# crosstable(mtcars2)
#
# crosstable(mtcars2, mpg, disp, hp, wt, qsec, carb)
# crosstable(mtcars2, disp, hp, wt, qsec)
# crosstable(mtcars2, is.numeric)
# crosstable(mtcars2, is.numeric, by=am)
#
#
#
# iris2$Sepal.Length %>% attributes
# mtcars2$mpg %>% attributes

# mtcars2 %>% (tibble::as_tibble)
# crosstable(mtcars2, disp+qsec~hp)    #OK
# crosstable(mtcars2, disp+qsec+vs~hp) #pas OK
# crosstable(iris, XX, by="Species")
# crosstable(mtcars2, disp+hp+cyl+am~vs)
# crosstable(mtcars2, disp+hp~vs)

# library(survival)
# mtcars2$surv = Surv(mtcars2$disp, mtcars2$am=="manual")
# crosstable(mtcars2, disp+hp+cyl+am+Surv(disp, am=="manual")~vs, times=c(100,200,400), followup=TRUE)
# crosstable(mtcars2, disp+hp+cyl+am+surv~vs, times=c(100,200,400), followup=TRUE)








# # cross_args2 = list(data_x=data_x, data_y=data_y, funs=funs, funs_arg=funs_arg,
# #                    margin=margin, total=total, percent_digits=percent_digits, showNA=showNA,
# #                    cor_method=cor_method, times=times, followup=followup, test=test, test_args=test_args,
# #                    effect=effect, effect_args=effect_args, label=label)
# # saveRDS(cross_args2, "tmp/cross_args.rds", version=2)
# # rtn = do.call(cross_by, cross_args2)
