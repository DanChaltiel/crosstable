
## Test environments

* local R installation, R 4.2.1
* check_rhub()
* check_win_devel()


## R CMD check results

On RHub (Windows Server 2022, R-devel), there was one error about "Graphics API version mismatch" that seems to be a false positive.

Everything I could read about this problem on stackoverflow is about installation problems, 
which seems odd considering a platform such as RHub.

There are no problems locally, with other RHub platforms, or with check_win_devel().

Thank you very much.


## Comment

* No comment
