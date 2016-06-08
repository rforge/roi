## ---------------------------
## @Bash
## ---------------------------
cd /home/florian/work/Optimization/ROI/cloud
ssh -i opti.pem wucloud@137.208.57.226


## ---------------------------
## @Cloud
## ---------------------------
cd work/ROI_R-Forge/pkg

mv *.tar.gz archive/
ls

Rdevel CMD build ROI
Rdevel CMD INSTALL ROI_*.tar.gz


Rdevel CMD build ROI.tests
Rdevel CMD INSTALL ROI.tests_*.tar.gz


## install nloptr
Rdevel
install.packages("nloptr")
q("no")

## test
Rdevel
library(ROI)
library(ROI.tests)

test_solver("cplex")


