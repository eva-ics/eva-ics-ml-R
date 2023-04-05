# eva-ics-ml-R

R client for EVA ICS Machine Learning Kit

## Installation

### With devtools

In R type:

```R
install.packages("devtools", dependencies=TRUE)
library(devtools)
devtools::install_github("eva-ics/eva-ics-ml-R")
```

### Manual

In the system shell type:

```shell
git clone https://github.com/eva-ics/eva-ics-ml-R
cd eva-ics-ml-R
(
cat << EOF
install.packages("curl")
install.packages("httr")
install.packages("jsonlite")
install.packages("readr")
install.packages('.', repos=NULL, type='source', dependencies=TRUE)
EOF
) | R --no-save
```
