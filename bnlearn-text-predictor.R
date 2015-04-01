## Bayesian model for text prediction

require("bnlearn")
require("ggplot2")
require("stringr")
require("plyr")
#require('wordnet')

setwd("~/Projects/Capstone/textprediction")

### get acquainted

data(learning.test)
res = empty.graph(names(learning.test))
modelstring(res) = "[A][C][F][B|A][D|A:C][E|B:F]"
plot(res)

learn.net = empty.graph(names(learning.test))
modelstring(learn.net) = "[A][C][F][B|A][D|A:C][E|B:F]"
learn.net
