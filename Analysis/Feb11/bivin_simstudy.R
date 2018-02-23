# Describe
def <- defData(varname="n1", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n2", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n3", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n4", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n5", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n6", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n7", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n8", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n9", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="n10", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="EV1", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="EV2", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="EV3", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="EV4", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="EV5", dist="normal", formula = "0", variance = .1, link = "identity")
def <- defData(def,varname="a1", dist="binary", formula = ".1 + .1*EV1 + .2*EV2 + .3*EV3 - .4 * EV4 - .5 * EV5", link = "logit")
#Generate
data5_10 = genData(10000,def)
#Model
gfit = glm(a1~EV1 + EV2 + EV3 + EV4 + EV5 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8 + n9 + n10, family = "binomial", data = data5_10)
step(gfit)

