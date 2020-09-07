using DataFrames, MixedModels, StatsBase

# lots of things similar to other languages like R, Python and MATLAB

scalar = 2;

1 == 2

1 != 2

1 ≠ 2

θ = 3

[1, 2, 3] == [1, 2, 3]

# but important distinction between scalars and array
# _broadcasting_ with . helps

[1, 2, 3] .== [1, 2, 3]

x = [1, 2, 3]
x .== 3

log(1)
log(x)
log.(x)

# dataframes
slp = MixedModels.dataset(:sleepstudy)

# from DataFrames and StatsBase; like summary() in R
describe(slp)
describe(slp.reaction)

# extract columns
slp.reaction
slp[!, :reaction] # note the colon!

slp[!, [:reaction, :subj]]

# create new columns
slp[!, :extra] .= 3 # note the . -- this is broadcasting

# extract rows
slp[(slp.reaction .< 250) .& (slp.subj .== "S308"), :]

# note the : for "everything" also works for columns
# the ! is preferred (for reasons I don't remember at the moment)
# note view vs. non view
slp[:, :reaction] 

# from StatsBase; like scale() in R
zscore(slp.reaction)
describe(zscore(slp.reaction))

rt = slp[:, :reaction]

# help pages with ?
zscore!(rt, mean(rt), std(rt))

# split-apply-combine

groupby(slp, :subj)

combine(groupby(slp, :subj), :reaction => mean)

combine(groupby(slp, :subj), :reaction => mean)
combine(groupby(slp, [:subj, :days]), :reaction => mean)
combine(groupby(slp, :days), :reaction => mean)
combine(groupby(slp, :subj), :reaction => maximum)

using RCall
# get a variable 
# @rput
# @rget

R"summary(mtcars)"

R"""
summary(mtcars)
summary(iris)
"""

R"""
library(effects)
"""