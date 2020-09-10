x = [randn(10); missing] .* 10

zscore(x)

ismissing.(x)

.!ismissing.(x)

x[.!ismissing.(x)]

zscore(x[.!ismissing.(x)])

disallowmissing(x[.!ismissing.(x)])

zscore(disallowmissing(x[.!ismissing.(x)]))

x[.!ismissing.(x)] = zscore(disallowmissing(x[.!ismissing.(x)]))

x

nrow(df)
dropmissing!(df, [:column1, :column2])
nrow(df)
disallowmssing!(df, [:column1, :column2])