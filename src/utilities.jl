using PooledArrays: PooledArray

"""
    taggedstring(x::AbstractVector{<:Integer}, tag='S')

Convert `x` to a `PooledArray` of aligned strings with `tag` prepended.

```
julia> show(taggedstring(1:10))
["S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", "S09", "S10"]
```
"""
function taggedstring(x::AbstractVector{<:Integer}, tag='S')
    strngs = string.(x)
    PooledArray(string.(tag, lpad.(strngs, maximum(length.(strngs)), '0')))
end

nothing   # squelch any output from including this file
