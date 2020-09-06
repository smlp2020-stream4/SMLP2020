# Complexity in fitting Linear Mixed Models

Linear mixed-effects models are increasingly used for the analysis of data from experiments in fields like psychology where several subjects are each exposed to each of several different items. In addition to a response, which here will be assumed to be on a continuous scale, such as a *response time*, a number of experimental conditions are systematically varied during the experiment. In the language of statistical experimental design the latter variables are called *experimental factors* whereas factors like `Subject` and `Item` are *blocking factors*. That is, these are known sources of variation that usually are not of interest by themselves but still should be accounted for when looking for systematic variation in the response.

# An example data set

The data from experiment 2 in *[Kronmueller and Barr (2007)](https://doi.org/10.1016/j.jml.2006.05.002)* are available in `.rds` (R Data Set) format in the file `kb07_exp2_rt.rds` in the [github repository](https://github.com/dalejbarr/kronmueller-barr-2007) provided by Dale Barr. Files in this format can be loaded using the *[RData](https://github.com/JuliaData/RData.jl)*[ package](https://github.com/JuliaData/RData.jl) for **[Julia](https://julialang.org)**.

[github-repository][nextjournal#github-repository#ada236b3-896e-4a2d-8a08-737bc5160868]

## Loading the data in Julia

Install some packages not part of the standard nextjournal environment. The version of `MixedModels` should be at least `v"2.1.0".`

```julia id=a6c198c1-3d0a-45db-a528-2e826b563d51
using Pkg
Pkg.add(["GLM", "MixedModels", "RData", "StatsModels"])
```

Attach the packages to be used.

```julia id=a217767d-beb7-4f13-a5d4-dd4ad4f52822
using BenchmarkTools, CSV, DataFrames, GLM, MixedModels
using RData, Statistics, StatsBase, StatsModels, StatsPlots
gr();  # plot backend
```

```julia id=825d5660-e406-4e40-8ac5-42a22a9acf8b
kb07 = load("/kronmueller-barr-2007/kb07_exp2_rt.rds")
```

[result][nextjournal#output#825d5660-e406-4e40-8ac5-42a22a9acf8b#result]

```julia id=b68a6fb0-c7e6-4783-9021-961bc0dd685b
describe(kb07)
```

[result][nextjournal#output#b68a6fb0-c7e6-4783-9021-961bc0dd685b#result]

The blocking factors are `subj` and `item` with 56 and 32 levels respectively. There are three experimental factors each with two levels: `spkr` (speaker), `prec` (precedence), and `load` (cognitive load). The response time, `rt_raw`, is measured in milliseconds. A few very large values, e.g. the maximum which is nearly 16 seconds, which could skew the results, are truncated in the `rt_trunc` column. In addition, three erroneously recorded responses (values of 300 ms.) have been dropped, resulting in a slight imbalance in the data.

A table of mean responses and standard deviations for combinations of the experimental factors, as shown in Table 3 of the paper and on the data repository can be reproduced as

```julia id=3b5d4560-3e9e-40eb-9379-bb6cd3304013
cellmeans = by(kb07, [:spkr, :prec, :load], 
  meanRT = :rt_trunc => mean, sdRT = :rt_trunc => std, n = :rt_trunc => length,
  semean = :rt_trunc => x -> std(x)/sqrt(length(x))
)
```

[result][nextjournal#output#3b5d4560-3e9e-40eb-9379-bb6cd3304013#result]

The data are slightly imbalanced because 3 unrealistically low response times were removed.

An interaction plot of the cell means shows that the main effect of `prec` is the dominant effect. (Need to fix this plot to show levels of prec in different colors/symbols.)

```julia id=264e5571-2c4e-4e71-b665-21e2bf833194
@df cellmeans scatter(:load, :meanRT, group=:spkr,
  layout=(1,2), link=:both, xlabel="Cognitive load",
  ylabel="Mean response time (ms)", label="")
```

![result][nextjournal#output#264e5571-2c4e-4e71-b665-21e2bf833194#result]

## Loading the data in R

```r id=3b61426f-22c9-4b0d-93cf-7d315305dbc0
kb07 <- readRDS("/kronmueller-barr-2007/kb07_exp2_rt.rds")
str(kb07)
```

The positions of the missing observations can be determined from

```r id=c969b50b-1f56-4b55-bac8-062bbc03f8bb
(subjitemtbl <- xtabs(~ subj + item, kb07))
```

```r id=7df4f4c7-af71-4def-9243-d2ec66fa2c87
table(subjitemtbl)
```

All of the experimental factors vary within subject and within item, as can be verified by examining the frequency tables for the experimental and grouping factors.  For example

```r id=4dc7c2eb-6317-4179-b6fb-25d686808762
xtabs(~ spkr + subj, kb07)
```

# Formulating a simple model

## Installing the required R package

For R lme4 and its dependencies must be installed

```r id=c1ee2fac-b3f1-4670-a174-1b2edc2b6e47
if (is.na(match("lme4", rownames(installed.packages())))) {
   install.packages("lme4", repos="https://cloud.R-project.org")
}
```

```r id=8f3302d5-9b53-41d8-8967-c1ac5d18ac33
require(lme4, quietly=TRUE)
```

## Formula and model for simple, scalar random effects

A simple model with main-effects for each of the experimental factors and with random effects for subject and for item is described by the formula `rt_trunc ~ 1 + spkr + prec + load + (1|subj) + (1|item)`. In the *MixedModels* package, which uses the formula specifications from the *[StatsModels](https://github.com/JuliaStats/StatsModels.jl)*[ package](https://github.com/JuliaStats/StatsModels.jl), a formula must be wrapped in a call to the `@formula` macro.  

```julia id=b75a7b72-e736-4297-841c-29f56b69ee75
f1 = @formula(rt_trunc ~ 1 + spkr + prec + load + (1|subj) + (1|item));
```

In R a formula can stand alone.

```r id=23342a51-9904-4043-bfe3-64a333d5b569
f1 <- rt_trunc ~ 1 + spkr + prec + load + (1|subj) + (1|item)
```

For the MixedModels package the model is constructed as a `LinearMixedModel` then fit with a call to fit`!`

(By convention, the names in Julia of *mutating functions*, which modify the value of one or more of their arguments, end in `!` as a warning to the user that arguments, usually just the first argument, can be overwritten with new values.)

```julia id=b5e23c28-0079-4636-a842-43372799a628
m1 = fit(MixedModel, f1, kb07)
```

The first fit of such a model can take several seconds because the Just-In-Time (JIT) compiler must analyze and compile a considerable amount of code.  (All of the code in the *MixedModels* package is Julia code.) Subsequent fits of this or similar models are much faster.

The comparable model fit with lme4 in R is

```r id=55d58b5c-98b5-415f-932d-e44cea162714
m1 <- lmer(f1, kb07, REML=FALSE)
summary(m1, corr=FALSE)
```

The estimated coefficients for the fixed-effects are different from those in the Julia fit because the reference level for `load` is different.  Conversion to a factor (CategoricalArray) is done slightly differently.

## Assigning contrasts

For two-level experimental factors, such as `prec`, `spkr` and `load`, in a (nearly) balanced design such as this it is an advantage to use a $\pm1$ encoding of the levels in the model matrix.  This is known as "assigning contrasts" in both R and Julia.

In R one assigns a contrasts specification to the factor itself.  The "Helmert" contrast type produces a $\pm1$ encoding with two levels.

```r id=46cd8179-0e68-4433-a33e-1f6d13270589
contrasts(kb07$spkr) <- contr.helmert(2)
contrasts(kb07$prec) <- contr.helmert(2)
contrasts(kb07$load) <- contr.helmert(2)
system.time(m1 <- lmer(f1, kb07, REML=FALSE))
```

```r id=93a8563c-a0d3-4364-81cd-1214abf59504
summary(m1, corr=FALSE)
```

The change in coding results in estimated coefficients (and standard errors) for the experimental factors being half the previous estimate, because of the $\pm1$coding.  Furthermore, the (`Intercept`) estimate now is now a "typical" response over all the conditions.  It would be the sample mean if the experiment were completely balanced.

```r id=7103d3b3-42a9-4308-badf-74833561f38b
mean(kb07$rt_trunc)
```

In Julia the contrasts are specified in a dictionary that is passed as an argument to the model constructor.

```julia id=0f7391b1-290a-4039-8756-4b56defd5afd
const HC = HelmertCoding();
const contrasts = Dict(:spkr => HC, :prec => HC, :load=> HC);
```

```julia id=ac02350d-fa34-4551-8d53-236b9e2fb22a
m1 = fit(MixedModel, f1, kb07, contrasts=contrasts)
```

The model matrix for the fixed effects is

```julia id=1eb9a6f4-9874-489a-a87a-ee092ab1d873
Int.(m1.X)    # convert to integers for cleaner printing
```

An advantage of the $\pm1$ encoding is that$X'X$is nearly a multiple of the identity matrix.  Furthermore, any interactions of these two-level factors will also have an $\pm1$ encoding.

```julia id=4956ff06-f8c9-4e3c-a812-386c5901425e
Int.(m1.X'm1.X)
```

A benchmark of this fit shows that it is quite fast - on the order of a few milliseconds.

```julia id=662a5f1d-d9f3-479d-a19e-0b1301dcc44a
m1bmk = @benchmark fit(MixedModel, $f1, $kb07, contrasts = $contrasts)
```

# Model construction versus model optimization

The `m1` object is created in the call to the constructor function, `LinearMixedModel`, then the parameters are optimized or fit in the call to `fit!`. Usually the process of fitting a model will take longer than creating the numerical representation but, for simple models like this, the creation time can be a significant portion of the overall running time.

```julia id=57e003b2-8097-413e-8038-5efb71a78fac
bm1construct = @benchmark LinearMixedModel($f1, $kb07, contrasts=$contrasts)
```

```julia id=4b8855ef-d1fc-4808-8849-3039ede2229a
bm1fit = @benchmark fit!($m1)
```

# Factors affecting the time to optimize the parameters

The optimization process is summarized in the `optsum` property of the model.

```julia id=383cf486-0d79-4916-be9c-7e38bb6a34cf
m1.optsum
```

For this model there are two parameters to be optimized because the objective function, negative twice the log-likelihood, can be *profiled* with respect to all the other parameters. (See section 3 of *[Bates et al. 2015](https://www.jstatsoft.org/article/view/v067i01)* for details.) Both these parameters must be non-negative (i.e. both have a lower bound of zero) and both have an initial value of one. After 28 function evaluations an optimum is declared according to the function value tolerance, either $10^{-8}$ in absolute terms or $10^{-12}$ relative to the current value.

The optimization itself has a certain amount of setup and summary time but the majority of the time is spent in the evaluation of the objective - the profiled log-likelihood.

Each function evaluation is of the form

```julia id=acb04b8e-5d64-4214-b8f4-d3a4ba347304
θ1 = m1.θ;
m1objective = @benchmark objective(updateL!(setθ!($m1, $θ1)))
```

On this machine 28 function evaluations, each taking around 60 microseconds, gives the total function evaluation time of at least 1.7 ms., which is practically all of the time to fit the model.

The majority of the time for the function evaluation for this model is in the call to `updateL!`

```julia id=226ff3f2-cb95-47e0-9a99-8006f40e9c8d
m1update = @benchmark updateL!($m1)
```

This is an operation that updates the lower Cholesky factor (often written as `L`) of a blocked sparse matrix.

There are 4 rows and columns of blocks. The first row and column correspond to the random effects for subject, the second to the random effects for item, the third to the fixed-effects parameters and the fourth to the response. Their sizes and types are

```julia id=058438f9-5c52-45e7-a089-cbf79da9a142
describeblocks(m1)
```

There are two lower-triangular blocked matrices in the model representation: `A` with fixed entries determined by the model and data, and `L` which is updated for each evaluation of the objective function. The type of the `A` block is given before the size and the type of the `L` block is after the size. For scalar random effects, generated by a random-effects term like `(1|G)`, the (1,1) block is always diagonal in both `A` and `L`. Its size is the number of levels of the grouping factor, `G`.

Because subject and item are crossed, the (2,1) block of `A` is dense, as is the (2,1) block of `L`. The (2,2) block of `A` is diagonal because, like the (1,1) block, it is generated from a scalar random effects term. However, the (2,2) block of `L` ends up being dense as a result of "fill-in" in the sparse Cholesky factorization. All the blocks associated with the fixed-effects or the response are stored as dense matrices but their dimensions are (relatively) small.

# Increasing the model complexity

In general, adding more terms to a model will increase the time required to fit the model. However, there is a big difference between adding fixed-effects terms and adding complexity to the random effects.

## Increasing the complexity of the fixed effects

Adding the two- and three-factor interactions to the fixed-effects terms increases the time required to fit the model.

```julia id=e9700f1c-70fb-4948-8852-ae3c414e3350
f2 = @formula(rt_trunc ~ 1 + spkr*prec*load + (1|subj) + (1|item));
```

```julia id=27f9620e-a1c2-4129-a822-e7ad29b59a5d
m2 = fit(MixedModel, f2, kb07, contrasts=contrasts)
```

[result][nextjournal#output#27f9620e-a1c2-4129-a822-e7ad29b59a5d#result]

(Notice that none of the interactions are statistically significant.)

```julia id=9ed9385d-c394-44ad-b75d-493478a4d2d4
m2bmk = @benchmark fit(MixedModel, $f2, $kb07,contrasts=$contrasts)
```

In this case, the increase in fitting time is more because the number of function evaluations to determine the optimum increases than because of increased evaluation time for the objective function.

```julia id=6f9a7d92-5b47-44e8-b173-e06dcdb1fe6e
m2.optsum.feval
```

```julia id=fb57bcef-00e9-4e5c-baaa-d73e65a4a99a
θ2 = m2.θ;
m2objective = @benchmark objective(updateL!(setθ!($m2, $θ2)))
```

```r id=86500356-654e-4040-8be4-0cc8be8b69a5
f2 <- rt_trunc ~ 1 + spkr*prec*load + (1|subj) + (1|item)
system.time(m2 <- lmer(f2, kb07, REML=FALSE))
```

```r id=fcf82605-4553-4ea1-9d73-4c6139104345
summary(m2, corr=FALSE)
```

## Increasing complexity of the random effects

Another way in which the model can be extended is to switch to vector-valued random effects. Sometimes this is described as having *random slopes*, so that a subject not only brings their own shift in the typical response but also their own shift in the change due to, say, `Load` versus `No Load`. Instead of just one, scalar, change associated with each subject there is an entire vector of changes in the coefficients.

A model with a random slopes for each of the experimental factors for both subject and item is specified as

```julia id=486d81b1-7773-41f9-a4f7-78b24cc9e941
f3 = @formula(
  rt_trunc ~ 1 + spkr*prec*load + (1+spkr+prec+load|subj) +
    (1+spkr+prec+load|item)
  );
```

```julia id=50476044-96c5-4d0d-ae94-af11952e43ad
m3 = fit(MixedModel, f3, kb07, contrasts=contrasts)
```

[result][nextjournal#output#50476044-96c5-4d0d-ae94-af11952e43ad#result]

```julia id=04bcef1b-663c-4c48-a770-c30b4826d608
m3bmk = @benchmark fit(MixedModel, $f3, $kb07, contrasts=$contrasts)
```

```r id=79ed4770-8e37-42f2-b352-dab04afc60c8
f3 <- 
  rt_trunc ~ 1 + spkr*prec*load + (1+spkr+prec+load|subj) + 
		(1+spkr+prec+load|item)
system.time(m3 <- lmer(f3, kb07, REML=FALSE,
                       control=lmerControl(calc.derivs=FALSE)))
```

```r id=6861176a-0345-43cd-89ad-80b444951844
summary(m3, corr=FALSE)
```

There are several interesting aspects of this model fit.

First, the number of parameters optimized directly has increased substantially. What was previously a 2-dimensional optimization has now become 20 dimensional.

```julia id=787a1964-28f4-417f-a094-9ced14148b8e
m3.optsum
```

[result][nextjournal#output#787a1964-28f4-417f-a094-9ced14148b8e#result]

and the number of function evaluations to convergence has gone from under 40 to over 600.

The time required for each function evaluation has also increased considerably,

```julia id=2ee69c0d-cd7a-490f-89b2-c2552ac3292d
θ3 = m3.θ;
m3objective = @benchmark objective(updateL!(setθ!($m3, $θ3)))
```

resulting in much longer times for model fitting - about three-quarters of a second in Julia and over 6 seconds in R.

Notice that the estimates of the fixed-effects coefficients and their standard errors have not changed substantially except for the standard error of `prec`, which is also the largest effect.

The parameters in the optimization for this model can be arranged as two lower-triangular 4 by 4 matrices.

```julia id=7cc502a0-7696-4996-b2db-9e6701f33514
m3.λ[1]
```

```julia id=b1cf832d-4e1b-451a-a132-c69294a20c02
m3.λ[2]
```

which generate the covariance matrices for the random effects. The cumulative proportion of the variance in the principal components of these covariance matrices, available as

```julia id=15d7379c-57fc-4d52-8c18-1481fda27a3a
m3.rePCA
```

show that 93% of the variation in the random effects for subject is in the first principal direction and 99% in the first two principal directions. The random effects for item also have 99% of the variation in the first two principal directions.

Furthermore the estimates of the standard deviations of the "slope" random effects are much smaller than the those of the intercept random effects except for the `prec` coefficient random effect for `item`, which suggests that the model could be reduced to `rt_trunc ~ 1 + spkr*prec*load + (1|subj) + (1+prec|item)` or even `rt_trunc ~ 1 + spkr+prec+load + (1|subj) + (1+prec|item)`.

```julia id=8a69369e-7364-488e-8c0b-14316e582c52
f4 = @formula(rt_trunc ~ 1 + spkr+prec+load + (1|subj) + (1+prec|item));
```

```julia id=41a58fb3-3c29-4427-a3dc-36e4dd74e029
m4 = fit(MixedModel, f4, kb07, contrasts=contrasts)
```

[result][nextjournal#output#41a58fb3-3c29-4427-a3dc-36e4dd74e029#result]

```julia id=4bb28a01-bd5a-40b6-a409-24e7afc420a1
m4bmk = @benchmark fit(MixedModel, $f4, $kb07, contrasts=$contrasts)
```

```julia id=49e90bc7-cf24-4469-8d5f-e255d4907367
m4.optsum.feval
```

```r id=32340782-4866-481c-b69d-8e847e88f94f
f4 <- rt_trunc ~ 1 + spkr+prec+load + (1+prec|item) + (1|subj)
system.time(m4 <- lmer(f4, kb07, REML=FALSE,  control=lmerControl(calc.derivs=FALSE)))
```

```r id=04a92ba7-9e3a-41d4-ae6c-cab893059ab2
summary(m4, corr=FALSE)
```

```julia id=c3714a60-9b60-4d1d-b986-2d4dcef0dd12
θ4 = m4.θ;
m4objective = @benchmark objective(updateL!(setθ!($m4, $θ4)))
```

These two model fits can be compared with one of the information criteria, `AIC` or `BIC`, for which "smaller is better". They both indicate a preference for the smaller model, `m4`.

These criteria are values of the objective, negative twice the log-likelihood at convergence, plus a penalty that depends on the number of parameters being estimated.

Because model `m4` is a special case of model `m3`, a likelihood ratio test could also be used. The alternative model, `m3`, will always produce an objective that is less than or equal to that from the null model, `m4`. The difference in these value is similar to the change in the residual sum of squares in a linear model fit. This objective would be called the *deviance* if there was a way of defining a saturated model but it is not clear what this should be. However, if there was a way to define a deviance then the difference in the deviances would be the same as the differences in these objectives, which is

```julia id=d488b9dc-29f8-48be-8ea9-f55a6cf34c6f
diff(objective.([m3, m4]))
```

This difference is compared to a $\chi^2$ distribution with degrees of freedom corresponding to the difference in the number of parameters

```julia id=9516ad28-2fe3-4949-940c-3c414a820c2a
diff(dof.([m4, m3]))
```

producing a p-value of about 14%.

```r id=d3119341-f167-43e5-bd5f-7a77908e09be
pchisq(26.7108, 20, lower.tail=FALSE)
```

## Going maximal

The "maximal" model as proposed by *[Barr et al., 2013](https://www.sciencedirect.com/science/article/pii/S0749596X12001180)* would include all possible interactions of experimental and grouping factors.

```julia id=2a3d112e-0e21-40c9-a11e-7421fa2d4d8e
f5 = @formula(rt_trunc ~ 1 + spkr*prec*load + (1+spkr*prec*load|subj) +      (1+spkr*prec*load|item));
```

```julia id=6ed09133-6f69-484b-aba5-0b7df5c17e22
m5 = fit(MixedModel, f5, kb07, contrasts=contrasts)
```

[result][nextjournal#output#6ed09133-6f69-484b-aba5-0b7df5c17e22#result]

```julia id=99678241-e57e-44e8-a00c-5fbec0307b21
m5bmk = @benchmark fit(MixedModel, $f5, $kb07, contrasts=$contrasts)
```

As is common in models with high-dimensional vector-valued random effects, the dominant portion of the variation is in the first few principal components

```julia id=4a336cba-52f2-4a5f-9575-5732a34b5af1
m5.rePCA
```

For both the subjects and the items practically all the variation of these 8-dimensional random effects is in the first 4 principal components.

The dimension of $\theta$, the parameters in the optimization, increases considerably

```julia id=62b06fb4-11e2-444c-8146-ade3a5bbfd14
θ5 = m5.θ;
length(θ5)
```

Of these 72 parameters, 36 are estimated from variation between items, yet there are only 32 items.

Because the dimension of the optimization problem has gotten much larger the number of function evaluations to convergence increases correspondingly.

```julia id=9d4d4eb2-795a-4744-87f6-cf21322a32ee
m5.optsum.feval
```

Also, each function evaluation requires more time

```julia id=8acdc87b-8821-4631-816e-8b5f7bc572f0
m5objective = @benchmark objective(updateL!(setθ!($m5, $θ5)))
```

almost all of which is for the call to `updateL!`.

```julia id=472ac8c0-f230-4cc4-987f-9bd8ae69bb37
@benchmark updateL!($m5)
```

This model takes a long time to fit using lme4.

```r id=5de55af1-4d1c-406e-9165-41a34fc31dc9
f5 <- rt_trunc ~ 1 + spkr*prec*load + (1+spkr*prec*load|subj) +
    (1+spkr*prec*load|item)
system.time(m5 <- lmer(f5, kb07, REML=FALSE,        control=lmerControl(calc.derivs=FALSE)))
```

```r id=60bb1d90-ffda-48b1-a357-f5f304c3229d
summary(m5, corr=FALSE)
```

## A model of intermediate complexity

To provide more granularity in the plots of execution time shown below, fit one more model without random effects for the third-order interaction of the experimental factors.

```julia id=576cb7e7-6174-44ee-8ee5-e1d04e91b3b6
f6 = @formula(rt_trunc ~ 1 + spkr*prec*load + 
    (1+spkr+prec+load+spkr&prec+spkr&load+prec&load|subj) + 
    (1+spkr+prec+load+spkr&prec+spkr&load+prec&load|item));
```

```julia id=d2a0b78f-ad03-47e3-96ba-2c8cf968474b
m6 = fit(MixedModel, f6, kb07, contrasts=contrasts)
```

[result][nextjournal#output#d2a0b78f-ad03-47e3-96ba-2c8cf968474b#result]

```julia id=094bdb87-c6a5-4429-b98f-0653a4a9f99f
m6bmk = @benchmark fit(MixedModel, $f6, $kb07, contrasts=$contrasts)
```

```julia id=a00a2dc7-7f49-4086-bc57-0faf83a8b220
θ6 = m6.θ;
length(θ6)
```

```julia id=7a76c506-c7f7-42bd-81d1-d5480d0bcee5
m6objective = @benchmark objective(updateL!(setθ!($m6, $θ6)))
```

```julia id=28c77c0c-8b06-467b-a080-95aaa7f2853b
@benchmark updateL!($m6)
```

# tSummary of goodness of fit

Apply the goodness of fit measures to `m1` to `m6` creating a data frame

```julia id=2c25ebdb-19e7-47e2-8b20-27c5cfb0e633
const mods = [m1, m2, m3, m4, m5, m6];
gofsumry = DataFrame(dof=dof.(mods), deviance=deviance.(mods),
    AIC = aic.(mods), AICc = aicc.(mods), BIC = bic.(mods))
```

Here `dof` or degrees of freedom is the total number of parameters estimated in the model and `deviance` is simply negative twice the log-likelihood at convergence, without a correction for a saturated model.  All the information criteria are on a scale of "smaller is better" and all would select model 4 as "best".

```julia id=b7b0245b-5bc7-43d5-88b3-2b05c794ca46
map(nm -> argmin(getproperty(gofsumry,nm)), (AIC=:AIC, AICc=:AICc, BIC=:BIC))
```

The benchmark times are recorded in nanoseconds. The median time in seconds to fit each model is evaluated as

```julia id=bd5b6f70-3a07-4e76-9215-d0bd119e8235
fits = [median(b.times) for b in [m1bmk, m2bmk, m3bmk, m4bmk, m5bmk, m6bmk]] ./ 10^9;
```

```julia id=7c67a088-7186-46f3-9117-1a524f718a86
nfe(m) = length(coef(m));
nre1(m) = MixedModels.vsize(first(m.reterms));
nlv1(m) = MixedModels.nlevs(first(m.reterms));
nre2(m) = MixedModels.vsize(last(m.reterms));
nlv2(m) = MixedModels.nlevs(last(m.reterms));
nθ(m) = sum(MixedModels.nθ, m.reterms);
nev = [m.optsum.feval for m in mods];
dimsumry = DataFrame(p = nfe.(mods), q1 = nre1.(mods), n1 = nlv1.(mods),
    q2 = nre2.(mods), n2 = nlv2.(mods), nθ = nθ.(mods),
    npar = dof.(mods), nev = nev, fitsec = fits)
```

[result][nextjournal#output#7c67a088-7186-46f3-9117-1a524f718a86#result]

In this table, `p` is the dimension of the fixed-effects vector, `q1` is the dimension of the random-effects for each of the `n1` levels of the first grouping factor while `q2` and `n2` are similar for the second grouping factor. `nθ` is the dimension of the parameter vector and `npar` is the total number of parameters being estimated, and is equal to `nθ + p + 1`.

`nev` is the number of function evaluations to convergence. Because this number will depend on the number of parameters in the model and several other factors such as the setting of the convergence criteria, only its magnitude should be considered reproducible. As described above,` fitsec` is the median time, in seconds, to fit the model.

# Relationships between model complexity and fitting time

As would be expected, the time required to fit a model increases with the complexity of the model. In particular, the number of function evaluations to convergence increases with the number of parameters being optimized.

```julia id=091f6473-04a0-4731-8e37-250e341b61a8
@df dimsumry scatter(:nθ, :nev, xlabel="# of parameters in the optimization",
    ylabel="Function evaluations to convergence", label="")
```

![result][nextjournal#output#091f6473-04a0-4731-8e37-250e341b61a8#result]

The relationship is more-or-less linear, based on this small sample.

Finally, consider the time to fit the model versus the number of parameters in the optimization. On a log-log scale these form a reasonably straight line.

```julia id=5d5f2226-9bc3-48b8-a872-c787ee864033
@df dimsumry scatter(:nθ, :fitsec, xlabel="# of parameters in the optimization",
  ylabel = "Median benchmark time (sec) to fit model", label="", yscale=:log10,
  xscale=:log10)
```

![result][nextjournal#output#5d5f2226-9bc3-48b8-a872-c787ee864033#result]

with a slope of about 2.2

```julia id=25ccfc8a-fbe6-4b55-a36b-1487e84c83b8
coef(lm(@formula(log(fitsec) ~ 1 + log(nθ)), dimsumry))
```

or close to a quadratic relationship between time to fit and the number of parameters in the optimization. However, the number of parameters to optimize is itself quadratic in the dimension of the random effects vector in each random-effects term. Thus, increasing the dimension of the vector-valued random effects can cause a considerable increase in the amount of time required to fit the model.

Furthermore, the estimated covariance matrix for high-dimensional vector-valued random effects is singular in `m3`, `m5` and `m6`, as is often the case.

[nextjournal#github-repository#ada236b3-896e-4a2d-8a08-737bc5160868]:
<https://github.com/dalejbarr/kronmueller-barr-2007>

[nextjournal#output#825d5660-e406-4e40-8ac5-42a22a9acf8b#result]:
<https://nextjournal.com/data/QmWMEEhqqYqAT51fmSWf7xyudxV5MrGGYwAaFApgpFx7t1?content-type=application/vnd.nextjournal.html%2Bhtml>

[nextjournal#output#b68a6fb0-c7e6-4783-9021-961bc0dd685b#result]:
<https://nextjournal.com/data/QmbLxbLpWJLvx7J3gqc8MSRbpD2kkhZ7swJYPzfTBwYV6e?content-type=application/vnd.nextjournal.html%2Bhtml>

[nextjournal#output#3b5d4560-3e9e-40eb-9379-bb6cd3304013#result]:
<https://nextjournal.com/data/QmZ4pjzDbLSKV7TCtuPaVnMnxK1pgSrEXj6kXJbvtmMri3?content-type=application/vnd.nextjournal.html%2Bhtml>

[nextjournal#output#264e5571-2c4e-4e71-b665-21e2bf833194#result]:
<https://nextjournal.com/data/QmNc8wEgfaFhN4Na6BcdC18yZ4zScJLeACmq7hqanA58fu?content-type=image/svg%2Bxml>

[nextjournal#output#27f9620e-a1c2-4129-a822-e7ad29b59a5d#result]:
<https://nextjournal.com/data/QmPCiz486jLePhjjHgV5LLi3W9QQNe89BSAUKpSNYYWGvb?content-type=text/plain>

[nextjournal#output#50476044-96c5-4d0d-ae94-af11952e43ad#result]:
<https://nextjournal.com/data/QmSitb14fUGH1ryQhVy7oHo2hN2S42z7RZTDXeh87Bs3S7?content-type=text/plain>

[nextjournal#output#787a1964-28f4-417f-a094-9ced14148b8e#result]:
<https://nextjournal.com/data/QmRCmyiU9U26f4FxJcXwwW1Znui94Ws91MiiRkxZWPTkEn?content-type=text/plain>

[nextjournal#output#41a58fb3-3c29-4427-a3dc-36e4dd74e029#result]:
<https://nextjournal.com/data/QmWvxg9JcELEUznhbw8UmzvFMRVFr3J5NtYvf85B4xWj4B?content-type=text/plain>

[nextjournal#output#6ed09133-6f69-484b-aba5-0b7df5c17e22#result]:
<https://nextjournal.com/data/QmXmdJX2SQUKbKj6CLqFQttLAK9BpA9pzFujHVJD5LBcZY?content-type=text/plain>

[nextjournal#output#d2a0b78f-ad03-47e3-96ba-2c8cf968474b#result]:
<https://nextjournal.com/data/QmX3LHPXXrUjGFijnTg81oDZdumeuafdA2LTyZnVGFf6bk?content-type=text/plain>

[nextjournal#output#7c67a088-7186-46f3-9117-1a524f718a86#result]:
<https://nextjournal.com/data/Qmerg7ivedGjkrH2du61qeuquifoVAzfqvJKVwk9UdkeKc?content-type=application/vnd.nextjournal.html%2Bhtml>

[nextjournal#output#091f6473-04a0-4731-8e37-250e341b61a8#result]:
<https://nextjournal.com/data/QmYKVnt5DpgSjnvZLeBGR1sL4rF4VzjesesLp6RTKoyoj4?content-type=image/svg%2Bxml>

[nextjournal#output#5d5f2226-9bc3-48b8-a872-c787ee864033#result]:
<https://nextjournal.com/data/QmScpS8NJ2UmWDv8XemqMaZ417e8zdnYn5LQV6vnFWwZyA?content-type=image/svg%2Bxml>

<details id="com.nextjournal.article">
<summary>This notebook was exported from <a href="https://nextjournal.com/a/L91CNS36QA8FjAkrRQzsj?change-id=CjePCppangWjJvnj6fHRE6">https://nextjournal.com/a/L91CNS36QA8FjAkrRQzsj?change-id=CjePCppangWjJvnj6fHRE6</a></summary>

```edn nextjournal-metadata
{:article
 {:settings {:numbered? true, :authors? true},
  :nodes
  {"04a92ba7-9e3a-41d4-ae6c-cab893059ab2"
   {:compute-ref #uuid "0faef330-0bfd-4162-b7fa-05fb12365514",
    :exec-duration 574,
    :id "04a92ba7-9e3a-41d4-ae6c-cab893059ab2",
    :kind "code",
    :output-log-lines {:stdout 27},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "04bcef1b-663c-4c48-a770-c30b4826d608"
   {:compute-ref #uuid "f9f9824b-a142-4239-b1b4-0e0dd4f5ab5b",
    :exec-duration 16791,
    :id "04bcef1b-663c-4c48-a770-c30b4826d608",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "058438f9-5c52-45e7-a089-cbf79da9a142"
   {:compute-ref #uuid "033a6707-f75f-451f-9736-c99096e0f937",
    :exec-duration 596,
    :id "058438f9-5c52-45e7-a089-cbf79da9a142",
    :kind "code",
    :output-log-lines {:stdout 10},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "091f6473-04a0-4731-8e37-250e341b61a8"
   {:compute-ref #uuid "059a560c-6c98-4e43-bf33-76f44d83cbb1",
    :exec-duration 527,
    :id "091f6473-04a0-4731-8e37-250e341b61a8",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "094bdb87-c6a5-4429-b98f-0653a4a9f99f"
   {:compute-ref #uuid "45acf7db-ff53-4a25-81b2-7b9dfbc7c5b1",
    :exec-duration 29922,
    :id "094bdb87-c6a5-4429-b98f-0653a4a9f99f",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "0f7391b1-290a-4039-8756-4b56defd5afd"
   {:compute-ref #uuid "6d7b65ff-8b75-474c-9b41-de9b924b3970",
    :exec-duration 436,
    :id "0f7391b1-290a-4039-8756-4b56defd5afd",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "15d7379c-57fc-4d52-8c18-1481fda27a3a"
   {:compute-ref #uuid "5da83389-1313-46cd-a2ea-ac63499c1fab",
    :exec-duration 10,
    :id "15d7379c-57fc-4d52-8c18-1481fda27a3a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "17b2e443-e360-4cf7-8617-f066c10a0af1"
   {:environment
    [:environment
     {:article/nextjournal.id
      #uuid "5b45e6f7-fe51-488a-b89b-1c8f74dfb387",
      :change/nextjournal.id
      #uuid "5cc978ff-5661-4b40-ae9c-a11ad582e02f",
      :node/id "4e307b44-52c3-4f79-9f3d-23047859e2fa"}],
    :environment? true,
    :id "17b2e443-e360-4cf7-8617-f066c10a0af1",
    :kind "runtime",
    :language "r",
    :name "lme4",
    :type :nextjournal,
    :docker/environment-image
    "docker.nextjournal.com/environment@sha256:a3b4ee5e46dc8092224cd51424d4093ebc3143720c52a08565816824e35da5fd",
    :runtime/mounts
    [{:src [:node "ada236b3-896e-4a2d-8a08-737bc5160868"],
      :dest "/kronmueller-barr-2007"}]},
   "1eb9a6f4-9874-489a-a87a-ee092ab1d873"
   {:compute-ref #uuid "ccfa7f2c-9329-415c-b75a-880478131143",
    :exec-duration 13,
    :id "1eb9a6f4-9874-489a-a87a-ee092ab1d873",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "226ff3f2-cb95-47e0-9a99-8006f40e9c8d"
   {:compute-ref #uuid "6ca6bcd0-8c7d-4232-b4a2-b57f85e08e67",
    :exec-duration 8332,
    :id "226ff3f2-cb95-47e0-9a99-8006f40e9c8d",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "23342a51-9904-4043-bfe3-64a333d5b569"
   {:compute-ref #uuid "e962bdb3-4e35-4d9b-ae92-f498e822eb72",
    :exec-duration 215,
    :id "23342a51-9904-4043-bfe3-64a333d5b569",
    :kind "code",
    :output-log-lines {:stdout 0},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "25ccfc8a-fbe6-4b55-a36b-1487e84c83b8"
   {:compute-ref #uuid "128b7c62-6016-4bfd-a691-80959001f7c9",
    :exec-duration 1519,
    :id "25ccfc8a-fbe6-4b55-a36b-1487e84c83b8",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "264e5571-2c4e-4e71-b665-21e2bf833194"
   {:compute-ref #uuid "930a2985-80bf-483c-a214-bf3d2f8e7b3d",
    :exec-duration 547,
    :id "264e5571-2c4e-4e71-b665-21e2bf833194",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "27f9620e-a1c2-4129-a822-e7ad29b59a5d"
   {:compute-ref #uuid "39b68b24-15ac-4bbe-8d01-c9ebac530763",
    :exec-duration 708,
    :id "27f9620e-a1c2-4129-a822-e7ad29b59a5d",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "28c77c0c-8b06-467b-a080-95aaa7f2853b"
   {:compute-ref #uuid "3fd1e06e-97cc-4184-b521-7171209f9949",
    :exec-duration 12982,
    :id "28c77c0c-8b06-467b-a080-95aaa7f2853b",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "2a3d112e-0e21-40c9-a11e-7421fa2d4d8e"
   {:compute-ref #uuid "514b30e7-866d-476f-814c-3a98076bca29",
    :exec-duration 429,
    :id "2a3d112e-0e21-40c9-a11e-7421fa2d4d8e",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "2c25ebdb-19e7-47e2-8b20-27c5cfb0e633"
   {:compute-ref #uuid "cd9d4c03-554b-4da4-9b68-21631d506536",
    :exec-duration 508,
    :id "2c25ebdb-19e7-47e2-8b20-27c5cfb0e633",
    :kind "code",
    :output-log-lines {:stdout 1},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "2ee69c0d-cd7a-490f-89b2-c2552ac3292d"
   {:compute-ref #uuid "62830f8d-414d-46e6-9d11-d74e0ce9291a",
    :exec-duration 13014,
    :id "2ee69c0d-cd7a-490f-89b2-c2552ac3292d",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "32340782-4866-481c-b69d-8e847e88f94f"
   {:compute-ref #uuid "3f270389-ecf0-4e15-916f-ca84eaeb07b5",
    :exec-duration 1046,
    :id "32340782-4866-481c-b69d-8e847e88f94f",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "383cf486-0d79-4916-be9c-7e38bb6a34cf"
   {:compute-ref #uuid "ea91bab2-b62f-4a38-bf8c-9bd92ff959fd",
    :exec-duration 24,
    :id "383cf486-0d79-4916-be9c-7e38bb6a34cf",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "3b5d4560-3e9e-40eb-9379-bb6cd3304013"
   {:compute-ref #uuid "0579df16-3d44-41f4-b47d-b00569ba148e",
    :exec-duration 504,
    :id "3b5d4560-3e9e-40eb-9379-bb6cd3304013",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "3b61426f-22c9-4b0d-93cf-7d315305dbc0"
   {:compute-ref #uuid "808130b3-206d-4e01-80ae-b0b816d5cacb",
    :exec-duration 527,
    :id "3b61426f-22c9-4b0d-93cf-7d315305dbc0",
    :kind "code",
    :output-log-lines {:stdout 8},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "41a58fb3-3c29-4427-a3dc-36e4dd74e029"
   {:compute-ref #uuid "7bba9724-cb25-4a76-9bc5-b92893fad2b4",
    :exec-duration 804,
    :id "41a58fb3-3c29-4427-a3dc-36e4dd74e029",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "46cd8179-0e68-4433-a33e-1f6d13270589"
   {:compute-ref #uuid "c08f735a-c60f-4846-9606-c2ae506cb325",
    :exec-duration 734,
    :id "46cd8179-0e68-4433-a33e-1f6d13270589",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "472ac8c0-f230-4cc4-987f-9bd8ae69bb37"
   {:compute-ref #uuid "e88da4b5-861e-49c9-8fb9-77c61ceb4160",
    :exec-duration 12872,
    :id "472ac8c0-f230-4cc4-987f-9bd8ae69bb37",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "486d81b1-7773-41f9-a4f7-78b24cc9e941"
   {:compute-ref #uuid "242502b8-617d-4e49-bf05-439fd4677e55",
    :exec-duration 453,
    :id "486d81b1-7773-41f9-a4f7-78b24cc9e941",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "4956ff06-f8c9-4e3c-a812-386c5901425e"
   {:compute-ref #uuid "70012eb7-bc06-440b-bcf6-7614679c911d",
    :exec-duration 6,
    :id "4956ff06-f8c9-4e3c-a812-386c5901425e",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "49e90bc7-cf24-4469-8d5f-e255d4907367"
   {:compute-ref #uuid "ab672ef5-795d-4cae-b207-e90045a26bbc",
    :exec-duration 18,
    :id "49e90bc7-cf24-4469-8d5f-e255d4907367",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "4a336cba-52f2-4a5f-9575-5732a34b5af1"
   {:compute-ref #uuid "108bff51-7af5-4a97-b955-9fb5d16aa485",
    :exec-duration 17,
    :id "4a336cba-52f2-4a5f-9575-5732a34b5af1",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "4b8855ef-d1fc-4808-8849-3039ede2229a"
   {:compute-ref #uuid "e2d14c5a-f752-4df3-8cbc-22783077bed5",
    :exec-duration 12789,
    :id "4b8855ef-d1fc-4808-8849-3039ede2229a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "4bb28a01-bd5a-40b6-a409-24e7afc420a1"
   {:compute-ref #uuid "00d96965-59b0-4c39-8549-d6db70ff1c87",
    :exec-duration 12812,
    :id "4bb28a01-bd5a-40b6-a409-24e7afc420a1",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "4dc7c2eb-6317-4179-b6fb-25d686808762"
   {:compute-ref #uuid "a8b32a54-d7a8-43a2-b82b-a806927e895d",
    :exec-duration 391,
    :id "4dc7c2eb-6317-4179-b6fb-25d686808762",
    :kind "code",
    :output-log-lines {:stdout 12},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"],
    :stdout-collapsed? false},
   "50476044-96c5-4d0d-ae94-af11952e43ad"
   {:compute-ref #uuid "1ad2e29a-2ba4-47c3-a149-92d950fc37fb",
    :exec-duration 1451,
    :id "50476044-96c5-4d0d-ae94-af11952e43ad",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "55d58b5c-98b5-415f-932d-e44cea162714"
   {:compute-ref #uuid "4ddc7d4d-854e-4ae8-a05c-9509590e910e",
    :exec-duration 578,
    :id "55d58b5c-98b5-415f-932d-e44cea162714",
    :kind "code",
    :output-log-lines {:stdout 24},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "576cb7e7-6174-44ee-8ee5-e1d04e91b3b6"
   {:compute-ref #uuid "245e4b54-8c24-4ce1-a704-f6a91a779af5",
    :exec-duration 452,
    :id "576cb7e7-6174-44ee-8ee5-e1d04e91b3b6",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "57e003b2-8097-413e-8038-5efb71a78fac"
   {:compute-ref #uuid "3e8db754-e7bc-44b5-bd93-49d02f189af6",
    :exec-duration 12878,
    :id "57e003b2-8097-413e-8038-5efb71a78fac",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "5d5f2226-9bc3-48b8-a872-c787ee864033"
   {:compute-ref #uuid "ba6188bf-84b6-450a-bd36-9d40178c7e7a",
    :exec-duration 540,
    :id "5d5f2226-9bc3-48b8-a872-c787ee864033",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "5de55af1-4d1c-406e-9165-41a34fc31dc9"
   {:compute-ref #uuid "343a022b-b9de-4b8c-afea-223f1c08cfb3",
    :exec-duration 118024,
    :id "5de55af1-4d1c-406e-9165-41a34fc31dc9",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "60bb1d90-ffda-48b1-a357-f5f304c3229d"
   {:compute-ref #uuid "93e734ab-6753-434e-9fff-a167b6a489d7",
    :exec-duration 599,
    :id "60bb1d90-ffda-48b1-a357-f5f304c3229d",
    :kind "code",
    :output-log-lines {:stdout 62},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "62b06fb4-11e2-444c-8146-ade3a5bbfd14"
   {:compute-ref #uuid "b16529c2-4caa-4c9b-91c7-bd1e6986b979",
    :exec-duration 20,
    :id "62b06fb4-11e2-444c-8146-ade3a5bbfd14",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "662a5f1d-d9f3-479d-a19e-0b1301dcc44a"
   {:compute-ref #uuid "064d03d4-afbc-4ba7-9700-63f67abca9f4",
    :exec-duration 12788,
    :id "662a5f1d-d9f3-479d-a19e-0b1301dcc44a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "6861176a-0345-43cd-89ad-80b444951844"
   {:compute-ref #uuid "9a66045f-fdbe-4785-af72-359b039354f4",
    :exec-duration 371,
    :id "6861176a-0345-43cd-89ad-80b444951844",
    :kind "code",
    :output-log-lines {:stdout 36},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "6ed09133-6f69-484b-aba5-0b7df5c17e22"
   {:compute-ref #uuid "30029eee-6295-4293-b2c8-382b8ca5541a",
    :exec-duration 13058,
    :id "6ed09133-6f69-484b-aba5-0b7df5c17e22",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "6f9a7d92-5b47-44e8-b173-e06dcdb1fe6e"
   {:compute-ref #uuid "619a9108-ce29-48dd-9d47-7b6e76b579e9",
    :exec-duration 18,
    :id "6f9a7d92-5b47-44e8-b173-e06dcdb1fe6e",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "7103d3b3-42a9-4308-badf-74833561f38b"
   {:compute-ref #uuid "3a9c0a2e-6d8a-4ed7-8144-b6955ae3d4a6",
    :exec-duration 491,
    :id "7103d3b3-42a9-4308-badf-74833561f38b",
    :kind "code",
    :output-log-lines {:stdout 1},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "787a1964-28f4-417f-a094-9ced14148b8e"
   {:compute-ref #uuid "653454be-23a7-47dc-a000-eb381e61646b",
    :exec-duration 65,
    :id "787a1964-28f4-417f-a094-9ced14148b8e",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "79ed4770-8e37-42f2-b352-dab04afc60c8"
   {:compute-ref #uuid "d1b6e045-7cf3-4ebd-a2cf-0d15043d25f7",
    :exec-duration 6676,
    :id "79ed4770-8e37-42f2-b352-dab04afc60c8",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "7a76c506-c7f7-42bd-81d1-d5480d0bcee5"
   {:compute-ref #uuid "b35b014e-ed03-4f7d-a135-29db1a4eb677",
    :exec-duration 13052,
    :id "7a76c506-c7f7-42bd-81d1-d5480d0bcee5",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "7c67a088-7186-46f3-9117-1a524f718a86"
   {:compute-ref #uuid "517df3cd-8ef0-4994-84c9-70035a5d2a93",
    :exec-duration 909,
    :id "7c67a088-7186-46f3-9117-1a524f718a86",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "7cc502a0-7696-4996-b2db-9e6701f33514"
   {:compute-ref #uuid "18c8f062-9f07-4c26-b6f8-510cb550bd3c",
    :exec-duration 29,
    :id "7cc502a0-7696-4996-b2db-9e6701f33514",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "7df4f4c7-af71-4def-9243-d2ec66fa2c87"
   {:compute-ref #uuid "5b0e44b1-3752-4f29-b0c0-a45496e07aee",
    :exec-duration 483,
    :id "7df4f4c7-af71-4def-9243-d2ec66fa2c87",
    :kind "code",
    :output-log-lines {:stdout 3},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "825d5660-e406-4e40-8ac5-42a22a9acf8b"
   {:compute-ref #uuid "205847df-423e-4428-a62b-23745cd65c8e",
    :exec-duration 398,
    :id "825d5660-e406-4e40-8ac5-42a22a9acf8b",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"],
    :stdout-collapsed? false},
   "86500356-654e-4040-8be4-0cc8be8b69a5"
   {:compute-ref #uuid "f233332c-b54f-4165-81b5-31b23f7fc10b",
    :exec-duration 816,
    :id "86500356-654e-4040-8be4-0cc8be8b69a5",
    :kind "code",
    :output-log-lines {:stdout 2},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "89de94b9-72c5-443f-806f-30621240b992"
   {:environment
    [:environment
     {:article/nextjournal.id
      #uuid "5b460d39-8c57-43a6-8b13-e217642b0146",
      :change/nextjournal.id
      #uuid "5d5c0bb4-2b00-4700-817c-f66351a3684f",
      :node/id "39e3f06d-60bf-4003-ae1a-62e835085aef"}],
    :environment? true,
    :id "89de94b9-72c5-443f-806f-30621240b992",
    :kind "runtime",
    :language "julia",
    :name "Mixed models complexity",
    :type :jupyter,
    :docker/environment-image
    "docker.nextjournal.com/environment@sha256:83caa8f11070ea6b84f7e400723df66a889b1d4a99d908b2a199adfbe8d4d566",
    :runtime/mounts
    [{:src [:node "ada236b3-896e-4a2d-8a08-737bc5160868"],
      :dest "/kronmueller-barr-2007"}]},
   "8a69369e-7364-488e-8c0b-14316e582c52"
   {:compute-ref #uuid "e456c517-bf8a-4fb8-b55c-144c153fab43",
    :exec-duration 524,
    :id "8a69369e-7364-488e-8c0b-14316e582c52",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "8acdc87b-8821-4631-816e-8b5f7bc572f0"
   {:compute-ref #uuid "657ca68a-30c3-4554-b949-fd3f03676a43",
    :exec-duration 13356,
    :id "8acdc87b-8821-4631-816e-8b5f7bc572f0",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "8f3302d5-9b53-41d8-8967-c1ac5d18ac33"
   {:compute-ref #uuid "fc6321f2-c160-4c05-9d4a-de931f28637b",
    :exec-duration 238,
    :id "8f3302d5-9b53-41d8-8967-c1ac5d18ac33",
    :kind "code",
    :output-log-lines {:stdout 0},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"],
    :stdout-collapsed? true},
   "93a8563c-a0d3-4364-81cd-1214abf59504"
   {:compute-ref #uuid "e5f61181-bf61-4eec-a886-2e901742c893",
    :exec-duration 400,
    :id "93a8563c-a0d3-4364-81cd-1214abf59504",
    :kind "code",
    :output-log-lines {:stdout 24},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "9516ad28-2fe3-4949-940c-3c414a820c2a"
   {:compute-ref #uuid "a0ef5d3b-b692-41f9-9a90-4fbc92188a36",
    :exec-duration 8,
    :id "9516ad28-2fe3-4949-940c-3c414a820c2a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "99678241-e57e-44e8-a00c-5fbec0307b21"
   {:compute-ref #uuid "3e082b77-e981-489f-85ea-ad8b9693938f",
    :exec-duration 51702,
    :id "99678241-e57e-44e8-a00c-5fbec0307b21",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "9d4d4eb2-795a-4744-87f6-cf21322a32ee"
   {:compute-ref #uuid "1523b5f9-c1bf-4629-98ba-1b05b28e57ba",
    :exec-duration 15,
    :id "9d4d4eb2-795a-4744-87f6-cf21322a32ee",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "9ed9385d-c394-44ad-b75d-493478a4d2d4"
   {:compute-ref #uuid "39eabf28-d57c-4244-961f-a77690dcf218",
    :exec-duration 12613,
    :id "9ed9385d-c394-44ad-b75d-493478a4d2d4",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "a00a2dc7-7f49-4086-bc57-0faf83a8b220"
   {:compute-ref #uuid "2e9547a2-cd63-49f2-8ecd-5363664b0ba0",
    :exec-duration 14,
    :id "a00a2dc7-7f49-4086-bc57-0faf83a8b220",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "a217767d-beb7-4f13-a5d4-dd4ad4f52822"
   {:compute-ref #uuid "b50e79b6-17ed-4764-b153-511083f1d97e",
    :exec-duration 15,
    :id "a217767d-beb7-4f13-a5d4-dd4ad4f52822",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"],
    :stdout-collapsed? true},
   "a6c198c1-3d0a-45db-a528-2e826b563d51"
   {:compute-ref #uuid "e9fc00eb-8d8c-4954-ae74-d14266e52f16",
    :exec-duration 15864,
    :id "a6c198c1-3d0a-45db-a528-2e826b563d51",
    :kind "code",
    :output-log-lines {:stdout 5},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"],
    :stdout-collapsed? false},
   "ac02350d-fa34-4551-8d53-236b9e2fb22a"
   {:compute-ref #uuid "e2db398b-6aef-4a11-92f3-1a51fe7d4a94",
    :exec-duration 397,
    :id "ac02350d-fa34-4551-8d53-236b9e2fb22a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "acb04b8e-5d64-4214-b8f4-d3a4ba347304"
   {:compute-ref #uuid "6f189853-6952-435d-a6a3-47d9d906df7f",
    :exec-duration 8532,
    :id "acb04b8e-5d64-4214-b8f4-d3a4ba347304",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "ada236b3-896e-4a2d-8a08-737bc5160868"
   {:id "ada236b3-896e-4a2d-8a08-737bc5160868",
    :kind "github-repository"},
   "b1cf832d-4e1b-451a-a132-c69294a20c02"
   {:compute-ref #uuid "67db197e-1da5-48b7-8061-9ea7ab0bacd6",
    :exec-duration 8,
    :id "b1cf832d-4e1b-451a-a132-c69294a20c02",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "b5e23c28-0079-4636-a842-43372799a628"
   {:compute-ref #uuid "7f4d4e4c-710b-4f65-b5e3-00d86f647bf7",
    :exec-duration 686,
    :id "b5e23c28-0079-4636-a842-43372799a628",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "b68a6fb0-c7e6-4783-9021-961bc0dd685b"
   {:compute-ref #uuid "c9c02e45-95b5-4c80-82e4-2ff2eab73173",
    :exec-duration 184,
    :id "b68a6fb0-c7e6-4783-9021-961bc0dd685b",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "b75a7b72-e736-4297-841c-29f56b69ee75"
   {:compute-ref #uuid "2b4f5989-3a3d-4af1-9754-201d975f1950",
    :exec-duration 408,
    :id "b75a7b72-e736-4297-841c-29f56b69ee75",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "b7b0245b-5bc7-43d5-88b3-2b05c794ca46"
   {:compute-ref #uuid "f0aa2110-d53c-4fe3-9a5f-aff3203f2038",
    :exec-duration 25,
    :id "b7b0245b-5bc7-43d5-88b3-2b05c794ca46",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "bd5b6f70-3a07-4e76-9215-d0bd119e8235"
   {:compute-ref #uuid "e0057089-314f-4072-a36a-da8abbb25976",
    :exec-duration 191,
    :id "bd5b6f70-3a07-4e76-9215-d0bd119e8235",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "c1ee2fac-b3f1-4670-a174-1b2edc2b6e47"
   {:compute-ref #uuid "425159ca-dd96-4589-ae4b-37d876ec9f55",
    :exec-duration 503,
    :id "c1ee2fac-b3f1-4670-a174-1b2edc2b6e47",
    :kind "code",
    :output-log-lines {:stdout 0},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"],
    :stdout-collapsed? true},
   "c3714a60-9b60-4d1d-b986-2d4dcef0dd12"
   {:compute-ref #uuid "46747b47-ef86-4baf-a8f3-7daa57d1cbfe",
    :exec-duration 10007,
    :id "c3714a60-9b60-4d1d-b986-2d4dcef0dd12",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "c969b50b-1f56-4b55-bac8-062bbc03f8bb"
   {:compute-ref #uuid "fda1f506-49b8-4f84-ad2e-f98d6d1779b7",
    :exec-duration 496,
    :id "c969b50b-1f56-4b55-bac8-062bbc03f8bb",
    :kind "code",
    :output-log-lines {:stdout 67},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "d2a0b78f-ad03-47e3-96ba-2c8cf968474b"
   {:compute-ref #uuid "251e9566-0049-4fbe-a510-fb95fe392276",
    :exec-duration 7536,
    :id "d2a0b78f-ad03-47e3-96ba-2c8cf968474b",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "d3119341-f167-43e5-bd5f-7a77908e09be"
   {:compute-ref #uuid "a93b8e91-74a5-4500-9ccf-a6e70b06723e",
    :exec-duration 424,
    :id "d3119341-f167-43e5-bd5f-7a77908e09be",
    :kind "code",
    :output-log-lines {:stdout 1},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]},
   "d488b9dc-29f8-48be-8ea9-f55a6cf34c6f"
   {:compute-ref #uuid "8f70986e-e839-4cf9-8764-dc87a81363aa",
    :exec-duration 11,
    :id "d488b9dc-29f8-48be-8ea9-f55a6cf34c6f",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "e9700f1c-70fb-4948-8852-ae3c414e3350"
   {:compute-ref #uuid "1fc01cb2-db55-453b-bab4-c939be5eedd4",
    :exec-duration 378,
    :id "e9700f1c-70fb-4948-8852-ae3c414e3350",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "fb57bcef-00e9-4e5c-baaa-d73e65a4a99a"
   {:compute-ref #uuid "98434566-2450-45ec-8f60-d7ef0bcb706a",
    :exec-duration 8691,
    :id "fb57bcef-00e9-4e5c-baaa-d73e65a4a99a",
    :kind "code",
    :output-log-lines {},
    :refs (),
    :runtime [:runtime "89de94b9-72c5-443f-806f-30621240b992"]},
   "fcf82605-4553-4ea1-9d73-4c6139104345"
   {:compute-ref #uuid "9559ca99-0d7f-4bca-815b-7b4c9b7991ab",
    :exec-duration 550,
    :id "fcf82605-4553-4ea1-9d73-4c6139104345",
    :kind "code",
    :output-log-lines {:stdout 28},
    :refs (),
    :runtime [:runtime "17b2e443-e360-4cf7-8617-f066c10a0af1"]}},
  :nextjournal/id #uuid "02ac105a-f71b-4cf7-bbcb-cdc79052e47a",
  :article/change
  {:nextjournal/id #uuid "5f08cd34-a52e-4442-9acb-8bb71aac0b57"}}}

```
</details>
