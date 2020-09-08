# Summer School Questions

## Daniel Backhaus

1. What is `lmer(..., control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))` good for? Are there pitfalls in usage? Do I need to worry about this with MixedModels?
2. When does double bar syntax work and when doesn’t work it? Is this equivalent to MixedModels?
3. Is a difference with this small effectsize reproducible or random? How to power calculate that? In MixedModels?

## Marleen Haupt

1. Transformation in case of non-Gaussian distributed model residuals? 
2. Contrast specifications for higher-order interactions?

## Kyla McConnell

1. How to specify matched items in the LMM?
2. What can be captured with (additional) random factors or random effects for such designs?
3. If we consider the entire spillover region (W2, spillover1, spillover2, spillover3) as the critical region, and add position as a categorical variable, how should this factor be coded? Helmert coding?

## Noam Tal-Perry

1. How are the statistics in emmeans calculated? Are they reliable? Should I instead rely solely on the contrasts given “for free” by the model? I just find that I cannot get a satisfactory group of contrasts, which means I need to remodel the data several times to get all the contrasts I need.
2. The LR test for [an effect] results in a non-significant p-value (and Bayesian approximation resulted in a large BF, yet the contrast for [this effect] in the full model summary results in an enormously significant (/large) effect. How are the two to be settled?
3. I’d like to understand better whether I can use LR test (anova) to contrast between models with different random structures. If not, how can I support the choice of one model and not the other, except for relying on model convergence?
