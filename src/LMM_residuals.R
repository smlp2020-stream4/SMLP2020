# Plot LMM residauls for quick inspection
# Author: Reinhold Kliegl
# Version 0.1 (2018-09-03)

plot_LMM_residuals <- function(model) {
  
  
  df <- model@frame
  df <- fortify.merMod(model)
  
  p_model_res1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_point(colour="blue", size=.3) +
    xlab("Fitted values") + ylab("Residuals") + 
    geom_hline(yintercept=0) + theme_bw()
  
  p_model_res2 <- ggplot(df, aes(.fitted, .resid/sd(.resid))) +
    geom_point(shape=I(".")) +  
    xlab("Fitted values") + ylab("Standardized residuals") + 
    geom_hline(yintercept=0) + geom_density2d(size=1) +  # + geom_hex()
    theme_bw() 
  
  p_res <- grid.arrange(p_model_res1, p_model_res2, nrow=2)
  
  return(p_res)
}
