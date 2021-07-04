# From https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
lm_eqn <- function(df,y,x,facet){
  df_eq <- data.frame()
  facet <- df[[facet]]
  formula = as.formula(sprintf('%s~%s',y,x))
  
  for(i in levels(facet)){
  m <- lm(formula, data=data.table(df) %>% subset(facet==i))
  sum_m <- summary(lm(formula, data=data.table(df)%>% subset(facet==i)))
  if (m$coefficients[2] >= 0)  {
    eq <- substitute(italic(target) == a + b %.% italic(input)*","~~italic(r)^2~"="~r2*","~~p~"="~italic(pvalue), 
                           list(target = 'y',
                                input = 'x',
                                a = format(as.vector(coef(m)[1]), digits = 2), 
                                b = format(as.vector(abs(coef(m)[2])), digits = 2), 
                                r2 = format(summary(m)$r.squared, digits = 3),
                                # getting the pvalue is painful
                                pvalue = format(summary(m)$coefficients[2,'Pr(>|t|)'], digits=1)
                           )
    )
  } else {
    eq <- substitute(italic(target) == a - b %.% italic(input)*","~~italic(r)^2~"="~r2*","~~p~"="~italic(pvalue), 
                     list(target = 'y',
                          input = 'x',
                          a = format(as.vector(coef(m)[1]), digits = 2), 
                          b = format(as.vector(abs(coef(m)[2])), digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3),
                          # getting the pvalue is painful
                          pvalue = format(summary(m)$coefficients[2,'Pr(>|t|)'], digits=1)
                     )
    )
  }
  
  df_eq <- rbind(df_eq,cbind(as.character(as.expression(eq)),i))}

            
  colnames(df_eq) <- c("equation","variable")
  return(df_eq)
}
