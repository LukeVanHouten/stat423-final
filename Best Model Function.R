
## Removes some of the primary variables
best_model = function(data, response) {
  new_dat = data
  n = length(new_dat)
  
  for(i in 1:n) {
    if(class(new_dat[,i]) == "numeric") {
      data[,i] = bestNormalize(data[,i]) %>% predict()
    }
  }
  
  form_1 = formula(paste(response, "~ (.^2)"))
  initial_model = lm(form_1, data = data)
  
  for(i in 1:n) {
  if(class(new_dat[,i]) == "factor") {
    colnames(new_dat)[i] = paste(colnames(new_dat)[i], "1", sep = "")
    }
  }
  
  label_names = names(which(summary(step(initial_model, direction = "forward"))$coefficients[,4] >= 0.5))
  
  vec_label_names = paste((labels_names), collapse = "+")
  form = as.formula(toString(paste(response, vec_label_names, sep = "~")))
  print(summary(lm(form, data = new_dat)))
  print(AIC(lm(form, data = new_dat)))
  print(BIC(lm(form, data = new_dat)))
}

## Should not remove any of the primary variables
best_mod = function(data, response) {
  new_dat = data
  n = length(data)
  for(i in 1:n) {
    if(class(new_dat[,i]) == "numeric") {
      data[,i] = bestNormalize(data[,i]) %>% predict()
    }
  }
  form_1 = formula(paste(response, "~ (.^2)"))
  initial_model = lm(form_1, data = data)
  best_model = step(initial_model)
  
  print(summary(best_model))
  print(AIC(best_model))
  print(p.adjust(summary(best_model)$coefficients[,4], method = "holm"))
}
