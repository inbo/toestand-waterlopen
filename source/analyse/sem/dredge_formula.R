x_vars_schoon <- setdiff(predictors, "jaar_s")

# 2. Plak alle overgebleven voorspellers (X) aan elkaar met een " + "
x_string <- paste(x_vars_schoon, collapse = " + ")

# 3. Bouw de formule: Y ~ X'en + vaste jaar_s + random meetplaats
# (Als x_string leeg is, fit hij netjes alleen jaar_s en het random effect)
if (nchar(x_string) > 0) {
  formule_string <- paste(y_var, "~", x_string, "+ jaar_s + (1 | meetplaats)")
} else {
  formule_string <- paste(y_var, "~ jaar_s + (1 | meetplaats)")
}

# Zet de tekst om naar een échte R-formule
formula_obj <- as.formula(formule_string)
print(formula_obj)
