frepcomgen <- function(m, n) {
  # Generăm matrice aleatoare pentru variabila X și Y cu maxim 3 zecimale
  p_X <- round(matrix(runif(m * n), nrow = m, ncol = n), 5)
  p_Y <- round(matrix(runif(m * n), nrow = m, ncol = n), 5)
  
  # Calculăm repartiția marginală a variabilei X și o normalizăm
  p_X_marginal <- rowSums(p_X, na.rm = TRUE)
  p_X_marginal <- round(p_X_marginal / sum(p_X_marginal, na.rm = TRUE), 5)
  
  # Calculăm repartiția marginală a variabilei Y și o normalizăm
  p_Y_marginal <- colSums(p_Y, na.rm = TRUE)
  p_Y_marginal <- round(p_Y_marginal / sum(p_Y_marginal, na.rm = TRUE), 5)
  
  # Construim tabelul cu repartiția comună și îl rotunjim la 3 zecimale
  p_XY_comuna <- round(p_X_marginal %*% t(p_Y_marginal), 5)
  
  # Setăm aleator unele elemente ca fiind NA pentru a indica incompletitudinea
  p_XY_comuna[sample(m * n, m * n * 0.3)] <- NA
  p_X_marginal[sample(m, m * 0.3)] <- NA
  p_Y_marginal[sample(n, n * 0.3)] <- NA
  
  
  return(list(p_XY_comuna = p_XY_comuna, p_X_marginal = p_X_marginal, p_Y_marginal = p_Y_marginal))
}

# Exemplu de utilizare
rezultate_incomplete_cu_zecimale_normalizat <- frepcomgen(3, 4)
print(rezultate_incomplete_cu_zecimale_normalizat$p_XY_comuna)
print(rezultate_incomplete_cu_zecimale_normalizat$p_X_marginal)
print(rezultate_incomplete_cu_zecimale_normalizat$p_Y_marginal)

