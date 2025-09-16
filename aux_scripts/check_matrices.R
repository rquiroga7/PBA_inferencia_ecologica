# Check matrix normalization
true_transfer_matrix1 <- matrix(c(
  0.70, 0.05, 0.20, 0.04, 0.01,
  0.03, 0.75, 0.10, 0.07, 0.05,
  0.02, 0.80, 0.12, 0.04, 0.02,
  0.05, 0.03, 0.85, 0.05, 0.02,
  0.10, 0.25, 0.15, 0.40, 0.10,
  0.02, 0.00, 0.16, 0.02, 0.80
), nrow = 6, ncol = 5, byrow = TRUE)

true_transfer_matrix2 <- matrix(c(
  0.70, 0.05, 0.20, 0.04, 0.01,
  0.03, 0.75, 0.10, 0.07, 0.05,
  0.07, 0.15, 0.65, 0.10, 0.03,
  0.05, 0.03, 0.85, 0.05, 0.02,
  0.05, 0.05, 0.10, 0.70, 0.10,
  0.01, 0.00, 0.11, 0.01, 0.87
), nrow = 6, ncol = 5, byrow = TRUE)

true_transfer_matrix3 <- matrix(c(
  0.80, 0.02, 0.15, 0.02, 0.01,
  0.02, 0.90, 0.05, 0.02, 0.01,
  0.01, 0.95, 0.03, 0.01, 0.00,
  0.03, 0.02, 0.93, 0.01, 0.01,
  0.05, 0.20, 0.10, 0.60, 0.05,
  0.02, 0.03, 0.05, 0.05, 0.85
), nrow = 6, ncol = 5, byrow = TRUE)

print("Matrix 1 row sums:")
print(rowSums(true_transfer_matrix1))
print("Matrix 2 row sums:")
print(rowSums(true_transfer_matrix2))
print("Matrix 3 row sums:")
print(rowSums(true_transfer_matrix3))