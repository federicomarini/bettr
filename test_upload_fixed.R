# Test script for the fixed bettr upload functionality

# Load the updated library
library(bettr)

# Create sample test data
test_data <- data.frame(
    Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC", "AlgorithmD", "AlgorithmE"),
    Precision = c(0.85, 0.92, 0.78, 0.88, 0.95),
    Recall = c(0.90, 0.85, 0.95, 0.82, 0.88),
    F1_Score = c(0.87, 0.88, 0.86, 0.85, 0.91),
    Accuracy = c(0.89, 0.91, 0.83, 0.87, 0.93),
    Runtime_seconds = c(12.5, 8.2, 15.7, 10.1, 9.8)
)

# Save test data to CSV
write.csv(test_data, "benchmark_test_data.csv", row.names = FALSE)

cat("Test data created: benchmark_test_data.csv\n")
cat("Sample data:\n")
print(head(test_data))

cat("\n=== Testing Upload Functionality ===\n")
cat("To test the upload feature:\n")
cat("1. Run: bettr()\n")
cat("2. Upload the file: benchmark_test_data.csv\n")
cat("3. Select 'Method' as ID column\n")
cat("4. Select metrics like 'Precision', 'Recall', 'F1_Score'\n")
cat("5. Click 'Generate Visualization'\n\n")

cat("The following fixes were applied:\n")
cat("- Removed type parameters from showNotification() calls\n")
cat("- Simplified data preparation in upload interface\n")
cat("- Fixed variable scope issues\n\n")

# Test basic functionality
if (interactive()) {
    cat("Launching upload interface...\n")
    bettr()
} else {
    cat("Run this interactively to test the upload interface:\n")
    cat("bettr()\n")
}