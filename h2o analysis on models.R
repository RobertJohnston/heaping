# Data quality analysis 

# Analysis of heaping in height/weight/age in months in anthro surveys

# Use of H20 to find best fitting model

options(timeout = 600)

# Step 1: Remove any old versions (optional but recommended)
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
pkgs <- c("RCurl","jsonlite")
# Load dependencies
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# install
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

# install.packages("devtools")
devtools::install_url("https://h2o-release.s3.amazonaws.com/h2o/latest_stable_R/h2o-R-3.46.0.7.tar.gz")
# Load h2o
library(h2o)
h2o.init()

# open correct count data for agemons 

# Convert to H2O frame
df_h2o <- as.h2o(df_clean)

# Run AutoML
aml <- h2o.automl(x = "agemons", y = "n",
                  training_frame = df_h2o,
                  max_models = 10,
                  seed = 123)

# View leaderboard
leaderboard <- h2o.get_leaderboard(aml)
print(leaderboard)