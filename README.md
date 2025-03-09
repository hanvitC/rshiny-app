# Capytool

## Overview
- Capytool is an advanced R Shiny application developed for the STAT 4243 Project 2 Assignment.
- The app provides interactive data processing, visualization, and modeling tools, guiding users through every step from data upload to predictive modeling.

## Features

### 1. Uploading Datasets of Different Formats
- Users can upload datasets in multiple formats (CSV, Excel, JSON, or RDS) or use built-in examples.
- The app enables dynamic selection of columns immediately after upload for further analysis.

### 2. Preprocessing and Cleaning
- The app handles missing values by imputing numeric data with the median and categorical data with the mode.
- It automatically removes duplicate records.
- Outliers can be detected using either the Z-score or IQR method, with an option to remove them.
- Data type conversion is supported, allowing users to convert columns to character, numeric, or date formats.
- Numeric features can be scaled using either standardization (Z-score) or normalization (0–1).
- Categorical variables can be encoded with either one‑hot encoding or label encoding.
- Text fields can be cleaned by trimming whitespace, converting to lowercase, and removing punctuation.

### 3. Feature Engineering
- Users can apply a log transformation to selected numeric columns.
- Polynomial features can be generated to capture nonlinear relationships (e.g. squared terms).
- Interaction terms can be created between two or more variables.
- Multi‑column interactions are supported by computing the product of several selected columns.
- Continuous variables can be binned into discrete categories.
- Custom mathematical transformations can be applied using user-defined expressions (e.g., `log(x+1)`).

### 4. Exploratory Data Analysis (EDA)
- Interactive scatter plots allow users to visualize relationships between two variables and select (brush) points (“Lasso Points”) for further inspection.
- Histograms and box plots can be generated for one or more selected columns, with titles reflecting the selected variables.
- A scatterplot matrix (pair plot) is available for multivariate exploration.
- PCA plots can be generated in 2D or 3D, with options to group by a selected categorical variable and download the PCA components.
- An interactive correlation heatmap shows relationships among selected numeric variables.
- Cluster plots (including both standard 2D and multivariable parallel coordinate plots) and violin plots for clusters are provided.
- Descriptive statistics display standard summary statistics for numeric columns, while categorical columns are labeled as "Categorical" with empty cells for numerical summaries.
- All outputs (data, descriptive statistics, PCA components, and brushed points) are downloadable.

### 5. Modeling
- The app allows users to split their data into training and test sets with an adjustable ratio.
- It supports multiple modeling techniques including Linear Regression, Logistic Regression, Random Forest, and Gradient Boosting.
- Interactive model outputs include a preview of the training set (showing only the target and predictor columns), model summaries, performance metrics, and plots comparing predicted versus actual values.
- Users can download both the training/test datasets and the model summaries.

## Installation

To run Capytool locally, ensure that you have R installed along with the required packages. Install them using the following command in R:

```r
install.packages(c("shiny", "shinyWidgets", "shinythemes", "dplyr", "DT", "readr", "readxl", "jsonlite", "plotly", "shinycssloaders", "caret", "randomForest", "gbm", "zip", "webshot", "reshape2"))
```

Additionally, install and configure PhantomJS for the webshot package:

```r
webshot::install_phantomjs()
```

## Usage

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/Capytool.git
   ```

2. **Open in RStudio:**  
   Open the project directory in RStudio.

3. **Run the App:**

   In the R console, run:

   ```r
   shiny::runApp("app.R")
   ```

## Files

- `app.R`: The main file containing the full Shiny application code.
- `data/`: Directory containing sample datasets used within the app (if applicable).

## Collaborators

- [Han Choi](https://github.com/hanvitC)
- [Kashish Kumar](https://github.com/kashishky)
- [Jimin Park](https://github.com/jp4632)
- [Shenghong Wu](https://github.com/Yang5356)

## Acknowledgments

- Special thanks to Instructor Alex Pijyan for guidance and support.
- Appreciation to the R community and the developers of the various R packages used in this project.