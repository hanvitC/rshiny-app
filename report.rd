\name{ReportProject2}
\alias{ReportProject2}
\title{Report Project 2}
\description{
Project 2 Report Capytool Group 9\\
Kashish Kumar, Shenghong Wu, Han Choi, Jimin Park
}
\details{
Report Project 2

Project 2 Report Capytool Group 9  
Kashish Kumar, Shenghong Wu, Han Choi, Jimin Park 

Design and Structure  
The application’s user interface is organized into several tabs that group functionalities. The Home tab 
presents instructions on how to use the tool and what it offers. The Preprocessing tab provides controls for 
file upload, column selection, cleaning options, and encoding choices. The Feature Engineering tab gives 
users the ability to generate new features from existing data using mathematical and statistical 
transformations. The Exploratory Data Analysis tab is designed to support multiple types of plots and 
displays summary statistics. Finally, the Modeling tab allows users to split data and run models such as 
linear regression, logistic regression, random forest, and gradient boosting. 

Reactive Programming and Library Usage  
The program makes extensive use of Shiny’s reactive programming paradigm. Reactive expressions and 
eventReactive functions are used to update data outputs based on user inputs. The dynamic updates ensure 
that visualizations, data tables, and model outputs reflect the current state of the data and user selections. 
The integration of libraries such as dplyr supports data manipulation while DT provides interactive tables. 
Plotly is used for generating interactive charts. Modeling functions from caret, randomForest, and gbm 
provide the statistical backbone for the modeling tab. The application is designed to operate in real time, 
adjusting outputs as input values change. 

Dataset Loading  
The application accepts file uploads in multiple formats including CSV, Excel, JSON, and RDS. The code 
uses a reactive function that iterates over each uploaded file and reads the file with the appropriate reader 
function. The function applies error handling using try-catch methods and displays messages when an 
error occurs. The files are processed into data frames after cleaning the column names by removing 
non-alphanumeric characters and converting them to lower case. This module establishes the basis for the 
subsequent data processing steps. 
Workflow 

Data cleaning and preprocessing are performed in the Preprocessing tab. Users have the option to 
remove missing values and duplicates. The code supports a choice between imputation methods. When 
the imputation method is set to use the median for numeric variables and the mode for categorical 
variables, the code iterates through each column in the dataset and applies the corresponding statistic. 
Outlier detection is implemented using two methods: Z-score and interquartile range (IQR). The code 
computes either the scaled Z-scores or the quartile ranges and provides an option to remove observations 
with values exceeding a user-specified threshold. 

The module also provides options to convert data types. The user can choose to convert numeric 
variables to characters, character variables to numeric, or character variables to dates. Scaling options are 
provided in the form of standardization (Z-score) or normalization (min-max scaling). Categorical 
encoding is offered with two methods: one-hot encoding and label encoding. In the one-hot encoding 
option, the code uses the model.matrix function to create dummy variables. A mapping key is generated 
by comparing original column names with the encoded column names. In the label encoding option, 
character columns are converted to factors and then to numeric values. The generated encoding key is 
displayed in a dedicated sub-tab titled Encoding Key. Additional text cleaning options include trimming 
whitespace, converting to lower case, and removing punctuation. 

The Feature Engineering tab allows users to create new features from the original dataset. The 
interface provides multiple methods for generating features. The log transformation option creates a new 

column by applying a logarithm (with an offset) to the selected column values. The polynomial feature 
option generates a new column by raising the values of a selected column to a user-specified power. For 
interaction terms, the code multiplies the values of two selected columns to form a new variable. 
Multi-column interaction computes the product of values across several selected columns. Binning splits a 
continuous variable into intervals based on the number of bins specified by the user. The custom 
transformation option permits the user to input an expression, using the variable x, to generate a new 
column based on the selected column. The code uses try-catch to handle errors in custom expressions and 
notifies the user when an error occurs. 

The EDA tab presents a range of interactive visualization tools. The code offers several types of 
plots including scatter plots, histograms, box plots, pair plots, PCA plots, correlation heatmaps, cluster 
plots, and violin plots. The interface allows users to select specific columns for analysis. The scatter plot 
and cluster plot options include interactive brushing to select subsets of data. The PCA plot is generated 
by applying principal component analysis to numeric columns and supports both two-dimensional and 
three-dimensional representations. The correlation heatmap computes pairwise correlations and displays 
the results using a heatmap. The cluster plot employs k-means clustering on the selected numeric data and 
provides an option for a multivariable parallel coordinates representation. In the scatter and cluster plot 
options, the code implements an interactive lasso function. This function permits users to select a subset 
of data points using brushing on the plot. When points are selected, the brushed data is captured and 
rendered as a data table. The table of selected points is available for review and download. This 
interactive lasso function supports data exploration by linking visual selections to tabular outputs. The 
EDA module also includes a section for descriptive statistics where summary measures are computed for 
numeric and categorical variables. 

The Modeling tab provides functionality for splitting the data and fitting statistical models. The 
code divides the dataset into training and test sets based on a user-defined ratio. Users can select the target 
variable and predictor variables. The modeling section supports linear regression for numeric outcomes 
and logistic regression for categorical outcomes. Options for Random Forest and Gradient Boosting 
models are also provided. The code builds the model based on a dynamically generated formula. 
Predictions are made on the test set and performance is assessed using measures such as RMSE and 
R-squared for numeric outcomes or confusion matrices for categorical outcomes. The interface displays a 
preview of the training set, model summary output, and interactive performance plots. Download options 
are available for the training set, test set, and model summary. 
 
Group Contributions 
We all contributed equally and agreed upon distribution.  
Kashish Kumar- EDA and Modeling Tabs, UI and amending other workflow elements for interactivity 
and additional features 
Shengdong Wu- Preprocessing tab backend and panel visual including all functions and outputs 
Han Choi- Data upload and sample data preprepartion and custom sample data functions, Github creation 
and group scheduling 
Jimin Park- Feature engineering tab including column selector options and all pre-built 
statistical/mathematical functions 

Github: https://github.com/hanvitC/rshiny-app 
Shiny: https://1s4aja-kashish-kumar.shinyapps.io/rshiny-app/ 

https://github.com/hanvitC/rshiny-app
https://1s4aja-kashish-kumar.shinyapps.io/rshiny-app/
}
\author{
Kashish Kumar, Shenghong Wu, Han Choi, Jimin Park
}
\keyword{report}
