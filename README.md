# UK Markets: Stall Occupancy Analysis 
## Introduction

Welcome to the UK Markets: Stall Occupancy Analysis project. This initiative is part of the MSc in 
Data Science program at the University of Sheffield, with a focus on applying machine learning and 
statistical techniques to address real-world challenges. 

The project utilizes the National Market Traders Federation (NMTF) dataset to analyze and predict stall 
occupancy rates across UK markets. Through regression analysis, it demonstrates proficiency in data 
preprocessing, exploratory analysis, feature selection, and the development of various machine learning 
models to identify trends and improve prediction accuracy.

## Dataset Description

The dataset, provided by the National Market Traders Federation (NMTF), comprises data from 763 UK markets 
collected between 2016 and 2019. It includes 53 variables capturing a comprehensive range of market 
characteristics:
* 	**Market Operation Days:** Indicators specifying whether the market operates on specific days of the week.
* 	**Stall Information: Data** on the number of stalls available, occupied, and actively trading for each 
                           day of the week.
* 	**Market Attributes:** Details such as region, geographical coordinates (latitude and longitude), 
                       market type, service charges, and rental agreements.

This dataset provides a valuable resource for analyzing market trends, identifying patterns in stall 
occupancy, and predicting market performance using regression analysis.

## Steps to Run the Code

### 1. Prerequisites
Before running the code, ensure the following software is installed and ready to use:
* 	R (version 4.0 or later)
*   Git (to clone the repository using the git clone command)
* 	RStudio (optional, but recommended)

### 2. Clone the Repository and Verify Branch
Download the project files by cloning the repository to your local machine by running the following command:

`git clone https://github.com/Vishh1999/uk-stall-occupancy-analysis.git`

After cloning, verify that the active Git branch is set to main:
Run the following command to check the current branch:

`git branch`

If the output shows * main,\
you can proceed to the next step.

If the branch is not set to main, switch to the main branch using the following command:

`git checkout main`

This ensures you are working on the correct branch for the project.
### 3. Dataset Placement
The dataset nmftmarkets.xlsx is typically included during the cloning process; however, ensure that the 
file nmftmarkets.xlsx is correctly placed in the root directory of the cloned repository.

### 4. Open the Script
Open the file analysis_regression.R in RStudio or another preferred IDE.

### 5. Execute the Script
Run the script in sequence or as a whole to perform the following steps:
1. 	Data Preprocessing
2. 	Exploratory Data Analysis (EDA)
3. 	Feature Selection
4. 	Model Training and Execution
5.  Model Performance Summary
    
### 6. Outputs
Upon execution, the script will generate:
* 	Visualisations, including density plots, box plots, and feature importance comparisons.
* 	A summary of model performance metrics, including Mean Squared Error (MSE).
* 	Feature importance rankings across models.

## Contact

For any queries or further information, feel free to reach out:

Name: Vishak LV\
Email: lvvishak@gmail.com\
GitHub: https://github.com/Vishh1999
