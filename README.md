# Project-Electricity-used-for-space-conditioning-in-the-New-England-region

Data Pre-processing:
1. The problem statement clearly mentioned me to evaluate and predict the Electricity consumed in the New England region based on the following Space Conditioning Parameters like Heating, Cooling and Water Heating.
2. The dataset had 6720 observations of various regions out of which only 319 observations belonged to New England region.
3. The dataset was well scrutinized, and it was found out that it had a lot of unimportant observations (with respect to the parameters asked) and missing values.
4. On using various techniques, these missing values were dropped out from the dataset with only 148 observations left in the dataset.
5. The system has been finally fitted on 148 observations with 56 predictors.
6. The response variable i.e. Electricity consumed is the summation of the three parameters i.e. Electricity Heating use, Electricity Cooling use and Electricity Water Heating use.
7. There were two outliers in the dataset that have been removed from the dataset as the Electricity dataset is highly skewed. It was observed that, the skewness had a very big impact on the RMSE values. The skewness of the dataset has been visualized via Boxplots.
