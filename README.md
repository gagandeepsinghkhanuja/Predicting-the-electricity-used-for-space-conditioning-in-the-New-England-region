# Project-Electricity-used-for-space-conditioning-in-the-New-England-region

Data Pre-processing:
1. The problem statement clearly mentioned me to evaluate and predict the Electricity consumed in the New England region based on the following Space Conditioning Parameters like Heating, Cooling and Water Heating.
2. The dataset had 6720 observations of various regions out of which only 319 observations belonged to New England region.
3. The dataset was well scrutinized, and it was found out that it had a lot of unimportant observations (with respect to the parameters asked) and missing values.
4. On using various techniques, these missing values were dropped out from the dataset with only 148 observations left in the dataset.
5. The system has been finally fitted on 148 observations with 56 predictors.
6. The response variable i.e. Electricity consumed is the summation of the three parameters i.e. Electricity Heating use, Electricity Cooling use and Electricity Water Heating use.
7. There were two outliers in the dataset that have been removed from the dataset as the Electricity dataset is highly skewed. It was observed that, the skewness had a very big impact on the RMSE values. The skewness of the dataset has been visualized via Boxplots.
8. Data transformation techniques like log transformation have been implemented on the response variable (i.e. Electricity) in the model. But no significant effect can be seen on the RMSE values when it was fitted on the models.
9. PCA Analysis was also done on the selected 56 variables, since my objective was to profile the variables. The biplots obtained for my variables were not in a good resolution; R does not process those plots well for many variables. Based on the plots obtained I could conclude that my variables did not form explicitly profiles, since most were Categorical Variables.
10. Density plots were used for visualization of the dataset. The plots were used to visualize the concentration of the dependent variables. The histograms helped me in visualizing this data in a better way and show me the frequency distribution of the dataset.
11. Encoding was used to create dummy variables as the dataset is categorical and the results were compared. It was found out that there was no change in the rmse values or r square values when the dataset was in the numeric format or in the encoded format. The snippet has been attached in the code for your reference, so that you could implement and check it, if at all you would like to check.
