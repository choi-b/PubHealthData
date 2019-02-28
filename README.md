# Public Health Data Challenge

This is an analysis motivated by the American Statistical Association (ASA)'s Public Health Data Challenge using a dataset from the Centers for Disease Control and Prevention (CDC).

**Motivation:** *"As of 2016,  2.1 million Americans have an opioid use disorder, and drug overdose deaths are now the leading cause of injury and death in the United States. But some of the country’s top minds are working to fight this epidemic, and statisticians are helping to lead the charge." - ASA*

The link to the challenge can be found [here.](https://thisisstatistics.org/public-health-data-challenge/)
The link to the original dataset can be found [here.](https://wonder.cdc.gov/mcd.html)

The dataset used in this analysis has 100,000 randomly sampled rows from the original 44 million observations in the original CDC WONDER Multiple Cause of Death (Detailed Mortality) dataset. 

There were 5 variables involved in the analysis: 
  1. Census Region (West, South, Northeast, Midwest) 
  2. Age of deceased
  3. Year of death (1999-2016)
  4. Gender
  5. Cause of death (drug and alcohol-induced) - 7 types.
  
For the cause of death variable, the focus was on causes that were drug and alcohol-related. 

#### Cause of Death Code/Full Description:
  1. A1 - Alcohol poisonings (overdose) (X45, X65, Y15)
  2. A9 - All other alcohol-induced causes
  3. D1 - Drug poisonings (overdose) Unintentional (X40-X44)
  4. D2 - Drug poisonings (overdose) Suicide (X60-X64) 
  5. D4 - Drug poisonings (overdose) Undetermined (Y10-Y14)
  6. D9 - All other drug-induced causes
  7. O9 - All other non-drug and non-alcohol causes

The categorical variables were rearranged as dummy variables (values 0 or 1), so in total, after taking the dummy variable trap into account, the dataset consisted of age + gender + year + 3 regions + 6 causes = 12 variables.

#### EXPLORATORY ANALYSIS

The cpairs function in the gclus library helps us see the correlations between the variables.

The *dplyr* and *ggplot2* libraries were used for general EDA.

<p align="center">
  <img src="https://user-images.githubusercontent.com/32057260/53530762-ee6f9c80-3abe-11e9-950c-e54678461391.png" width="350">
  <img src="https://user-images.githubusercontent.com/32057260/53530859-4a3a2580-3abf-11e9-9bf1-b95d1952db05.png" width="350">
</p>

#### MULTIVARIATE ANALYSIS

The multivariate techniques primarily used in the analysis include:
  1. Principal Component Analysis (PCA) 
  2. Multiple Correspondence Analysis (MCA)
  3. Canonical Correlations Analysis (CCA) - once with region vs. rest, once with causes vs. rest.

# Principal Components Analysis

Using the **prcomp** function, and *factoextra* library.

<p align="center">
  <img src="https://user-images.githubusercontent.com/32057260/53532583-6771f280-3ac5-11e9-94cf-ce06c6afe22b.png" width="350">
  <img src="https://user-images.githubusercontent.com/32057260/53532586-69d44c80-3ac5-11e9-81e8-50351839d25e.png" width="350">
</p>

# Multiple Correspondence Analysis

Using the **MCA** function, and *factoextra* library.

<p align="center">
  <img src="https://user-images.githubusercontent.com/32057260/53533018-25e24700-3ac7-11e9-9383-1d68010b8e64.png" width="350">
  <img src="https://user-images.githubusercontent.com/32057260/53533017-25e24700-3ac7-11e9-9dbf-bcb0a1758e26.png" width="350">
</p>


# Canonical Correlations Analysis

Using the libraries *dotcall64, fields, and CCA*.
