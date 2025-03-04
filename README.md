# Insurance Fraud Detection

Fraud detection is one of the most popular applications of machine learning in anti-financial crime activities. In this experiment, we will be working with a dataset containing over 500k insurance claims and 55 features around patients, practitioners, and healthcare providers.

This dataset can be found at https://www.kaggle.com/datasets/rohitrox/healthcare-provider-fraud-detection-analysis

The experiment is sequentially conducted as follows:
1. Data preprocessing: As labelled data were assigned to healthcare providers, we need to transform original variables and create new features corresponding to healthcare providers.
2. Feature selection: Out of dozens of features available at hand, only the most important ones are input to the models. This can be obtained via correlation matrix, Random Forest, Recursive Feature Elimination, etc.
3. Modelling: Multiple machine learning models are constructed (Decision Tree, SVM, Neural Network, ensemble) with hyperparameter tuning to identify the optimal set of parameters for each classifier.
4. Model selection: Based on pre-defined metrics (AUC, accuracy, etc.) to choose the superior model for this dataset.

In this experiment, Random Forest produced the highest AUC (0.935), marginally greater than SVM (0.934) and Neural Network (0.931). This result once again confirms the dominance of ensembles in predictive analytics arena.

![image](https://github.com/user-attachments/assets/d413070a-d7d2-43c2-afe4-5000ff51721a)

