# Classification-for-ordinal-data-and-applications-in-clinical-research

Built up statistical models (Cumulative Logits Model, Continuation Ratio Model) and machine learning models 
(Conditional Inference Trees and Forest, SVM with robust tree decoding) on depression data (with normal, mild, 
severe ordinal response)

Made comparison of the performance of each model under higher and lower dimensional feature

This topic was accepted by 2016 ASA Conference on Statistical Practice (CSP 2016)

- Project Description
Data set: 99 patients, 25 normal, 39 mild depression and 35 severe depression.
Purpose: Predict depression level (normal < mild < severe)
Predictors :
  - Demographic variables (age, gender, handness)
  - High-dimension MRI image data to measure mean fractional anisotropy and gray matter volume for different brain regions (all continuous). 222 predictors.
Typical n <p datasets.
  -Reduced dataset (all demographic variables and 20 most important predictors obtained from prior information)
  
- Results
  - Pre-processing is very important for high-dimension noisy data
  - For each machine learning based model, careful tunningof parameters is required to obtain better result
  - For low dimension data, regression model is advantageous in performance and model interpretation
  - For high dimension data, machine learning based model is advantageous in performance and time efficiency
  - Performance measurement and variable importance ranking specific for ordinal response should be used
