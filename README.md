# Classification-for-ordinal-data-and-applications-in-clinical-research

Built up statistical models (Cumulative Logits Model, Continuation Ratio Model) and machine learning models 
(Conditional Inference Trees and Forest, SVM with robust tree decoding) on depression data (with normal, mild, 
severe ordinal response)

Made comparison of the performance of each model under higher and lower dimensional feature

This topic was accepted by 2016 ASA Conference on Statistical Practice (CSP 2016)


Project Description <br />
  - Data set: 99 patients, 25 normal, 39 mild depression and 35 severe depression <br />
  - Purpose: Predict depression level (normal < mild < severe) <br />
  - Predictors :    <br />
        - Demographic variables (age, gender, handness) <br />
        - High-dimension MRI image data to measure mean fractional anisotropy and gray matter volume for different  <br />
          brain regions (all continuous). 222 predictors. <br />
  - Typical n <p datasets. <br />
  - Reduced dataset (all demographic variables and 20 most important predictors obtained from prior information) <br />
  
  
Results <br />
  - Pre-processing is very important for high-dimension noisy data <br />
  - For each machine learning based model, careful tunningof parameters is required to obtain better result <br />
  - For low dimension data, regression model is advantageous in performance and model interpretation <br />
  - For high dimension data, machine learning based model is advantageous in performance and time efficiency <br />
  - Performance measurement and variable importance ranking specific for ordinal response should be used <br />
