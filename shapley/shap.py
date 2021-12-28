import shap

import pandas as pd
from sklearn.ensemble import RandomForestClassifier
import numpy

with open('coco.csv') as coco_file, open('artificial.csv') as artificial_file:
  train = pd.read_csv(coco_file)
  X = train.iloc[:,:-1]
  y = train.iloc[:,-1]
  rforest = RandomForestClassifier(n_estimators=1000, max_depth=None, min_samples_split=2, random_state=0)
  model = rforest.fit(X, y)

  explainer = shap.TreeExplainer(model)
  shap_values = explainer(X, check_additivity=False)

  train_artificial = pd.read_csv(artificial_file)
  X_artificial = train_artificial.iloc[:,:-1]
  y_artificial = train_artificial.iloc[:,-1]

  X_artificial = numpy.clip(X_artificial, -1e30, 1e30)
  rforest_artificial = RandomForestClassifier(n_estimators=1000, max_depth=None, min_samples_split=2, random_state=0)
  model_artificial = rforest_artificial.fit(X_artificial, y_artificial)
  explainer_artificial = shap.TreeExplainer(model_artificial)
  shap_values_artificial = explainer_artificial(X_artificial, check_additivity=False)


for i in range(0,8):
  numpy.savetxt(f"data/coco_{i+1}.csv", shap_values.values[:,:,i], delimiter=",")

for i in range(0,10):
  numpy.savetxt(f"data/artificial_{i+1}.csv", shap_values_artificial.values[:,:,i], delimiter=",")