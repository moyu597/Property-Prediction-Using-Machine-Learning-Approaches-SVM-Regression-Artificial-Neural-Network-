"importing the required libraries"
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tensorflow as tf
import sklearn
from sklearn import preprocessing
from sklearn import metrics
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split
from keras.models import Sequential
from keras.layers import Dense
from keras.optimizers import Adam
from keras.callbacks import EarlyStopping

datf = pd.read_excel(r'/Users/og/Desktop/Workbook1.xlsx') #importing the excel spread sheet containing the input feature into python using pandas
datf1= pd.read_excel(r'/Users/og/Desktop/Workbook2.xlsx')#importing the excel spread sheet containing the predictor/output feature into python using pandas
datf2= pd.read_excel(r'/Users/og/Desktop/Workbook3.xlsx')#importing the excel spread sheet containing the predictor/output feature into python using pandas
x_train, x_test, y_train, y_test = train_test_split(datf, datf1, test_size=0.2, random_state=37)#splitting the data at random into training and testing dataset and setting test size to 20%
x_train = preprocessing.scale(x_train) #scaling the training set to avaoid high range variables having bias effect on model
x_test = preprocessing.scale(x_test) #scaling the test set to avaoid high range variables having bias effect on model
model = Sequential()
model.add(Dense(4, input_shape=(3,), activation = 'relu'))#building the neural network
model.add(Dense(4, activation='relu'))
model.add(Dense(4, activation='relu'))
model.add(Dense(1,))
model.compile(loss='mean_squared_error', optimizer='Adam')#optimizing parameters
earlystopper = EarlyStopping(monitor='val_loss', min_delta=0, patience=15, verbose=1, mode='auto')
history = model.fit(x_train, y_train, epochs = 103, validation_split = 0.2,shuffle = True, verbose = 0,callbacks = [earlystopper])
pred = model.predict(x_test)
print(pred)
score = np.sqrt(metrics.mean_squared_error(pred,y_test))
print("Final score (RMSE): {}".format(score))
print(pred)

