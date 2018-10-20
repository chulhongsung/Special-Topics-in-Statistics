# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
import os

from sklearn.decomposition import PCA
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis

#os.chdir('C:/Users/UOS/Desktop/data/AutoEncoLDA')

np.random.seed(1)

'''1st variable'''
a = np.random.normal(5, 2, 1000)
b = np.random.normal(3, 2, 1000)
X1 = np.concatenate([a,b])

'''2nd variable'''
c = np.random.randint(10, size = 1000)
d = np.random.randint(5, 15, size = 1000)
X2 = np.concatenate([c,d])

'''3rd variable'''
e = np.random.normal(0, 3, 2000)
X3 = e

'''4th variable'''
f = np.random.binomial(1, 0.7, 1000)
g = np.random.binomial(1, 0.3, 1000)
X4 = np.concatenate([f,g])

''' 5th variable'''
h = np.random.randint(5,10, size = 1000)
i = np.random.randint(9,15, size = 1000)
X5 = np.concatenate([h,i])

'''6th variable'''
j = np.random.poisson(5, 1000)
k = np.random.poisson(2, 1000)
X6 = np.concatenate([j,k])

'''Y class'''
y1 = np.repeat(1, 1000)
y2 = np.repeat(0, 1000)
Y_data = np.concatenate([y1, y2]).reshape([2000,1])

'''X table'''
X_data = np.concatenate([X1, X2, X3, X4, X5, X6], axis = 0).reshape([2000,6])


'''Tensorflow graph'''
tf.reset_default_graph() 

'''Layer 1'''
X = tf.placeholder(tf.float32, shape = (None, 6))
Y = tf.placeholder(tf.float32, shape =(None, 1))

w1 = tf.get_variable('W1', shape= [6,30], initializer=tf.contrib.layers.xavier_initializer())
b1 = tf.Variable(tf.random_normal([30]))
h1 = tf.nn.relu(tf.matmul(X, w1) + b1)

'''Layer 2'''
w2 = tf.get_variable('W2', shape= [30,20], initializer=tf.contrib.layers.xavier_initializer())
b2 = tf.Variable(tf.random_normal([20])) 
h2 = tf.nn.relu(tf.matmul(h1, w2) + b2)

'''Layer 3'''
w3 = tf.get_variable('W3', shape= [20,4], initializer=tf.contrib.layers.xavier_initializer())
b3 = tf.Variable(tf.random_normal([4]))
h3 = tf.nn.relu(tf.matmul(h2, w3) + b3)

'''Layer 4'''
w4 = tf.get_variable('W4', shape= [4,2], initializer=tf.contrib.layers.xavier_initializer())
b4 = tf.Variable(tf.random_normal([2]))
xy = tf.matmul(h3, w4) + b4

'''mean, variance'''
Y_double = tf.matmul(Y, tf.constant([1,1], shape=[1,2],dtype = tf.float32))
mu1 = tf.reduce_sum(tf.multiply(xy,Y_double), 0)/tf.reduce_sum(Y, 0)
mu2 = tf.reduce_sum(tf.multiply(xy,(1-Y_double)), 0)/tf.reduce_sum((1-Y), 0)
v1 = tf.reduce_sum(tf.square(tf.multiply(xy,Y_double) - mu1*Y_double), 0)/tf.reduce_sum(Y, 0)
v2 = tf.reduce_sum(tf.square(tf.multiply(xy,(1-Y_double)) - mu2*(1-Y_double)),0)/tf.reduce_sum((1-Y),0)

'''loss function'''
normal_mu1 = tf.constant([5,5], dtype=tf.float32)
normal_mu2 = tf.constant([2,2], dtype=tf.float32)

loss = (tf.reduce_sum(v1)+ tf.reduce_sum(tf.square(mu1 - normal_mu1))- tf.log(v1[0]*v1[1]))/2 + (tf.reduce_sum(v2)+ tf.reduce_sum(tf.square(mu2 - normal_mu2)) - tf.log(v2[0]*v2[1]))/2 -2
optimizer = tf.train.AdamOptimizer(learning_rate = 1e-2)
train = optimizer.minimize(loss)

'''Session'''
saver = tf.train.Saver(max_to_keep = 30)
xylist = []
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    for step in range(1001):
        sess.run(train, feed_dict = {X: X_data, Y: Y_data})
        if step % 50 == 0:
            xylist.append(sess.run(xy, feed_dict = {X: X_data, Y: Y_data}))
            print(step, sess.run(loss, feed_dict = {X: X_data, Y: Y_data}))
            save_path = saver.save(sess, './tmp_20181018/epoch' + str(step)+ 'model.ckpt')
    print(sess.run([mu1, mu2, v1, v2],feed_dict = {X: X_data, Y: Y_data}))
xycoord = np.array(xylist).reshape(42000,2)

colk = ["orange" for x in range(1000)]
colm = ["crimson" for x in range(1000)]

plt.scatter(xycoord[0:2000,0], xycoord[0:2000,1], c = colk + colm, alpha = 0.5)
plt.scatter(xycoord[2000:4000,0], xycoord[2000:4000,1], c = colk + colm, alpha = 0.5)
plt.scatter(xycoord[4000:6000,0], xycoord[4000:6000,1], c = colk + colm, alpha = 0.5)
plt.scatter(xycoord[6000:8000,0], xycoord[6000:8000,1], c = colk + colm, alpha = 0.5)
plt.scatter(xycoord[40000:42000,0], xycoord[40000:42000,1], c = colk + colm, alpha = 0.5)

twodim = xycoord[40000:42000,]
Y_data

PCA = PCA(n_components=2)
X_PCA = PCA.fit(X_data).transform(X_data)

LDA = LinearDiscriminantAnalysis(n_components = 2)
QDA = QuadraticDiscriminantAnalysis()

twodim_LDA = LDA.fit_transform(twodim, Y_data)
X_PCA_LDA = LDA.fit_transform(X_data, Y_data)

LDA.score(X_data, Y_data)
'''Out[72]: 0.912'''

LDA.score(twodim, Y_data)
'''Out[76]: 0.51'''

X_QDA = QDA.fit(X_data, Y_data)

QDA.score(X_data, Y_data)
'''Out[85]: 0.8045'''


'''fig, ax = plt.subplots()
ax.set(xlim = [-5, 10], ylim = [-5,10])

scat = ax.scatter([],[])


def animate(i):
    x_plot = xycoord[2000*(i-1):2000*i,0]
    y_plot = xycoord[2000*(i-1):2000*i,1]
    
    scat.set_offsets([x_plot,y_plot])
    scat.set_edgecolors(colk + colm)
    
    return scat,

anim = FuncAnimation(fig, animate, frames = 20, interval = 10)

plt.show()'''



