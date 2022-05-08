# -*- coding: utf-8 -*-
import numpy as np
import tensorflow
import spektral
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Model
from spektral.data import Dataset, BatchLoader, Graph
from spektral.transforms.normalize_adj import NormalizeAdj
from tensorflow.keras.layers import Dropout
from tensorflow.keras.regularizers import l2
from tensorflow.keras.callbacks import EarlyStopping
from spektral.layers import GCNConv, GlobalSumPool
import scipy.sparse as sparse
################################################################################
# Config
################################################################################
learning_rate = 1e-3  # Learning rate
epochs = 40  # Number of training epochs
es_patience = 10  # Patience for early stopping
batch_size = 30  # Batch size
l2_reg = 5e-4
local_test = r.test_data
local_train = r.training_data
local_val = r.validation_data
################################################################################
# Load data
################################################################################

class GraphDataset(Dataset):
    def __init__(self, n_samples, df, n_colors=60, **kwargs):
        self.n_samples = n_samples
        self.df = df  
        self.n_colors = n_colors  
        super().__init__(**kwargs)

    def read(self):
        output = []
        for i in range(self.n_samples):
            # Node features
            iter_x = self.df["train_x"][i].copy()
            x = np.array(iter_x)#.reshape(10,402)
            # Edges
            iter_a =  self.df["train_a"][i].copy()
            adj_V = iter_a[:,2]
            adj_R = iter_a[:,0]
            adj_C = iter_a[:,1]
            a = sparse.coo_matrix((adj_V, (adj_R, adj_C)), shape=(401,401))
            a.todense()
            y = np.zeros((30,))
            y_index = int((self.df["y"][i])*10)
            y[:y_index] = 1
           
            output.append(Graph(x=x, a=a, y=y))
        return(output)

        # We must return a list of Graph objects

 #
# Train/valid/test split
len_train = len(local_train["train_x"])
len_val = len(local_val["train_x"])
len_test = len(local_test["train_x"])

data_tr = GraphDataset(n_samples = len_train, df = local_train, transforms=NormalizeAdj())
data_va = GraphDataset(n_samples = len_val, df = local_val, transforms=NormalizeAdj())
data_te = GraphDataset(n_samples = len_test, df=local_test, transforms=NormalizeAdj())

# Data loaders
loader_tr = BatchLoader(data_tr, batch_size=batch_size, epochs=epochs)
loader_va = BatchLoader(data_va, batch_size=batch_size)
loader_te = BatchLoader(data_te, batch_size=batch_size)
#add callback to stop training if loss doesn't improve for 3 epochs
callback = EarlyStopping(monitor='loss', patience=3, min_delta = 0.001)
################################################################################
# Build model
################################################################################
class BDB22GNN(Model):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.conv1 = GCNConv(32, activation="elu")
        self.conv2 = GCNConv(32, activation="elu")
        self.flatten = GlobalSumPool()
        self.fc1 = Dense(512, activation="relu")
        self.fc2 = Dense(30, activation="sigmoid")  

    def call(self, inputs):
        x, a = inputs
        x = self.conv1([x, a])
        x = self.conv2([x, a])
        output = self.flatten(x)
        output = self.fc1(output)
        output = self.fc2(output)

        return output
model = BDB22GNN()
model.compile('adam', "mean_absolute_error")

model.fit(loader_tr.load(), validation_data= loader_va.load(), steps_per_epoch=loader_tr.steps_per_epoch, validation_steps=loader_va.steps_per_epoch, epochs=100, callbacks=[callback])
#model.fit(loader_tr.load(), validation_data= loader_va.load())

test_loss = model.evaluate(loader_te.load(), steps=loader_te.steps_per_epoch)

print('Test loss: {}'.format(test_loss))
predictions = model.predict(loader_te.load(), steps =loader_te.steps_per_epoch)