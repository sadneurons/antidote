import pandas as pd
import seaborn as sb
from matplotlib import pyplot as plt
import pyreadr
x = pyreadr.read_r("x.rds")
x = x['x']
plt.plot(x).show()

