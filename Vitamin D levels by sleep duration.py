import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy import stats
import numpy as np

# Load the dataset
df = pd.read_csv(r'D:\課程\@論文\【論文電子檔】\GitHub_Code\01_CleanTable.csv')

# Remove rows with missing values in relevant columns
df = df[['sleeptimecheckresult', 'vitmaindcheckresult']].dropna()

# Assign variables for regression
x = df['sleeptimecheckresult']
y = df['vitmaindcheckresult']

# Perform linear regression and calculate R²
slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
r_squared = r_value**2
line_eq = f"y = {slope:.2f}x + {intercept:.2f}\n$R^2$ = {r_squared:.3f}"

# Plot using seaborn jointplot
sns.set(style="whitegrid")
g = sns.jointplot(
    data=df,
    x='sleeptimecheckresult',
    y='vitmaindcheckresult',
    kind='reg',
    height=6,
    color='teal',
    marginal_kws=dict(bins=20, fill=True)
)

# Add regression equation and R² to the plot
g.ax_joint.text(
    0.05, 0.95, line_eq,
    transform=g.ax_joint.transAxes,
    fontsize=12,
    verticalalignment='top',
    bbox=dict(boxstyle="round,pad=0.3", edgecolor="gray", facecolor="white", alpha=0.6)
)

# Label axes
g.set_axis_labels("Sleep Duration (hours)", "Vitamin D Level (ng/mL)")

plt.show()
