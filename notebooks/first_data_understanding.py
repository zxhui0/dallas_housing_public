# # First data understanding

import sys
from pathlib import Path
import os
source_path = str(Path(os.path.abspath('ajv-first_data_understanding')).parent.parent / 'src')
if source_path not in sys.path:
    sys.path.insert(0, source_path)

import matplotlib.pyplot as plt
import pandas as pd
import re
import seaborn as sns

from utils import utils

data_path = utils.load_config('dev')['data_path']

# ## Reading the data

file = Path(data_path) / 'eviction0818crt41-42.csv.xlsx'

raw = pd.read_excel(file)

df = raw.copy()

df.columns = [c.strip().lower().replace(' ', '_') for c in df.columns]

# ## Some basic notions of the data

df.shape

df['case_number'].nunique()

# *case_number* is the unique identifier of the table.

df.head()

df.info()

df.describe()

categorical_cols = ['case_number', 'pl_city', 'pl_state', 'pl_zip', 'df_city', 'df_state', 'df_zip']

# +
print("Number of unique elements for:")

for col in categorical_cols:
    print(f"* {col}: {df[col].nunique()}")
    
    try:
        df[col] = df[col].str.strip()
    except AttributeError:
        continue
# -

string_cols = [c for c in df.select_dtypes(include=['object']).columns if c != 'case_number']

df.loc[:, string_cols] = df.loc[:, string_cols].apply(lambda x: x.str.lower())

df.groupby(['df_state'])[['case_number']].count()

df.groupby(['df_state', 'df_city'])[['case_number']].count().rename(columns={'case_number': 'count'}).\
    sort_values('count', ascending=False).head(10)


# **Note**: There are empty states, cities mispelled ('irivng', instead of 'irving'), etc. We need to think how to address this!

# ## Dealing with addresses

# Separate addresses into street number, street name and possible floor or apartment. We need to improve this part!

def separate_address(address_str):
    
    regex = r"^(?P<street_number>\d+)*[ ]*(?P<street>[\D]*)[ ]*(?P<apartment>#*\d+)*"  # TODO: Improve regex
    
    match = re.match(regex, address_str)
    
    try:
        street_number = match.group('street_number')
        
    except AttributeError:
        street_number = None
    
    try:
        street = match.group('street')
    except AttributeError:
        street = None
    
    street = re.sub('#', '', street)
        
    try: 
        apt = match.group('apartment')
    except AttributeError:
        apt = None
    
    return street_number, street, apt


def normalize_address(address_str):
    
    address_dict = {
        'rd': 'road',
        'st': 'street',
        'ave': 'avenue',
        'dr': 'drive',
        'pkwy': 'parkway',
        'ln': 'lane'
    }
    
    for abbr, long_str in address_dict.items():
        address_str = re.sub(abbr, long_str, address_str)
    
    return address_str.replace('.', '')    


df.head()

df[['df_street_number', 'df_street', 'df_apt']] = \
    pd.DataFrame(df.apply(lambda x: separate_address(x.df_address), axis=1).tolist())

df['df_street'] = df['df_street'].apply(normalize_address)

df.sample(10)

# ## Some first visualizations

amount_s = df.amount

# +
nrows = 1
ncols = 1
fontsize = 14

fig, ax = plt.subplots(nrows=nrows, ncols=ncols, figsize=(15, 7))


axis = ax

sns.distplot(amount_s, kde=True, rug=True)

axis.set_ylabel('Distribution', fontsize=fontsize)
axis.set_xlabel('Amount ($)', fontsize=fontsize)

# axis.legend(loc='best', fontsize=fontsize)

axis.tick_params(axis='both', labelsize=12)
fig.tight_layout()

# +
nrows = df.df_state.nunique()
ncols = 1
fontsize = 14

fig, ax = plt.subplots(nrows=nrows, ncols=ncols, figsize=(15, 7*nrows))

count = 0
for st, aux_df in df.groupby('df_state'):
    
    axis = ax[count]

    sns.distplot(aux_df['amount'], kde=True, ax=axis)
    
    count = count + 1

axis.set_ylabel('Distribution', fontsize=fontsize)
axis.set_xlabel('Amount ($)', fontsize=fontsize)

axis.tick_params(axis='both', labelsize=12)
fig.tight_layout()
# -

df['amount'].hist()


