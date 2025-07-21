
import streamlit as st
import pandas as pd
import altair as alt
from pathlib import Path

# --- Page config ---
st.set_page_config(
    page_title='Subcommunity Health Explorer',
    layout='centered',
    page_icon='🧬'
)

# --- Data loading ---
@st.cache_data
def load_data():
    base_path = Path(__file__).parent
    env = pd.read_csv(base_path / 'aggregated_subcommunity_data.csv')
    disease = pd.read_csv(base_path / 'intermediate_disease_data.csv')
    socio = pd.read_csv(base_path / 'summary_table.csv')
    # Harmonise column names
    socio = socio.rename(columns={'Categoría':'category', 'Comunidad':'subcommunity', 'OR':'value', 'IC 95%':'ci'})
    socio['dataset'] = 'socio'
    env['dataset'] = 'environment'
    disease['dataset'] = 'disease'
    return env, disease, socio

env_df, disease_df, socio_df = load_data()

st.title('Subcommunity Health Explorer')

dataset_choice = st.sidebar.selectbox(
    'Dataset',
    options=['Environmental / Ancestry', 'Disease', 'Socio‑economic'],
    index=0
)

if dataset_choice == 'Environmental / Ancestry':
    df = env_df.copy()
    variables = df['variable'].unique()
    var = st.sidebar.selectbox('Variable', sorted(variables))
    df = df[df['variable'] == var]
    category = st.sidebar.selectbox('Category', sorted(df['category'].unique()))
    df_plot = df[df['category'] == category]
    st.subheader(f'{var} – {category}')
    chart = alt.Chart(df_plot).mark_bar().encode(
        x=alt.X('subcommunity:N', sort='-y', title='Sub‑community'),
        y=alt.Y('prop:Q', title='Proportion'),
        tooltip=['subcommunity', 'prop', 'min_ci', 'max_ci']
    ).properties(height=400)
    st.altair_chart(chart, use_container_width=True)
    st.dataframe(df_plot[['subcommunity', 'prop', 'min_ci', 'max_ci']].set_index('subcommunity'))

elif dataset_choice == 'Disease':
    df = disease_df.copy()
    condition = st.sidebar.selectbox('Condition', sorted(df['condition'].unique()))
    df_plot = df[df['condition'] == condition]
    st.subheader(f'Disease prevalence – {condition}')
    chart = alt.Chart(df_plot).mark_bar().encode(
        x=alt.X('subcommunity:N', sort='-y', title='Sub‑community'),
        y=alt.Y('prop:Q', title='Prevalence (%)'),
        tooltip=['subcommunity', 'prop', 'min_prop', 'max_prop']
    ).properties(height=400)
    st.altair_chart(chart, use_container_width=True)
    st.dataframe(df_plot[['subcommunity', 'prop', 'min_prop', 'max_prop']].set_index('subcommunity'))

else:
    df = socio_df.copy()
    category = st.sidebar.selectbox('Category', sorted(df['category'].unique()))
    df_plot = df[df['category'] == category]
    st.subheader(f'Socio‑economic odds ratio – {category}')
    # Extract ci lower and upper
    ci_bounds = df_plot['ci'].str.strip('[]').str.split(',', expand=True).astype(float)
    df_plot['ci_lower'] = ci_bounds[0]
    df_plot['ci_upper'] = ci_bounds[1]
    chart = alt.Chart(df_plot).mark_bar().encode(
        x=alt.X('subcommunity:N', sort='-y', title='Sub‑community'),
        y=alt.Y('value:Q', title='Odds Ratio'),
        tooltip=['subcommunity', 'value', 'ci_lower', 'ci_upper']
    ).properties(height=400)
    st.altair_chart(chart, use_container_width=True)
    st.dataframe(df_plot[['subcommunity', 'value', 'ci_lower', 'ci_upper']].set_index('subcommunity'))

st.markdown('---')
st.markdown(
    """**How to use this app**

1. Choose a dataset in the sidebar.
2. Select filters to focus on specific variables or conditions.
3. Hover bars for details; sort will reflect magnitude.

_Data sources: aggregated_subcommunity_data.csv, intermediate_disease_data.csv, tabla_resumen.csv._"""
)
