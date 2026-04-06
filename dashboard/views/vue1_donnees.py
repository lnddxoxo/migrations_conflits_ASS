import streamlit as st
import pandas as pd
import plotly.express as px
from config import COULEURS, PAYS_ASS

def afficher():
    
    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            📊 Exploration des Données
        </h2>
    """, unsafe_allow_html=True)
    
    df = pd.read_csv("data/panel_clean.csv")
    
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Pays", df["iso3"].nunique())
    with col2:
        st.metric("Années", f"{int(df['annee'].min())} — {int(df['annee'].max())}")
    with col3:
        st.metric("Observations", len(df))
    
    st.markdown("---")
    
    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Évolution temporelle</h3>", 
                unsafe_allow_html=True)
    
    pays_choisi = st.selectbox(
        "Choisissez un pays",
        sorted(df["iso3"].unique())
    )
    
    variable = st.radio(
        "Variable",
        ["log_conflits", "log_migrants", "polity2"],
        horizontal=True
    )
    
    df_pays = df[df["iso3"] == pays_choisi]
    
    fig = px.line(
        df_pays,
        x="annee",
        y=variable,
        title=f"{variable} — {pays_choisi}",
        color_discrete_sequence=[COULEURS["or"]]
    )
    
    fig.update_layout(
        plot_bgcolor="white",
        paper_bgcolor="white",
        font_color="#1A1A2E"
    )
    
    st.plotly_chart(fig, use_container_width=True)
    
    st.markdown("---")
    
    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Données brutes</h3>",
                unsafe_allow_html=True)
    
    st.dataframe(df[df["iso3"] == pays_choisi], use_container_width=True)