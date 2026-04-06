import streamlit as st
import pandas as pd
import plotly.express as px
import geopandas as gpd
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

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Carte des Conflits en ASS</h3>",
                unsafe_allow_html=True)

    world = gpd.read_file("data/ne_50m_admin_0_countries")
    afrique = world[world["ISO_A3"].isin(PAYS_ASS)]

    df_moy = df.groupby("iso3")["log_conflits"].mean().reset_index()
    df_moy.columns = ["ISO_A3", "log_conflits_moy"]

    afrique = afrique.merge(df_moy, on="ISO_A3", how="left")

    fig_carte = px.choropleth(
        afrique,
        geojson=afrique.geometry,
        locations=afrique.index,
        color="log_conflits_moy",
        color_continuous_scale=[
            COULEURS["conflit_bas"],
            COULEURS["or"],
            COULEURS["conflit_haut"]
        ],
        hover_name="NAME",
        hover_data={"log_conflits_moy": ":.2f"},
        title="Intensité moyenne des conflits 1996-2024"
    )

    fig_carte.update_geos(fitbounds="locations", visible=False)
    fig_carte.update_layout(
        paper_bgcolor="white",
        font_color="#1A1A2E"
    )

    st.plotly_chart(fig_carte, use_container_width=True)

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