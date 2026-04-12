import streamlit as st
import pandas as pd
import plotly.express as px
import geopandas as gpd
from config import COULEURS, PAYS_ASS, NOMS_PAYS

def afficher():

    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            🎛️ Simulateur de Chocs Spatiaux
        </h2>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <p style='color:#1A1A2E;'>
        Choisissez un pays source et simulez une hausse des migrations. 
        La carte montre l'impact sur les conflits de tous les pays voisins.
        </p>
    """, unsafe_allow_html=True)

    df_impacts = pd.read_csv("data/impacts_matrix.csv", index_col=0)

    pays_disponibles = sorted(
        df_impacts.columns.tolist(),
        key=lambda x: NOMS_PAYS.get(x, x)
    )

    pays_options = {NOMS_PAYS.get(p, p): p for p in pays_disponibles}

    col1, col2 = st.columns([2, 1])
    with col1:
        pays_source_nom = st.selectbox(
            "Pays source du choc migratoire",
            list(pays_options.keys())
        )
        pays_source = pays_options[pays_source_nom]
    with col2:
        choc = st.slider(
            "Intensité du choc (%)",
            min_value=1,
            max_value=50,
            value=10
        )

    impacts = df_impacts[pays_source] * choc

    world = gpd.read_file("data/ne_50m_admin_0_countries")
    afrique = world[world["ISO_A3"].isin(PAYS_ASS)]

    impacts_df = impacts.reset_index()
    impacts_df.columns = ["ISO_A3", "impact"]

    afrique = afrique.merge(impacts_df, on="ISO_A3", how="left")

    fig = px.choropleth(
        afrique,
        geojson=afrique.geometry,
        locations=afrique.index,
        color="impact",
        color_continuous_scale=[
            COULEURS["conflit_bas"],
            COULEURS["or"],
            COULEURS["conflit_haut"]
        ],
        hover_name="NAME",
        hover_data={"impact": ":.4f"},
        title=f"Impact d'un choc de +{choc}% des migrations au {pays_source_nom}"
    )

    fig.update_geos(fitbounds="locations", visible=False)
    fig.update_layout(
        paper_bgcolor="white",
        font_color="#1A1A2E",
        height=500
    )

    st.plotly_chart(fig, use_container_width=True)

    st.markdown("---")

    top5 = impacts.abs().nlargest(5)

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Top 5 pays les plus affectés</h3>",
                unsafe_allow_html=True)

    for pays, valeur in top5.items():
        nom_pays = NOMS_PAYS.get(pays, pays)
        direction = "hausse" if valeur > 0 else "baisse"
        st.markdown(f"""
            <div style='background-color:#F5F0E8; padding:10px; 
            margin:5px 0; border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
            <b>{nom_pays}</b> — {direction} de {abs(valeur):.4f} des conflits
            </div>
        """, unsafe_allow_html=True)

    st.markdown("---")

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:15px; 
        border-left:4px solid {COULEURS["orange"]}; border-radius:5px;'>
        <b>Interprétation :</b> Un choc de +{choc}% des migrations au <b>{pays_source_nom}</b> 
        génère des effets spillover sur les pays voisins via les interactions spatiales 
        capturées par la matrice W. Les valeurs positives indiquent une hausse des conflits, 
        les valeurs négatives une baisse.
        </div>
    """, unsafe_allow_html=True)