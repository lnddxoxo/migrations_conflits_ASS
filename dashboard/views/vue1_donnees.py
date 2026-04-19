import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import geopandas as gpd
from config import COULEURS, PAYS_ASS

def afficher():

    st.markdown(f"""
        <div style='background-color:{COULEURS["fond"]}; 
        padding:20px; border-radius:10px; margin-bottom:20px;'>
            <h2 style='color:{COULEURS["or"]}; margin:0;'>
                📊 Exploration des Données
            </h2>
            <p style='color:{COULEURS["texte"]}; margin:5px 0 0 0; font-size:15px;'>
                Analyse exploratoire des données — Afrique Subsaharienne 1996–2024
            </p>
        </div>
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
        font_color="#1A1A2E",
        height=600,
        font=dict(size=14)
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

    fig.update_traces(line=dict(width=3))

    fig.update_layout(
        plot_bgcolor="white",
        paper_bgcolor="white",
        font_color="#1A1A2E",
        height=500,
        font=dict(size=14),
        xaxis=dict(showgrid=True, gridcolor="#F5F0E8", title="Année"),
        yaxis=dict(showgrid=True, gridcolor="#F5F0E8")
    )

    st.plotly_chart(fig, use_container_width=True)

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Données brutes</h3>",
                unsafe_allow_html=True)

    colonnes_affichees = ["iso3", "annee", "n_events", "n_deaths",
                          "log_conflits", "log_migrants", "polity2"]
    st.dataframe(
        df[df["iso3"] == pays_choisi][colonnes_affichees],
        use_container_width=True,
        height=400
    )

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Animation LISA — Clusters spatiaux 1996-2024</h3>",
                unsafe_allow_html=True)

    df_lisa = pd.read_csv("data/lisa_panel.csv")

    world2 = gpd.read_file("data/ne_50m_admin_0_countries")
    afrique2 = world2[world2["ISO_A3"].isin(PAYS_ASS)].copy()

    couleurs_cluster = {
        "HH": "#C0392B",
        "LL": "#2E8B57",
        "HL": "#E8651A",
        "LH": "#3498DB",
        "NS": "#F5F0E8"
    }

    annees = sorted(df_lisa["annee"].unique())

    frames = []
    for annee in annees:
        df_an = df_lisa[df_lisa["annee"] == annee].copy()
        afrique_an = afrique2.merge(
            df_an[["iso3", "cluster"]],
            left_on="ISO_A3",
            right_on="iso3",
            how="left"
        )
        afrique_an["cluster"] = afrique_an["cluster"].fillna("NS")

        frames.append(go.Frame(
            data=[go.Choropleth(
                geojson=afrique_an.geometry.__geo_interface__,
                locations=afrique_an.index,
                z=[list(couleurs_cluster.keys()).index(c) for c in afrique_an["cluster"]],
                colorscale=[[i/4, c] for i, c in enumerate(couleurs_cluster.values())],
                showscale=False,
                hovertext=afrique_an["NAME"] + " - " + afrique_an["cluster"],
                hoverinfo="text"
            )],
            name=str(annee)
        ))

    df_premier = df_lisa[df_lisa["annee"] == annees[0]].copy()
    afrique_premier = afrique2.merge(
        df_premier[["iso3", "cluster"]],
        left_on="ISO_A3",
        right_on="iso3",
        how="left"
    )
    afrique_premier["cluster"] = afrique_premier["cluster"].fillna("NS")

    fig_lisa = go.Figure(
        data=[go.Choropleth(
            geojson=afrique_premier.geometry.__geo_interface__,
            locations=afrique_premier.index,
            z=[list(couleurs_cluster.keys()).index(c) for c in afrique_premier["cluster"]],
            colorscale=[[i/4, c] for i, c in enumerate(couleurs_cluster.values())],
            showscale=False,
            hovertext=afrique_premier["NAME"] + " - " + afrique_premier["cluster"],
            hoverinfo="text"
        )],
        frames=frames
    )

    fig_lisa.update_layout(
        title="Clusters LISA des conflits en ASS",
        paper_bgcolor="white",
        height=600,
        font=dict(size=14),
        updatemenus=[{
            "type": "buttons",
            "showactive": False,
            "buttons": [
                {"label": "Play", "method": "animate",
                 "args": [None, {"frame": {"duration": 500}, "fromcurrent": True}]},
                {"label": "Pause", "method": "animate",
                 "args": [[None], {"frame": {"duration": 0}, "mode": "immediate"}]}
            ]
        }],
        sliders=[{
            "steps": [{"args": [[str(a)], {"frame": {"duration": 300}, "mode": "immediate"}],
                       "label": str(a), "method": "animate"} for a in annees],
            "currentvalue": {"prefix": "Annee : "}
        }]
    )

    fig_lisa.update_geos(fitbounds="locations", visible=False)

    st.plotly_chart(fig_lisa, use_container_width=True)

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:15px;
        border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
        <b>Legende :</b>
        Rouge <b>HH</b> - pays en conflit entoure de voisins en conflit |
        Vert <b>LL</b> - pays stable entoure de voisins stables |
        Orange <b>HL</b> - pays en conflit entoure de voisins stables |
        Bleu <b>LH</b> - pays stable entoure de voisins en conflit |
        Blanc <b>NS</b> - non significatif
        </div>
    """, unsafe_allow_html=True)