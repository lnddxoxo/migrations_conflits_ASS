import streamlit as st
import pandas as pd
import plotly.graph_objects as go
from config import COULEURS

def afficher():

    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            📈 Résultats DSDM
        </h2>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <p style='color:#1A1A2E;'>
        Résultats du modèle spatial dynamique (DSDM) — effets directs, 
        indirects et totaux des migrations sur les conflits armés en ASS.
        </p>
    """, unsafe_allow_html=True)

    df = pd.read_csv("data/sdm_results.csv")

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Tableau des effets</h3>",
                unsafe_allow_html=True)

    df_affiche = df[["variable", "direct", "z_direct", "p_direct",
                     "indirect", "z_indirect", "p_indirect",
                     "total", "z_total", "p_total"]].copy()

    def significativite(p):
        if p < 0.01:
            return "***"
        elif p < 0.05:
            return "**"
        elif p < 0.10:
            return "*"
        else:
            return ""

    df_affiche["sig_direct"] = df_affiche["p_direct"].apply(significativite)
    df_affiche["sig_indirect"] = df_affiche["p_indirect"].apply(significativite)
    df_affiche["sig_total"] = df_affiche["p_total"].apply(significativite)

    st.dataframe(df_affiche, use_container_width=True)

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Effets directs / indirects / totaux</h3>",
                unsafe_allow_html=True)

    variables = df["variable"].tolist()

    fig = go.Figure()

    fig.add_trace(go.Bar(
        name="Effet Direct",
        x=variables,
        y=df["direct"],
        marker_color=COULEURS["vert"]
    ))

    fig.add_trace(go.Bar(
        name="Effet Indirect",
        x=variables,
        y=df["indirect"],
        marker_color=COULEURS["or"]
    ))

    fig.add_trace(go.Bar(
        name="Effet Total",
        x=variables,
        y=df["total"],
        marker_color=COULEURS["orange"]
    ))

    fig.update_layout(
        barmode="group",
        paper_bgcolor="white",
        plot_bgcolor="white",
        font_color="#1A1A2E",
        legend=dict(orientation="h", yanchor="bottom", y=1.02),
        height=500
    )

    st.plotly_chart(fig, use_container_width=True)

    st.markdown("---")

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:15px; 
        border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
        <b>Lecture des résultats :</b> *** = significatif à 1%, ** = 5%, * = 10%. 
        L'effet direct mesure l'impact des migrations d'un pays sur ses propres conflits. 
        L'effet indirect (spillover) mesure l'impact sur les conflits des pays voisins. 
        L'effet total est la somme des deux.
        </div>
    """, unsafe_allow_html=True)