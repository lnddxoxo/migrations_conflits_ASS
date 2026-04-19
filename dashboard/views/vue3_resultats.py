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

    def significativite(p):
        if p < 0.01:
            return "***"
        elif p < 0.05:
            return "**"
        elif p < 0.10:
            return "*"
        else:
            return ""

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Tableau des effets</h3>",
                unsafe_allow_html=True)

    df_affiche = df[["variable", "direct", "z_direct", "p_direct",
                     "indirect", "z_indirect", "p_indirect",
                     "total", "z_total", "p_total"]].copy()

    df_affiche["sig_direct"] = df_affiche["p_direct"].apply(significativite)
    df_affiche["sig_indirect"] = df_affiche["p_indirect"].apply(significativite)
    df_affiche["sig_total"] = df_affiche["p_total"].apply(significativite)

    df_affiche["Direct"] = df_affiche.apply(
        lambda r: f"{r['direct']:.4f} ({r['z_direct']:.3f}){r['sig_direct']}", axis=1)
    df_affiche["Indirect"] = df_affiche.apply(
        lambda r: f"{r['indirect']:.4f} ({r['z_indirect']:.3f}){r['sig_indirect']}", axis=1)
    df_affiche["Total"] = df_affiche.apply(
        lambda r: f"{r['total']:.4f} ({r['z_total']:.3f}){r['sig_total']}", axis=1)

    st.dataframe(
        df_affiche[["variable", "Direct", "Indirect", "Total"]],
        use_container_width=True
    )

    st.markdown("---")

    st.markdown(f"<h3 style='color:{COULEURS['vert']};'>Effets directs / indirects / totaux</h3>",
                unsafe_allow_html=True)

    variables = df["variable"].tolist()
    sig_direct = df["p_direct"].apply(significativite).tolist()
    sig_indirect = df["p_indirect"].apply(significativite).tolist()
    sig_total = df["p_total"].apply(significativite).tolist()

    labels_direct = [f"{v}<br>{s}" if s else v for v, s in zip(variables, sig_direct)]
    labels_indirect = [f"{v}<br>{s}" if s else v for v, s in zip(variables, sig_indirect)]
    labels_total = [f"{v}<br>{s}" if s else v for v, s in zip(variables, sig_total)]

    fig = go.Figure()

    fig.add_trace(go.Bar(
        name="Effet Direct",
        x=variables,
        y=df["direct"],
        marker_color=COULEURS["vert"],
        error_y=dict(
            type="data",
            symmetric=False,
            array=(df["ic_direct_high"] - df["direct"]).tolist(),
            arrayminus=(df["direct"] - df["ic_direct_low"]).tolist(),
            color="#1A1A2E",
            thickness=1.5
        ),
        text=sig_direct,
        textposition="outside",
        textfont=dict(size=14, color=COULEURS["vert"])
    ))

    fig.add_trace(go.Bar(
        name="Effet Indirect",
        x=variables,
        y=df["indirect"],
        marker_color=COULEURS["or"],
        error_y=dict(
            type="data",
            symmetric=False,
            array=(df["ic_indirect_high"] - df["indirect"]).tolist(),
            arrayminus=(df["indirect"] - df["ic_indirect_low"]).tolist(),
            color="#1A1A2E",
            thickness=1.5
        ),
        text=sig_indirect,
        textposition="outside",
        textfont=dict(size=14, color=COULEURS["or"])
    ))

    fig.add_trace(go.Bar(
        name="Effet Total",
        x=variables,
        y=df["total"],
        marker_color=COULEURS["orange"],
        text=sig_total,
        textposition="outside",
        textfont=dict(size=14, color=COULEURS["orange"])
    ))

    fig.update_layout(
        barmode="group",
        paper_bgcolor="white",
        plot_bgcolor="white",
        font_color="#1A1A2E",
        legend=dict(orientation="h", yanchor="bottom", y=1.02),
        height=550,
        font=dict(size=13),
        yaxis=dict(showgrid=True, gridcolor="#F5F0E8", zeroline=True,
                   zerolinecolor="#1A1A2E", zerolinewidth=1)
    )

    st.plotly_chart(fig, use_container_width=True)

    st.markdown("---")

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:15px; 
        border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
        <b>Lecture des résultats :</b> *** = significatif à 1%, ** = 5%, * = 10%. 
        Les barres d'erreur représentent les intervalles de confiance à 95%.
        L'effet direct mesure l'impact des migrations d'un pays sur ses propres conflits. 
        L'effet indirect (spillover) mesure l'impact sur les conflits des pays voisins. 
        L'effet total est la somme des deux.
        </div>
    """, unsafe_allow_html=True)