import streamlit as st
import pandas as pd
import networkx as nx
import plotly.graph_objects as go
from config import COULEURS, PAYS_ASS

def afficher():

    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            🕸️ Réseau Spatial W
        </h2>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <p style='color:#1A1A2E;'>
        La matrice W formalise les interactions spatiales entre pays. 
        Chaque lien représente une influence géographique entre deux pays voisins.
        </p>
    """, unsafe_allow_html=True)

    matrice = st.radio(
        "Choisissez la matrice",
        ["W1 — Contiguïté", "W2 — Inverse Distance"],
        horizontal=True
    )

    if "W1" in matrice:
        df_w = pd.read_csv("data/W1_matrix.csv", index_col=0)
    else:
        df_w = pd.read_csv("data/W2_matrix.csv", index_col=0)

    G = nx.from_pandas_adjacency(df_w)
    pos = nx.spring_layout(G, seed=42)

    edge_x, edge_y = [], []
    for u, v in G.edges():
        x0, y0 = pos[u]
        x1, y1 = pos[v]
        edge_x += [x0, x1, None]
        edge_y += [y0, y1, None]

    edge_trace = go.Scatter(
        x=edge_x, y=edge_y,
        mode="lines",
        line=dict(width=0.8, color=COULEURS["or"]),
        hoverinfo="none"
    )

    node_x = [pos[n][0] for n in G.nodes()]
    node_y = [pos[n][1] for n in G.nodes()]
    node_labels = list(G.nodes())
    node_degrees = [G.degree(n) for n in G.nodes()]

    node_trace = go.Scatter(
        x=node_x, y=node_y,
        mode="markers+text",
        text=node_labels,
        textposition="top center",
        textfont=dict(size=8, color="#1A1A2E"),
        hovertemplate="<b>%{text}</b><br>Connexions: %{customdata}<extra></extra>",
        customdata=node_degrees,
        marker=dict(
            size=10,
            color=node_degrees,
            colorscale=[COULEURS["vert"], COULEURS["or"], COULEURS["conflit_haut"]],
            showscale=True,
            colorbar=dict(title="Connexions")
        )
    )

    fig = go.Figure(
        data=[edge_trace, node_trace],
        layout=go.Layout(
            title="Réseau des interactions spatiales entre pays ASS",
            showlegend=False,
            paper_bgcolor="white",
            plot_bgcolor="white",
            height=700,
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            font=dict(color="#1A1A2E")
        )
    )

    st.plotly_chart(fig, use_container_width=True)

    if "W1" in matrice:
        st.markdown(f"""
            <div style='background-color:#F5F0E8; padding:15px; 
            border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
            <b>Lecture du graphe :</b> Chaque point représente un pays d'Afrique subsaharienne. 
            Les lignes indiquent une frontière terrestre commune. 
            Le Congo Démocratique (COD) apparaît comme le pays le plus connecté. 
            Madagascar (MDG) est isolé — c'est une île sans frontière terrestre.
            </div>
        """, unsafe_allow_html=True)
    else:
        st.markdown(f"""
            <div style='background-color:#F5F0E8; padding:15px; 
            border-left:4px solid {COULEURS["or"]}; border-radius:5px;'>
            <b>Lecture du graphe :</b> Dans la matrice inverse-distance, tous les pays sont connectés 
            mais avec des poids différents selon leur proximité géographique. 
            Plus deux pays sont proches, plus leur lien est fort. 
            Avec 904 connexions, cette matrice capture des interactions au-delà des simples frontières.
            </div>
        """, unsafe_allow_html=True)

    st.markdown("---")

    col1, col2 = st.columns(2)
    with col1:
        st.metric("Nombre de pays", len(G.nodes()))
    with col2:
        st.metric("Connexions totales", len(G.edges()))