import streamlit as st
import os
os.chdir(os.path.dirname(os.path.abspath(__file__)))
from config import COULEURS, TITRE_DASHBOARD, SOUS_TITRE

st.set_page_config(
    page_title="Migrations & Conflits ASS",
    page_icon="🌍",
    layout="wide"
)

st.markdown("""
    <link rel="stylesheet" 
    href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css">
    <style>
        [data-testid="metric-container"] {
            background-color: #F5F0E8;
            border: 2px solid #F5A623;
            border-radius: 10px;
            padding: 15px;
        }
        [data-testid="metric-container"] label {
            font-size: 16px !important;
            color: #1A1A2E !important;
            font-weight: bold;
        }
        [data-testid="metric-container"] div {
            font-size: 28px !important;
            color: #F5A623 !important;
            font-weight: bold;
        }
        .stDataFrame {
            border: 2px solid #F5A623;
            border-radius: 10px;
        }
        p, li, div {
            font-size: 15px !important;
        }
    </style>
""", unsafe_allow_html=True)

st.markdown(f"""
    <h1 style='color:#F5A623; text-align:center; margin-top:20px;'>
        {TITRE_DASHBOARD}
    </h1>
    <p style='color:#1A1A2E; text-align:center; font-size:16px;'>
        {SOUS_TITRE}
    </p>
""", unsafe_allow_html=True)

vue = st.radio(
    label="",
    options=[
        "📊  Données & Exploration",
        "🕸️  Réseau Spatial W",
        "📈  Résultats DSDM",
        "🎛️  Simulateur de Chocs"
    ],
    horizontal=True,
    label_visibility="collapsed"
)

st.markdown("---")

if "Données" in vue:
    from views.vue1_donnees import afficher
    afficher()
elif "Réseau" in vue:
    from views.vue2_reseau import afficher
    afficher()
elif "Résultats" in vue:
    from views.vue3_resultats import afficher
    afficher()
elif "Simulateur" in vue:
    from views.vue4_simulateur import afficher
    afficher()