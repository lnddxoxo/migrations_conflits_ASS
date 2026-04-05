import streamlit as st
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
        .topbar {
            background-color: white;
            padding: 10px 20px;
            display: flex;
            gap: 10px;
            border-bottom: 2px solid #F5A623;
            margin-bottom: 20px;
        }
        .topbar-btn {
            background: none;
            border: none;
            padding: 8px 16px;
            cursor: pointer;
            font-size: 14px;
            color: #1A1A2E;
            border-radius: 5px;
            display: flex;
            align-items: center;
            gap: 6px;
        }
        .topbar-btn:hover {
            background-color: #F5A623;
            color: white;
        }
        .topbar-btn.active {
            background-color: #F5A623;
            color: white;
            font-weight: bold;
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