import streamlit as st
from config import COULEURS

def afficher():

    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            🎛️ Simulateur de Chocs Spatiaux
        </h2>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <p style='color:#1A1A2E;'>
        Ce simulateur permet de visualiser en temps réel l'impact d'une hausse 
        des migrations dans un pays sur les conflits armés de ses voisins. 
        C'est la démonstration visuelle des effets spillover du modèle DSDM.
        </p>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:30px; 
        border-left:4px solid {COULEURS["orange"]}; border-radius:5px;
        text-align:center; height:400px; display:flex; 
        align-items:center; justify-content:center;'>
        <p style='color:#1A1A2E; font-size:18px;'>
        ⏳ En attente de la matrice d'impacts<br>
        <small>impacts_matrix.csv sera disponible le 13 avril</small><br><br>
        <span style='font-size:14px; color:{COULEURS["vert"]};'>
        Le simulateur permettra de :<br>
        ✓ Choisir un pays source<br>
        ✓ Simuler une hausse des migrations<br>
        ✓ Visualiser l'impact sur tous les pays voisins
        </span>
        </p>
        </div>
    """, unsafe_allow_html=True)