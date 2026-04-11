import streamlit as st
from config import COULEURS

def afficher():

    st.markdown(f"""
        <h2 style='color:{COULEURS["or"]};'>
            📈 Résultats DSDM
        </h2>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <p style='color:#1A1A2E;'>
        Cette vue présente les résultats du modèle spatial dynamique (DSDM) — 
        les effets directs, indirects et totaux des migrations sur les conflits armés 
        en Afrique subsaharienne.
        </p>
    """, unsafe_allow_html=True)

    st.markdown(f"""
        <div style='background-color:#F5F0E8; padding:30px; 
        border-left:4px solid {COULEURS["or"]}; border-radius:5px;
        text-align:center; height:300px; display:flex; 
        align-items:center; justify-content:center;'>
        <p style='color:#1A1A2E; font-size:18px;'>
        ⏳ En attente des résultats du modèle DSDM<br>
        <small>sdm_results.csv sera disponible le 12 avril</small>
        </p>
        </div>
    """, unsafe_allow_html=True)