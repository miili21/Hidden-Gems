import os
import pandas as pd
import plotly.express as px
import streamlit as st

# -------------------- CONFIGURACIÓN GENERAL --------------------
st.set_page_config(
    page_title="Netflix´s Gems", 
    page_icon="🎬", 
    layout="wide",
    initial_sidebar_state="expanded"
    )

# --- Título y Descripción ---
st.title("Dashboard Interactivo sobre Gemas de Netflix")
st.markdown("""
Bienvenido a este dashboard interactivo para el análisis del catalogo de Netflix.
Utiliza los filtros en la barra lateral para explorar los datos por Genero, Año de lanzamiento, Tipo de produccion y Pais de producción.
""")

# --------- Paleta de colores y tematica --------------------
st.markdown("""
    <style>
    [data-testid="stAppViewContainer"] {
        background-color: #141414;
        color: #FFFFFF;
    }
    [data-testid="stHeader"] {
        background: rgba(0,0,0,0);
    }
    h1, h2, h3, h4 {
        color: #a6050d;
    }
    .stMetric {
        background-color: #181818;
        border-radius: 10px;
        padding: 10px;
    }
    .stTabs [data-baseweb="tab"] {
        background-color: #181818;
        color: #FFFFFF;
        border-radius: 10px;
        margin-right: 5px;
    }
    .stTabs [data-baseweb="tab"]:hover {
        background-color: #E50914;
        color: white;
    }
    </style>
""", unsafe_allow_html=True)

# -------------------- Cargar la data y limpiar los datos --------------------
ruta_BD = os.path.join("Data", "netflix_dataset.csv")
netflix = pd.read_csv(ruta_BD)

# Renombrar columnas
netflix.rename(columns={
    "title": "Titulo",
    "type": "Tipo",
    "genres": "Genero",
    "production_countries": "Pais",
    "runtime": "Duracion",
    "tmdb_popularity": "Popularidad",
    "tmdb_score": "Puntaje",
    "release_year": "Ano",
    "seasons": "Temporadas"
}, inplace=True)

# Limpieza
netflix["Genero"].fillna("No definido", inplace=True)
netflix["Pais"].fillna("No definido", inplace=True)
netflix["Duracion"].fillna(0, inplace=True)
netflix["Popularidad"].fillna(0, inplace=True)
netflix["Puntaje"].fillna(0, inplace=True)
netflix["Temporadas"].fillna(0, inplace=True)

#Arreglar las listas
netflix["Genero"] = netflix["Genero"].str.lower().str.replace("[\\[\\]']", "", regex=True)
netflix["Pais"] = netflix["Pais"].str.lower().str.replace("[\\[\\]']", "", regex=True)

# Expandir listas
netflix = netflix.assign(
    Genero=netflix["Genero"].str.split(",\\s*"),
    Pais=netflix["Pais"].str.split(",\\s*")
).explode("Genero").explode("Pais")

# Agregar la variable de década
netflix["Decada"] = netflix["Ano"].apply(lambda x: f"{x//10*10}s")

# -------------------- Opciones laterales --------------------
with st.sidebar:
    st.header("Filtrea tus gemas")

    # Filtro multi-selección para década
    decadas_unicas = sorted(netflix['Decada'].dropna().unique())
    selected_decadas = st.multiselect(
        "Selecciona que década quieres explorar:",
        options=decadas_unicas,
        default=decadas_unicas
    )

    # Filtro multi-selección para tipo de contenido
    tipos_unicos = sorted(netflix['Tipo'].dropna().unique())
    selected_tipos = st.multiselect(
        "Selecciona Tipo de Contenido quieres reproducir:",
        options=tipos_unicos,
        default=tipos_unicos
    )

    # Filtro multi-selección para géneros
    generos_unicos = sorted(netflix['Genero'].dropna().unique())
    selected_generos = st.multiselect(
        "Selecciona que Géneros quieres ver hoy:",
        options=generos_unicos,
        default=generos_unicos
    )

    # Filtro multi-selección para países
    paises_unicos = sorted(netflix['Pais'].dropna().unique())
    selected_paises = st.multiselect(
        "Selecciona de que Pais quieres ver tu titulo:",
        options=paises_unicos,
        default=paises_unicos
    )

# Filtrar el DataFrame principal según las selecciones
df_filtrado = netflix[
    (netflix['Decada'].isin(selected_decadas)) &
    (netflix['Tipo'].isin(selected_tipos)) &
    (netflix['Genero'].isin(selected_generos)) &
    (netflix['Pais'].isin(selected_paises))
]

if df_filtrado.empty:
    st.warning("No hay datos que cumplan con los filtros seleccionados")

