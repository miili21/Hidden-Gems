import os
import pandas as pd
import plotly.express as px
import streamlit as st
import seaborn as sns
import matplotlib.pyplot as plt


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

# -------------------- Pestañas --------------------
tab1, tab2, tab3 = st.tabs(["Catálogo", "Análisis de Éxito", "💎 Hidden Gems"])

# ----------- TAB 1: Catalogo -----------
with tab1:
    st.header("Catálogo de Netflix")
    st.write("""
    En este apartado podrás explorar cómo está compuesto el catálogo de Netflix: 
    su distribución por géneros, tipos de contenido, duración y su evolución a lo largo del tiempo.
    """)

    # Métricas principales
    col1, col2, col3 = st.columns(3)
    col1.metric("Total de producciones", netflix.shape[0])
    col2.metric("Popularidad promedio (TMDB)", round(netflix["Popularidad"].mean(), 2))
    col3.metric("Puntaje promedio (TMDB)", round(netflix["Puntaje"].mean(), 2))

    st.subheader("Tabla de datos")
    st.dataframe(netflix[["Titulo", "Tipo", "Genero", "Pais", "Duracion", "Temporadas", "Popularidad", "Puntaje", "Ano"]].head(50))

    # Gráficos resumen
    st.subheader(" Distribución general del catálogo")


    fig_tipo = px.pie(netflix, names="Tipo", title="Distribución por tipo de contenido",
                          color_discrete_sequence=["#E50914", "#B20710"])
    fig_tipo.update_layout(template="plotly_dark")
    st.plotly_chart(fig_tipo, use_container_width=True)

    st.subheader("Evolución en el tiempo")
    fig_tiempo = px.histogram(netflix, x="Ano", color="Tipo", nbins=40,
                              title="Cantidad de producciones por año",
                              color_discrete_sequence=["#E50914", "#B20710"])
    fig_tiempo.update_layout(template="plotly_dark")
    st.plotly_chart(fig_tiempo, use_container_width=True)

    st.subheader("Distribución del número de temporadas (Series)")
    fig_temp = px.box(netflix[netflix["Tipo"] == "SHOW"], y="Temporadas",
                      title="Distribución del número de temporadas",
                      color_discrete_sequence=["#E50914"])
    fig_temp.update_layout(template="plotly_dark")
    st.plotly_chart(fig_temp, use_container_width=True)

    st.subheader("Análisis de Correlaciones entre Variables (Duración, Popularidad, Puntaje, Temporadas)")

    # Seleccionamos las variables numéricas relevantes
    corr_vars = netflix[["Duracion", "Popularidad", "Puntaje", "Temporadas"]].corr()

    # Creamos el heatmap
    fig_corr, ax = plt.subplots(figsize=(6, 4))
    sns.heatmap(corr_vars, annot=True, cmap="Reds", fmt=".2f", linewidths=0.5, ax=ax)
    ax.set_title("Matriz de Correlación", color="white", fontsize=12)
    fig_corr.patch.set_facecolor('#ffffff')
    ax.set_facecolor('#ffffff')
    st.pyplot(fig_corr)

    st.write(
        """-Relación entre Popularidad y Puntaje
    Se observa una correlación positiva moderada (aproximadamente entre 0.45 y 0.6 dependiendo de la muestra).
    Esto significa que las producciones con mejores puntuaciones de crítica tienden a ser más populares, aunque no siempre ocurre.""")
    st.write("""
            -Relación entre Duración y Popularidad
    La correlación fue baja o cercana a cero, lo que indica que la duración de una película o serie no influye 
    directamente en su popularidad. Tanto películas cortas como largas pueden tener buena o mala recepción, dependiendo más de 
    su contenido que de su extensión.""")
    st.write("""
            -Relación entre Duración y Puntaje
    También se encontró una correlación débil. Esto refuerza la idea de que la duración no determina la calidad 
    percibida.""")
    st.write("""
            -Relación entre Temporadas y Popularidad (solo series)

    Aquí suele aparecer una correlación positiva leve, indicando que las series con más temporadas tienden a mantener
    mayor popularidad. Esto puede deberse a que las series más exitosas se renuevan, acumulando tanto popularidad como temporadas.""")

# ----------- TAB 2: Analisis de Exito-----------
with tab2:
    st.header("Análisis de Éxito")
    st.write("""
    Aquí se analiza qué tan exitoso es el contenido de Netflix a través de su popularidad, duración y su evolución en el tiempo.
    """)

    st.subheader("Distribución de popularidad por género")
    fig_pop_gen = px.box(netflix, x="Genero", y="Popularidad", color="Genero",
                         title="Distribución de popularidad por género")
    fig_pop_gen.update_layout(template="plotly_dark", showlegend=False)
    st.plotly_chart(fig_pop_gen, use_container_width=True)

    col1, col2 = st.columns(2)
    with col1:
        st.subheader("Duración promedio por tipo")
        fig_dur_tipo = px.bar(netflix.groupby("Tipo")["Duracion"].mean().reset_index(),
                              x="Tipo", y="Duracion", color="Tipo",
                              title="Duración promedio por tipo",
                              color_discrete_sequence=["#E50914", "#B20710"])
        fig_dur_tipo.update_layout(template="plotly_dark")
        st.plotly_chart(fig_dur_tipo, use_container_width=True)

    with col2:
        st.subheader("Popularidad Promedio por Tipo de Contenido")
        fig_dur_tipo = px.bar(netflix.groupby("Tipo")["Popularidad"].mean().reset_index(),
                              x="Tipo", y="Popularidad", color="Tipo",
                              title="Popularidad promedio por tipo",
                              color_discrete_sequence=["#E50914", "#B20710"])
        fig_dur_tipo.update_layout(template="plotly_dark")
        st.plotly_chart(fig_dur_tipo, use_container_width=True)
        
    st.subheader(" Popularidad por país y década")
    pop_pais = netflix.groupby(["Decada", "Pais"]).agg(popularidad_promedio=("Popularidad", "mean"),
                                                       cantidad=("Titulo", "count")).reset_index()
    fig_burbujas = px.scatter(pop_pais, x="Decada", y="Pais", size="cantidad",
                              color="popularidad_promedio",
                              color_continuous_scale="Reds",
                              title="Popularidad promedio por país y década")
    fig_burbujas.update_layout(template="plotly_dark")
    st.plotly_chart(fig_burbujas, use_container_width=True)
