# Elisita-CreenciaActualizada

**Elisita-CreenciaActualizada** es una aplicación Shiny que facilita formalizar el conocimiento de persona experta en **distribuciones probabilísticas**. Combina las opiniones de persona experta **(consenso)** y **actualizar sus creencias** después de observar los **datos** mediante **métodos bayesianos** utilizando RStan.


## Descripción

Permite:

1. El ingreso de datos de persona experta (mínimos, máximos, medias) manualmente o archivo *.xlsx*.
2. Convierte la opinión de la persona experta en distribuciones probabilísticas (Beta, lognormal, Normal, Triangular, Poisson, etc.).  
3. Pondera las opiniones según los años de experiencia de la persona experta.
4. Combina las opiniones de múltiples personas expertas en una distribución agregada (consenso).  
5. Combina la distribución de las persona experta (prior) con datos observados (verosimilitud) usando **Stan/MCMC** para actualizar sus creencias.  


## Instalación

La aplicación Elisita-CreenciaActualizada está configurada para ejecutarse dentro de contenedores Docker.

1. Clonar el repositorio:

git clone https://github.com/SVMendoza/Elisita-CreenciaActualizada

cd elisita

2. Construir la imagen y ejecutar con Docker:

docker build -t elisita

docker run -d -p 3838:3838 elisita

3. Ejecutar
   
http://localhost:3838


## Uso 

  
![Ingreso de información de persona(s) experta(s)](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash1.png)

![Visualización de la distribución](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash2.png)

![Resultados elicitación](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash3.png)

![Cargar datos y actualizar distribucíon de verosimilitud](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenesdash4.png)

![Configurar para actualizar la creencia]https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash5.png)

![Resultados gráfico](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash6.png)

![Estimación](https://github.com/SVMendoza/Elisita-CreenciaActualizada/tree/main/imagenes/dash7.png)


### Créditos
Idea y desarrollo: Sergio J. Vilchez-Mendoza

Sugerencias de código: Deepseek y ChatGPT (OpenAI)

## Licencia
Este proyecto está licenciado bajo MIT License. Ver archivo LICENSE para detalles.
