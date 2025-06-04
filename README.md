# EasyPlot

**EasyPlot** is a general-purpose R Shiny application for interactive data visualization. It supports a wide variety of plot types and allows flexible parameter control for dynamic exploration of datasets.

![screenshot](screenshot.png) <!-- Optional: include a UI image -->

---

## 🚀 Features

- Interactive plotting with customizable options
- Support for multiple plot types (e.g., bar plots, box/violin plots, grouped bar/box/violin plots, survival plots, histogram/density plots, scatter plots, heatmaps, etc.)
- User-friendly interface with real-time updates
- Faceted plots and statistical overlays
- Dynamic dataset upload and reset functionality
- Theme support via `bslib`

---

## 🌐 Live App

You can access the deployed app here:  
[https://k78x0p-xiaoke-xu.shinyapps.io/deployment/](https://k78x0p-xiaoke-xu.shinyapps.io/deployment/)  

---

## 📁 Directory Structure
EasyPlot/
├── app.R
├── functions.R
├── DESCRIPTION
└── README.md

---

## 🛠️ Installation (For Local Run)

1. Clone the repository:

```bash
git clone https://github.com/macrophage666/EasyPlot
cd EasyPlot

---

2. Open R and install dependencies (if renv is used):
install.packages("renv")
renv::restore()

3. Run the app:
shiny::runApp()

---

📦 Dependencies

Core packages used in the app include:

shiny
ggplot2
dplyr
tidyr
ComplexHeatmap
ggsignif
survminer
RColorBrewer
See DESCRIPTION

🧪 Development Notes

Uses bslib::page_navbar() for multi-tab layout and theming
Uses ComplexHeatmap for advanced heatmap rendering
Dynamic UI reactivity based on uploaded data
Reset button redirects to a specific tab (not the default)

📜 License

This project is licensed under the MIT License. See the LICENSE file for details.

👤 Author

Xiaoke Xu

