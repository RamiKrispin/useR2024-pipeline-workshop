# A VScode Template for Dockerized R Environment

This Github repository provides a template for a dockerized R development environment with VScode and the Dev Containers extension. It contains the following folders and files:



```shell
.
├── README.md
├── .devcontainer
│   └── devcontainer.json
├── .vscode
│   └── settings.json
├── docker
│   ├── Dockerfile
│   ├── build_docker.sh
│   ├── devcontainer.json
│   ├── install_packages.R
│   ├── install_quarto.sh
│   ├── packages.json
│   └── requirements.txt
└── tests
    ├── app.R
    ├── htmlwidgets.R
    └── plot.R

```

It includes the following folders and files:
- `.devcontainer` - defines the dockerized environment settings with the `devcontainer.json` file
- `.vscode` - enables the modification of the VScode general settings for the dockerized environment with the `settings.json` file
- `docker` - contains the template image settings
- `tests` - R scripts for testing the environment functionality (e.g., Shiny app, static and interactive plots, etc.)

The template default image in the template is `rkrispin/vscode_r_dev:0.1.0`, which comes with R version `4.3.1` and core packages (e.g., `dplyr`, `shiny`, `ggplot2`, `plotly`, etc.). 
