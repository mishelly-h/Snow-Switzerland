# SnoWatch - Snow Depth in Switzerland

## Motivation
Climate change and its impact on our environment are a constant topic. Extreme weather conditions are becoming more and more frequent. It is important to give observations a context in order to recognize potential trends. My dashboard focuses on visualizing snow measurements from different locations. The underlying dataset comes from [Kaggle](https://www.kaggle.com/datasets/thedevastator/european-alps-snow-depth-observations) and includes monthly observations from each location.


## Demo
![Demo video](https://github.com/mishelly-h/snowfall_switzerland/blob/main/img/demo.gif)  


## Installation Instructions

To run the app locally, run the following code in your terminal.

1. Clone the repo to your local machine.

```bash
git clone https://github.com/mishelly-h/snowfall_switzerland.git
```
2. Step into the root directory of the repo you have just cloned.

3. Create the environment.

```bash
conda env create --file environment.yaml
```
4. Activate the environment.

```bash
conda activate snowy_environment
```

5. Run the app.

```bash
R -e "shiny::runApp('src/app.R')"
```