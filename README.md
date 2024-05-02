# SnoWatch - Snow Depth in Switzerland

## Motivation
Climate change and its impact on our environment are a constant topic. Extreme weather conditions are becoming more and more frequent. It is important to give observations a context in order to recognize potential trends. My dashboard focuses on visualizing snow measurements from different locations. The underlying dataset comes from [Kaggle](https://www.kaggle.com/datasets/thedevastator/european-alps-snow-depth-observations) and includes monthly observations from each location.

The target audience of my dashboard are local authorities responsible for environmental monitoring. With the help of the dashboard, they can assess the snow cover conditions in Switzerland. This is relevant information because it can provide insights into water supplies, for example, and thus enable deficiencies to be identified at an early stage. This is because a winter with little precipitation has a direct impact on the available water reserves in summer.


## Demo
My computer slows down quite a bit with Zoom open. The app is much faster when I am not recording.
![Demo video](https://github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_mihunn/blob/master/img/demo.mp4)  


## Installation Instructions

To run the app locally, run the following code in your terminal.

1. Clone the repo to your local machine.

```bash
git clone https://github.ubc.ca/MDS-2023-24/DSCI_532_individual-assignment_mihunn.git
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