
# 3110 Final Project: Our Crack At Machine Learning (OCAML)

 **Pattern Matchers:** Jonah Bernard (jb2528), Danielle Imogu (dii3), Aaditya Bahl (ab2844)
## Overview

This project provides an interactive visualization tool for understanding how basic machine learning algorithms — specifically the Perceptron and Decision Tree — operate on 2D data. Written in OCaml using the Bogue GUI toolkit and Cairo rendering, this project reads datasets from CSV files, trains models step-by-step, and visualizes their decision boundaries.

## Features

- **CSV Input**: Load datasets in CSV format for binary classification.
- **Perceptron**:
  - Step-by-step or full training.
  - Visualization of data points, weight vector, and decision boundary.
- **Decision Tree**:
  - Recursive visualization of decision regions.
  - Highlights classification zones for leaf nodes.
- **Graphical Visualization**:
  - Axis rendering.
  - Coordinate transformation to fit any dataset within the canvas.
  - Real-time drawing using SDL via Bogue.

## Project Structure

- `Finalproject.Data`: Handles CSV reading and data preprocessing.
- `Finalproject.Lin_alg`: Provides basic linear algebra operations.
- `Finalproject.Perceptron`: Implements the Perceptron algorithm.
- `Finalproject.Decision_tree`: Builds and traverses decision trees.
- `Bogue` / `Cairo`: Libraries for GUI and rendering.

## Usage

### Run the App

The application's main purpose is to read user inputed data (of any dimension) 
from a CSV file (format specification below), train both a linear (perceptron) 
and non-linear (decision tree) classifier on the data, then prompt the user 
for more points, and finally output predicitions from the two models for those
new points. To do this, 3 arguments must be passed:
1) The file path to the csv file with the training data
2) The maximum steps allowed for the perceptron
3) The maximum depth allowed for the decision tree

However, in the case of 2D data, we offer a way to vizualize the decision 
boundaries that these models create. An optional fourth argument can be 
passed, `g`, that will start a GUI before the text based prediction
interface. The GUI graphs the data points, loaded from the CSV file, and
then gives the options to visualize the perceptron and decsion tree decsion
boundaries. Below is the syntax of the command that will start the 
application.

```bash
dune exec ./main.exe <csv_file> <max_step> <max_depth> <g (optional)>
```

Example:

```bash
dune exec ./main.exe data/valid_linear.csv 100 10 g
```

### CSV Format

- Each row should contain a label followed by its feature values.
- Each feature vector must be of the same dimension.
- Example ((-1, +1) + 2D features):

```
1,2,1
-1,4,0
1,0,1
```

## Dependencies

- [OCaml](https://ocaml.org/)
- [Dune](https://dune.build/)
- [Bogue](https://github.com/sanette/bogue)
- [Cairo](https://cairographics.org/)

Install dependencies via `opam`:

```bash
opam install dune bogue cairo
```

## License

This project is for educational purposes.

---
