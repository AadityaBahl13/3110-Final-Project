
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

```bash
dune exec ./main.exe <csv_file>
```

Example:

```bash
dune exec ./main.exe data/points.csv
```

### CSV Format

- Each row should contain a label followed by its feature values.
- Example ((-1, +1) + 2D features):

```
1,2,1
-3,4,0
0,0,1
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
