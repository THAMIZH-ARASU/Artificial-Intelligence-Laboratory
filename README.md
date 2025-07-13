# 🤖 Artificial Intelligence Laboratory Exercises

This repository contains a comprehensive collection of Artificial Intelligence laboratory exercises covering various AI algorithms, techniques, and applications. Each program demonstrates different aspects of AI including search algorithms, game playing, natural language processing, and rule-based systems.

## 📚 Table of Contents

- [Overview](#overview)
- [Programs](#programs)
- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Technical Details](#technical-details)
- [Contributing](#contributing)

## 🎯 Overview

This laboratory collection demonstrates fundamental AI concepts through practical implementations:

- **Search Algorithms**: BFS, DFS, A* Search, AO* Algorithm
- **Game Playing**: 8-Puzzle with AI heuristics
- **Natural Language Processing**: English grammar checking
- **Rule-Based Systems**: Smart home automation
- **Heuristic Functions**: Various optimization techniques

## 🚀 Programs

### 1. **BFS (8-Puzzle Problem)** - `1-BFS (for 8 puzzle problem).py`
**Algorithm**: Breadth-First Search  
**Problem**: 8-Puzzle solving using BFS algorithm

**Features:**
- Complete state space exploration
- Guaranteed optimal solution
- Visual puzzle representation
- Step-by-step solution path
- Performance metrics

**Usage:**
```bash
python "1-BFS (for 8 puzzle problem).py"
```

### 2. **DFS (Water Jug Problem)** - `2-DFS(water jug problem).py`
**Algorithm**: Depth-First Search  
**Problem**: Water jug problem using DFS

**Features:**
- State space tree visualization
- Solution path tracking
- Memory-efficient implementation
- Problem-specific heuristics

**Usage:**
```bash
python "2-DFS(water jug problem).py"
```

### 3. **A* Implementation** - `3-A-Star (Implementation).py`
**Algorithm**: A* Search Algorithm  
**Problem**: General A* implementation with heuristics

**Features:**
- Configurable heuristic functions
- Priority queue implementation
- Path reconstruction
- Performance analysis
- Visual state exploration

**Usage:**
```bash
python "3-A-Star (Implementation).py"
```

### 4. **AO* Algorithm** - `4-AO-Star Algorithm.py`
**Algorithm**: AO* (And-Or) Search Algorithm  
**Problem**: AND-OR graph problem solving

**Features:**
- AND-OR graph representation
- Cost evaluation
- Solution tree construction
- Backward chaining
- Optimal solution finding

**Usage:**
```bash
python "4-AO-Star Algorithm.py"
```

### 5. **Single Player Game (Heuristic Function)** - `5-SinglePlayerGame(using heuristic function).py`
**Algorithm**: A* with Heuristics  
**Problem**: 8-Puzzle game with AI assistance

**Features:**
- 🎮 Interactive GUI with modern design
- 🤖 AI-powered puzzle solving
- 💡 Intelligent hint system
- 🎯 Multiple heuristic functions (Manhattan Distance, Misplaced Tiles)
- 🏆 Auto-solve functionality
- 📊 Real-time statistics
- 🎨 Modern dark theme UI

**Key Heuristics:**
- **Manhattan Distance**: Sum of distances from current to goal positions
- **Misplaced Tiles**: Count of tiles not in correct position
- **Combined Heuristic**: f(n) = g(n) + h(n)

**Usage:**
```bash
python "5-SinglePlayerGame(using heuristic function).py"
```

### 6. **English Grammar Checker** - `9-SyntaxCheckingOfEnglishSentence.py`
**Algorithm**: Rule-Based NLP with NLTK  
**Problem**: English grammar and syntax checking

**Features:**
- 📝 Advanced grammar analysis
- 🔍 Multiple error detection types
- 💡 Intelligent suggestions
- 📊 Text statistics and readability
- 🎨 Modern tabbed interface
- 🔄 Real-time processing
- 📋 Comprehensive logging

**Grammar Checks:**
- Capitalization errors
- Punctuation issues
- Subject-verb agreement
- Article usage (a/an)
- Word repetition
- Sentence structure
- Common mistakes
- Spelling patterns

**Usage:**
```bash
python "9-SyntaxCheckingOfEnglishSentence.py"
```

### 7. **Smart Home Automation** - `RuleBasedv2.py`
**Algorithm**: Rule-Based System  
**Problem**: Smart home automation with sensors and devices

**Features:**
- 🏠 Complete smart home simulation
- 📡 Multiple sensor types (Temperature, Motion, Light, Occupancy)
- 💡 Smart devices (Lights, Thermostats, Security Cameras)
- ⚙️ Configurable automation rules
- 🎮 Modern GUI with real-time monitoring
- 📊 System status dashboard
- 🔄 Automatic and manual control modes
- 📋 Comprehensive logging system

**Sensor Types:**
- Temperature sensors
- Motion detectors
- Light sensors
- Occupancy sensors

**Device Types:**
- Smart lights with brightness/color control
- Thermostats with temperature control
- Security cameras with recording

**Automation Rules:**
- Temperature control rules
- Motion-activated lighting
- Security monitoring
- Ambient lighting control

**Usage:**
```bash
python "RuleBasedv2.py"
```

## 🛠️ Installation

### Prerequisites
- Python 3.7 or higher
- tkinter (usually included with Python)
- NLTK (for grammar checker)

### Setup Instructions

1. **Clone or download the repository**
```bash
git clone <repository-url>
cd AI-Lab-Exercises
```

2. **Install required packages**
```bash
pip install nltk
```

3. **Download NLTK data (for grammar checker)**
```python
import nltk
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('stopwords')
nltk.download('wordnet')
```

## 🎮 Usage

### Running Individual Programs

Each program can be run independently:

```bash
# Run 8-Puzzle with BFS
python "1-BFS (for 8 puzzle problem).py"

# Run Water Jug with DFS
python "2-DFS(water jug problem).py"

# Run A* Implementation
python "3-A-Star (Implementation).py"

# Run AO* Algorithm
python "4-AO-Star Algorithm.py"

# Run 8-Puzzle Game with AI
python "5-SinglePlayerGame(using heuristic function).py"

# Run Grammar Checker
python "9-SyntaxCheckingOfEnglishSentence.py"

# Run Smart Home Automation
python "RuleBasedv2.py"
```

### GUI Programs

The following programs feature modern graphical interfaces:

1. **8-Puzzle Game** - Interactive puzzle solving with AI assistance
2. **Grammar Checker** - Advanced text analysis with suggestions
3. **Smart Home Automation** - Complete home automation simulation

## ✨ Features

### 🧠 AI Algorithms Implemented

- **Breadth-First Search (BFS)**
  - Complete state exploration
  - Optimal solution guarantee
  - Memory-intensive but thorough

- **Depth-First Search (DFS)**
  - Memory-efficient exploration
  - Quick solution finding
  - Suitable for deep problems

- **A* Search Algorithm**
  - Heuristic-guided search
  - Optimal solution with admissible heuristics
  - Efficient for complex problems

- **AO* Algorithm**
  - AND-OR graph problem solving
  - Cost evaluation and optimization
  - Backward chaining implementation

### 🎯 Heuristic Functions

- **Manhattan Distance**: Sum of absolute differences
- **Misplaced Tiles**: Count of incorrect positions
- **Euclidean Distance**: Geometric distance calculation
- **Linear Conflicts**: Advanced heuristic for 8-puzzle

### 🎮 Interactive Features

- **Real-time Updates**: Live data monitoring
- **Visual Feedback**: Color-coded status indicators
- **Progress Tracking**: Step-by-step solution visualization
- **Statistics Display**: Performance metrics and analysis

### 📊 Analysis Tools

- **Performance Metrics**: Time complexity analysis
- **Memory Usage**: Space complexity tracking
- **Solution Quality**: Optimality verification
- **Algorithm Comparison**: Side-by-side performance

## 🔧 Technical Details

### Architecture

- **Modular Design**: Each program is self-contained
- **Object-Oriented**: Clean class-based implementations
- **GUI Framework**: tkinter for modern interfaces
- **Threading**: Non-blocking operations for responsive UI

### Data Structures

- **Priority Queues**: For A* and AO* algorithms
- **Graph Representations**: For state space exploration
- **Tree Structures**: For solution path tracking
- **Hash Tables**: For efficient state storage

### Algorithms Complexity

| Algorithm | Time Complexity | Space Complexity | Optimality |
|-----------|----------------|------------------|------------|
| BFS | O(b^d) | O(b^d) | ✅ Optimal |
| DFS | O(b^m) | O(b*m) | ❌ Not Optimal |
| A* | O(b^d) | O(b^d) | ✅ Optimal |
| AO* | O(b^d) | O(b^d) | ✅ Optimal |

### GUI Features

- **Responsive Design**: Adapts to different screen sizes
- **Dark Theme**: Modern color scheme
- **Tabbed Interface**: Organized content sections
- **Real-time Updates**: Live data refresh
- **Export Capabilities**: Save results and logs

## 📈 Learning Outcomes

### AI Concepts Covered

1. **Search Algorithms**
   - Uninformed search (BFS, DFS)
   - Informed search (A*, AO*)
   - Heuristic function design
   - State space representation

2. **Game Playing**
   - Adversarial search
   - Heuristic evaluation
   - Move generation
   - Solution optimization

3. **Natural Language Processing**
   - Grammar checking
   - Error detection
   - Text analysis
   - Rule-based systems

4. **Rule-Based Systems**
   - Knowledge representation
   - Inference engines
   - Automation logic
   - Real-time processing

### Practical Skills Developed

- **Algorithm Implementation**: Converting theory to code
- **GUI Development**: Creating user-friendly interfaces
- **System Design**: Modular and scalable architectures
- **Performance Analysis**: Algorithm efficiency evaluation
- **Problem Solving**: Real-world AI applications

## 🤝 Contributing

### Adding New Programs

1. Follow the existing naming convention
2. Include comprehensive documentation
3. Add appropriate GUI if applicable
4. Test thoroughly before submission

### Code Standards

- **Python PEP 8**: Follow style guidelines
- **Documentation**: Include docstrings and comments
- **Error Handling**: Proper exception management
- **User Experience**: Intuitive interface design

## 📝 License

This project is created for educational purposes as part of Artificial Intelligence laboratory exercises.

## 👨‍💻 Author

Created for AI Laboratory Exercises - 7th Semester

---

## 🎓 Academic Context

This collection serves as a comprehensive laboratory manual for Artificial Intelligence course, covering:

- **Search and Optimization**: Fundamental AI algorithms
- **Game Theory**: Adversarial and cooperative scenarios
- **Natural Language Processing**: Text analysis and understanding
- **Rule-Based Systems**: Knowledge representation and reasoning
- **Heuristic Design**: Problem-specific optimization techniques

Each program demonstrates practical implementation of theoretical AI concepts, providing hands-on experience with real-world applications. 