import tkinter as tk
from tkinter import ttk, scrolledtext, messagebox
import re
import threading
import queue
import os
import sys

# Try to import NLTK, but create fallback if it fails
try:
    import nltk
    from nltk.tokenize import word_tokenize, sent_tokenize
    from nltk.tag import pos_tag
    from nltk.corpus import stopwords
    from nltk.stem import WordNetLemmatizer
    NLTK_AVAILABLE = True
    
    # Function to safely download NLTK data
    def download_nltk_data():
        try:
            # Set NLTK data path to user directory
            nltk_data_dir = os.path.join(os.path.expanduser('~'), 'nltk_data')
            if not os.path.exists(nltk_data_dir):
                os.makedirs(nltk_data_dir)
            
            # Download required data
            required_data = ['punkt', 'averaged_perceptron_tagger', 'stopwords', 'wordnet']
            for data in required_data:
                try:
                    nltk.data.find(f'tokenizers/{data}' if data == 'punkt' else 
                                 f'taggers/{data}' if data == 'averaged_perceptron_tagger' else 
                                 f'corpora/{data}')
                except LookupError:
                    print(f"Downloading {data}...")
                    nltk.download(data, download_dir=nltk_data_dir)
            return True
        except Exception as e:
            print(f"Error downloading NLTK data: {e}")
            return False
    
    # Try to download NLTK data
    if not download_nltk_data():
        NLTK_AVAILABLE = False
        print("NLTK data download failed. Using fallback grammar checker.")

except ImportError:
    NLTK_AVAILABLE = False
    print("NLTK not available. Using fallback grammar checker.")

class FallbackGrammarChecker:
    """Fallback grammar checker that doesn't require NLTK"""
    
    def __init__(self):
        self.common_words = set(['the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at', 'to', 'for', 'of', 'with', 'by'])
        
    def check_grammar(self, text):
        """Grammar checking without NLTK dependency"""
        errors = []
        suggestions = []
        
        # Simple sentence splitting
        sentences = self._split_sentences(text)
        
        for i, sentence in enumerate(sentences):
            if sentence.strip():
                sentence_errors = []
                
                # Check various grammar rules
                sentence_errors.extend(self._check_capitalization(sentence, i))
                sentence_errors.extend(self._check_punctuation(sentence, i))
                sentence_errors.extend(self._check_basic_agreement(sentence, i))
                sentence_errors.extend(self._check_article_usage(sentence, i))
                sentence_errors.extend(self._check_word_repetition(sentence, i))
                sentence_errors.extend(self._check_sentence_structure(sentence, i))
                sentence_errors.extend(self._check_common_mistakes(sentence, i))
                sentence_errors.extend(self._check_spelling_patterns(sentence, i))
                
                errors.extend(sentence_errors)
                
                # Generate suggestions
                if sentence_errors:
                    suggestions.extend(self._generate_suggestions(sentence, sentence_errors))
        
        return errors, suggestions
    
    def _split_sentences(self, text):
        """Simple sentence splitting"""
        # Split on sentence endings
        sentences = re.split(r'[.!?]+', text)
        return [s.strip() for s in sentences if s.strip()]
    
    def _check_capitalization(self, sentence, sent_idx):
        """Check capitalization rules"""
        errors = []
        
        # Check if sentence starts with capital letter
        if sentence and not sentence[0].isupper():
            errors.append({
                'type': 'Capitalization',
                'message': 'Sentence should start with a capital letter',
                'sentence': sent_idx,
                'position': 0
            })
        
        # Check proper nouns
        days = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday']
        months = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
        proper_nouns = days + months + ['english', 'american', 'british', 'french', 'german', 'spanish', 'italian', 'chinese', 'japanese']
        
        words = sentence.lower().split()
        for word in words:
            clean_word = re.sub(r'[^\w]', '', word)
            if clean_word in proper_nouns:
                errors.append({
                    'type': 'Capitalization',
                    'message': f'"{clean_word}" should be capitalized (proper noun)',
                    'sentence': sent_idx,
                    'word': clean_word
                })
        
        return errors
    
    def _check_punctuation(self, sentence, sent_idx):
        """Check punctuation rules"""
        errors = []
        
        # Check if sentence ends with proper punctuation (this is for display, actual ending punct might be stripped)
        if sentence and sentence[-1] not in '.!?':
            errors.append({
                'type': 'Punctuation',
                'message': 'Sentence should end with proper punctuation (., !, or ?)',
                'sentence': sent_idx,
                'position': len(sentence)
            })
        
        # Check for multiple spaces
        if re.search(r'\s{2,}', sentence):
            errors.append({
                'type': 'Spacing',
                'message': 'Multiple spaces found - should be single space',
                'sentence': sent_idx
            })
        
        # Check for space before punctuation
        if re.search(r'\s+[.!?,:;]', sentence):
            errors.append({
                'type': 'Punctuation',
                'message': 'Remove space before punctuation',
                'sentence': sent_idx
            })
        
        # Check for missing space after punctuation
        if re.search(r'[.!?,:;]\w', sentence):
            errors.append({
                'type': 'Punctuation',
                'message': 'Add space after punctuation',
                'sentence': sent_idx
            })
        
        return errors
    
    def _check_basic_agreement(self, sentence, sent_idx):
        """Check basic subject-verb agreement patterns"""
        errors = []
        
        # Common agreement patterns
        patterns = [
            (r'\b(I|we|they|you)\s+is\b', 'Use "am" with "I", "are" with "we/they/you"'),
            (r'\b(he|she|it)\s+are\b', 'Use "is" with "he/she/it"'),
            (r'\bI\s+are\b', 'Use "I am" instead of "I are"'),
            (r'\b(you|we|they)\s+was\b', 'Use "were" with "you/we/they"'),
            (r'\b(he|she|it)\s+were\b', 'Use "was" with "he/she/it"'),
            (r'\b(I|he|she|it)\s+have\b', 'Use "has" with "he/she/it", "have" with "I"'),
            (r'\b(we|you|they)\s+has\b', 'Use "have" with "we/you/they"'),
        ]
        
        for pattern, message in patterns:
            if re.search(pattern, sentence, re.IGNORECASE):
                errors.append({
                    'type': 'Subject-Verb Agreement',
                    'message': message,
                    'sentence': sent_idx
                })
        
        return errors
    
    def _check_article_usage(self, sentence, sent_idx):
        """Check article usage (a, an, the)"""
        errors = []
        
        # Check "a" vs "an"
        if re.search(r'\ba\s+[aeiouAEIOU]', sentence):
            errors.append({
                'type': 'Article Usage',
                'message': 'Use "an" before words starting with vowel sounds',
                'sentence': sent_idx
            })
        
        if re.search(r'\ban\s+[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]', sentence):
            errors.append({
                'type': 'Article Usage',
                'message': 'Use "a" before words starting with consonant sounds',
                'sentence': sent_idx
            })
        
        return errors
    
    def _check_word_repetition(self, sentence, sent_idx):
        """Check for repeated words"""
        errors = []
        
        words = sentence.lower().split()
        for i in range(len(words) - 1):
            word1 = re.sub(r'[^\w]', '', words[i])
            word2 = re.sub(r'[^\w]', '', words[i + 1])
            if word1 == word2 and word1 not in self.common_words and len(word1) > 2:
                errors.append({
                    'type': 'Word Repetition',
                    'message': f'Repeated word: "{word1}"',
                    'sentence': sent_idx
                })
        
        return errors
    
    def _check_sentence_structure(self, sentence, sent_idx):
        """Check basic sentence structure"""
        errors = []
        
        words = sentence.strip().split()
        
        # Check for very short sentences
        if len(words) < 3:
            errors.append({
                'type': 'Sentence Structure',
                'message': 'Sentence might be too short (less than 3 words)',
                'sentence': sent_idx
            })
        
        # Check for very long sentences
        if len(words) > 35:
            errors.append({
                'type': 'Sentence Structure',
                'message': 'Sentence might be too long - consider breaking it into smaller sentences',
                'sentence': sent_idx
            })
        
        # Check for sentences starting with conjunctions
        if re.match(r'^\s*(and|but|or|so|yet|for|nor)\s+', sentence, re.IGNORECASE):
            errors.append({
                'type': 'Sentence Structure',
                'message': 'Consider avoiding starting sentences with conjunctions',
                'sentence': sent_idx
            })
        
        return errors
    
    def _check_common_mistakes(self, sentence, sent_idx):
        """Check for common grammar mistakes"""
        errors = []
        
        common_mistakes = [
            (r'\byour\s+welcome\b', 'Use "you\'re welcome" instead of "your welcome"'),
            (r'\bits\s+going\b', 'Use "it\'s going" instead of "its going"'),
            (r'\btheir\s+going\b', 'Use "they\'re going" instead of "their going"'),
            (r'\bwould\s+of\b', 'Use "would have" instead of "would of"'),
            (r'\bshould\s+of\b', 'Use "should have" instead of "should of"'),
            (r'\bcould\s+of\b', 'Use "could have" instead of "could of"'),
            (r'\bthere\s+is\s+\w+\s+\w+s\b', 'Use "there are" with plural nouns'),
            (r'\bto\s+much\b', 'Use "too much" instead of "to much"'),
            (r'\blose\s+weight\b', 'Correct usage (not "loose weight")'),
            (r'\bloose\s+weight\b', 'Use "lose weight" instead of "loose weight"'),
            (r'\beffect\s+on\b', 'Consider "affect" (verb) vs "effect" (noun)'),
            (r'\baccept\s+for\b', 'Use "except for" instead of "accept for"'),
        ]
        
        for pattern, message in common_mistakes:
            if re.search(pattern, sentence, re.IGNORECASE):
                errors.append({
                    'type': 'Common Mistake',
                    'message': message,
                    'sentence': sent_idx
                })
        
        return errors
    
    def _check_spelling_patterns(self, sentence, sent_idx):
        """Check for common spelling patterns and errors"""
        errors = []
        
        # Common spelling mistakes
        spelling_patterns = [
            (r'\bteh\b', 'Did you mean "the"?'),
            (r'\brecieve\b', 'Use "receive" (i before e except after c)'),
            (r'\bbelive\b', 'Use "believe"'),
            (r'\bwent\s+to\s+school\s+everyday\b', 'Use "every day" (two words) for frequency'),
            (r'\balot\b', 'Use "a lot" (two words)'),
            (r'\bthier\b', 'Use "their"'),
            (r'\bwere\s+going\s+to\b', 'Consider "we\'re going to" if you mean "we are"'),
        ]
        
        for pattern, message in spelling_patterns:
            if re.search(pattern, sentence, re.IGNORECASE):
                errors.append({
                    'type': 'Spelling/Usage',
                    'message': message,
                    'sentence': sent_idx
                })
        
        return errors
    
    def _generate_suggestions(self, sentence, errors):
        """Generate suggestions for improvement"""
        suggestions = []
        
        error_types = set(error['type'] for error in errors)
        
        if 'Capitalization' in error_types:
            suggestions.append("Remember to capitalize the first letter of sentences and proper nouns")
        if 'Punctuation' in error_types:
            suggestions.append("Check punctuation marks and spacing around them")
        if 'Subject-Verb Agreement' in error_types:
            suggestions.append("Ensure subjects and verbs agree in number (singular/plural)")
        if 'Article Usage' in error_types:
            suggestions.append("Use 'a' before consonant sounds and 'an' before vowel sounds")
        if 'Word Repetition' in error_types:
            suggestions.append("Avoid repeating the same word unnecessarily")
        if 'Sentence Structure' in error_types:
            suggestions.append("Consider sentence length and variety for better readability")
        if 'Common Mistake' in error_types:
            suggestions.append("Review commonly confused words and phrases")
        if 'Spelling/Usage' in error_types:
            suggestions.append("Double-check spelling and word usage")
        
        return suggestions

class NLTKGrammarChecker:
    """Enhanced grammar checker using NLTK"""
    
    def __init__(self):
        self.lemmatizer = WordNetLemmatizer()
        self.stop_words = set(stopwords.words('english'))
        self.fallback_checker = FallbackGrammarChecker()
        
    def check_grammar(self, text):
        """Grammar checking with NLTK"""
        try:
            errors = []
            suggestions = []
            
            # Tokenize into sentences
            sentences = sent_tokenize(text)
            
            for i, sentence in enumerate(sentences):
                if sentence.strip():
                    sentence_errors = []
                    
                    # Use NLTK for more advanced checking
                    sentence_errors.extend(self._check_capitalization(sentence, i))
                    sentence_errors.extend(self._check_punctuation(sentence, i))
                    sentence_errors.extend(self._check_pos_agreement(sentence, i))
                    sentence_errors.extend(self._check_article_usage(sentence, i))
                    sentence_errors.extend(self._check_word_repetition(sentence, i))
                    sentence_errors.extend(self._check_sentence_structure(sentence, i))
                    sentence_errors.extend(self._check_common_mistakes(sentence, i))
                    
                    errors.extend(sentence_errors)
                    
                    # Generate suggestions
                    if sentence_errors:
                        suggestions.extend(self._generate_suggestions(sentence, sentence_errors))
            
            return errors, suggestions
            
        except Exception as e:
            print(f"NLTK checker failed: {e}, using fallback")
            return self.fallback_checker.check_grammar(text)
    
    def _check_capitalization(self, sentence, sent_idx):
        """Check capitalization with NLTK POS tagging"""
        errors = []
        
        # Basic capitalization check
        if sentence and not sentence[0].isupper():
            errors.append({
                'type': 'Capitalization',
                'message': 'Sentence should start with a capital letter',
                'sentence': sent_idx,
                'position': 0
            })
        
        # Use POS tagging to find proper nouns
        try:
            tokens = word_tokenize(sentence)
            pos_tags = pos_tag(tokens)
            
            for word, pos in pos_tags:
                if pos in ['NNP', 'NNPS'] and word.islower():
                    errors.append({
                        'type': 'Capitalization',
                        'message': f'"{word}" appears to be a proper noun and should be capitalized',
                        'sentence': sent_idx,
                        'word': word
                    })
        except:
            pass
        
        return errors
    
    def _check_punctuation(self, sentence, sent_idx):
        """Enhanced punctuation checking"""
        return self.fallback_checker._check_punctuation(sentence, sent_idx)
    
    def _check_pos_agreement(self, sentence, sent_idx):
        """Check agreement using POS tags"""
        errors = []
        
        try:
            tokens = word_tokenize(sentence)
            pos_tags = pos_tag(tokens)
            
            # Find subject-verb patterns
            for i in range(len(pos_tags) - 1):
                word, pos = pos_tags[i]
                next_word, next_pos = pos_tags[i + 1]
                
                # Check for plural subject with singular verb
                if pos == 'NNS' and next_pos == 'VBZ':
                    errors.append({
                        'type': 'Subject-Verb Agreement',
                        'message': f'Plural subject "{word}" should use plural verb form',
                        'sentence': sent_idx
                    })
                
                # Check for singular subject with plural verb
                if pos == 'NN' and next_pos == 'VBP' and next_word.lower() not in ['am', 'are']:
                    errors.append({
                        'type': 'Subject-Verb Agreement',
                        'message': f'Singular subject "{word}" should use singular verb form',
                        'sentence': sent_idx
                    })
        except:
            # Fall back to pattern matching
            errors.extend(self.fallback_checker._check_basic_agreement(sentence, sent_idx))
        
        return errors
    
    def _check_article_usage(self, sentence, sent_idx):
        """Enhanced article checking"""
        return self.fallback_checker._check_article_usage(sentence, sent_idx)
    
    def _check_word_repetition(self, sentence, sent_idx):
        """Check repetition with lemmatization"""
        errors = []
        
        try:
            tokens = word_tokenize(sentence.lower())
            lemmatized = [self.lemmatizer.lemmatize(token) for token in tokens]
            
            for i in range(len(lemmatized) - 1):
                if (lemmatized[i] == lemmatized[i + 1] and 
                    lemmatized[i] not in self.stop_words and 
                    len(lemmatized[i]) > 2):
                    errors.append({
                        'type': 'Word Repetition',
                        'message': f'Repeated word: "{tokens[i]}"',
                        'sentence': sent_idx
                    })
        except:
            errors.extend(self.fallback_checker._check_word_repetition(sentence, sent_idx))
        
        return errors
    
    def _check_sentence_structure(self, sentence, sent_idx):
        """Enhanced sentence structure checking"""
        return self.fallback_checker._check_sentence_structure(sentence, sent_idx)
    
    def _check_common_mistakes(self, sentence, sent_idx):
        """Enhanced common mistake checking"""
        return self.fallback_checker._check_common_mistakes(sentence, sent_idx)
    
    def _generate_suggestions(self, sentence, errors):
        """Generate enhanced suggestions"""
        return self.fallback_checker._generate_suggestions(sentence, errors)

class ModernGrammarCheckerGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("English Grammar Checker")
        self.root.geometry("1000x700")
        self.root.configure(bg='#f0f0f0')
        
        # Initialize grammar checker based on availability
        if NLTK_AVAILABLE:
            try:
                self.grammar_checker = NLTKGrammarChecker()
                self.checker_type = "NLTK Enhanced"
            except:
                self.grammar_checker = FallbackGrammarChecker()
                self.checker_type = "Fallback"
        else:
            self.grammar_checker = FallbackGrammarChecker()
            self.checker_type = "Fallback"
        
        # Create styles
        self.setup_styles()
        
        # Create GUI elements
        self.create_widgets()
        
        # Queue for threading
        self.queue = queue.Queue()
        
    def setup_styles(self):
        """Setup modern styling"""
        style = ttk.Style()
        style.theme_use('clam')
        
        # Configure colors
        style.configure('Title.TLabel', font=('Arial', 16, 'bold'), background='#f0f0f0')
        style.configure('Modern.TButton', font=('Arial', 10), padding=10)
        style.configure('Error.TLabel', foreground='red', font=('Arial', 9))
        style.configure('Success.TLabel', foreground='green', font=('Arial', 9))
        style.configure('Info.TLabel', foreground='blue', font=('Arial', 8))
        
    def create_widgets(self):
        """Create all GUI widgets"""
        # Main frame
        main_frame = ttk.Frame(self.root, padding="20")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Configure grid weights
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)
        main_frame.columnconfigure(1, weight=1)
        main_frame.rowconfigure(2, weight=1)
        
        # Title
        title_label = ttk.Label(main_frame, text="English Grammar Checker", style='Title.TLabel')
        title_label.grid(row=0, column=0, columnspan=3, pady=(0, 10))
        
        # Checker type info
        info_label = ttk.Label(main_frame, text=f"Using {self.checker_type} Checker", style='Info.TLabel')
        info_label.grid(row=1, column=0, columnspan=3, pady=(0, 20))
        
        # Input section
        input_frame = ttk.LabelFrame(main_frame, text="Enter Text to Check", padding="10")
        input_frame.grid(row=2, column=0, columnspan=3, sticky=(tk.W, tk.E), pady=(0, 20))
        input_frame.columnconfigure(0, weight=1)
        
        # Text input
        self.text_input = scrolledtext.ScrolledText(input_frame, height=8, width=80, font=('Arial', 11))
        self.text_input.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        input_frame.rowconfigure(0, weight=1)
        
        # Placeholder text
        placeholder_text = "Enter your text here to check for grammar errors..."
        self.text_input.insert(tk.END, placeholder_text)
        self.text_input.config(fg='grey')
        
        # Bind events for placeholder
        self.text_input.bind('<FocusIn>', self.on_text_focus_in)
        self.text_input.bind('<FocusOut>', self.on_text_focus_out)
        
        # Button frame
        button_frame = ttk.Frame(main_frame)
        button_frame.grid(row=3, column=0, columnspan=3, pady=(0, 20))
        
        # Check button
        self.check_button = ttk.Button(button_frame, text="Check Grammar", 
                                     command=self.check_grammar_threaded, style='Modern.TButton')
        self.check_button.pack(side=tk.LEFT, padx=(0, 10))
        
        # Clear button
        self.clear_button = ttk.Button(button_frame, text="Clear Text", 
                                     command=self.clear_text, style='Modern.TButton')
        self.clear_button.pack(side=tk.LEFT, padx=(0, 10))
        
        # Sample text button
        self.sample_button = ttk.Button(button_frame, text="Load Sample", 
                                      command=self.load_sample_text, style='Modern.TButton')
        self.sample_button.pack(side=tk.LEFT, padx=(0, 10))
        
        # Progress bar
        self.progress = ttk.Progressbar(button_frame, mode='indeterminate')
        self.progress.pack(side=tk.LEFT, padx=(10, 0))
        
        # Results section
        results_frame = ttk.LabelFrame(main_frame, text="Grammar Check Results", padding="10")
        results_frame.grid(row=4, column=0, columnspan=3, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0, 20))
        results_frame.columnconfigure(0, weight=1)
        results_frame.rowconfigure(0, weight=1)
        
        # Create notebook for tabs
        self.notebook = ttk.Notebook(results_frame)
        self.notebook.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Errors tab
        errors_frame = ttk.Frame(self.notebook)
        self.notebook.add(errors_frame, text="Errors")
        
        self.errors_text = scrolledtext.ScrolledText(errors_frame, height=10, width=80, font=('Arial', 10))
        self.errors_text.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # Suggestions tab
        suggestions_frame = ttk.Frame(self.notebook)
        self.notebook.add(suggestions_frame, text="Suggestions")
        
        self.suggestions_text = scrolledtext.ScrolledText(suggestions_frame, height=10, width=80, font=('Arial', 10))
        self.suggestions_text.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # Statistics tab
        stats_frame = ttk.Frame(self.notebook)
        self.notebook.add(stats_frame, text="Statistics")
        
        self.stats_text = scrolledtext.ScrolledText(stats_frame, height=10, width=80, font=('Arial', 10))
        self.stats_text.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # Status bar
        self.status_label = ttk.Label(main_frame, text="Ready", style='Success.TLabel')
        self.status_label.grid(row=5, column=0, columnspan=3, sticky=tk.W)
        
    def load_sample_text(self):
        """Load sample text with various grammar errors"""
        sample_text = """this is a sample text with various grammar errors. i went to the store yesterday and bought a apple. He are very happy about his purchase. Your welcome to check this text for errors.

The sentence structure can be improved, and their are some spelling mistakes. I should of been more careful when writing this. There is many errors in this text that needs to be corrected.

monday is a good day to start the week. The students was excited about the test results."""
        
        self.text_input.delete('1.0', tk.END)
        self.text_input.insert('1.0', sample_text)
        self.text_input.config(fg='black')
        
    def on_text_focus_in(self, event):
        """Handle focus in event for placeholder text"""
        if self.text_input.get('1.0', tk.END).strip() == "Enter your text here to check for grammar errors...":
            self.text_input.delete('1.0', tk.END)
            self.text_input.config(fg='black')
    
    def on_text_focus_out(self, event):
        """Handle focus out event for placeholder text"""
        if not self.text_input.get('1.0', tk.END).strip():
            self.text_input.insert('1.0', "Enter your text here to check for grammar errors...")
            self.text_input.config(fg='grey')
    
    def clear_text(self):
        """Clear all text areas"""
        self.text_input.delete('1.0', tk.END)
        self.text_input.insert('1.0', "Enter your text here to check for grammar errors...")
        self.text_input.config(fg='grey')
        
        self.errors_text.delete('1.0', tk.END)
        self.suggestions_text.delete('1.0', tk.END)
        self.stats_text.delete('1.0', tk.END)
        
        self.status_label.config(text="Ready", style='Success.TLabel')
    
    def check_grammar_threaded(self):
        """Start grammar checking in a separate thread"""
        text = self.text_input.get('1.0', tk.END).strip()
        
        if not text or text == "Enter your text here to check for grammar errors...":
            messagebox.showwarning("Warning", "Please enter some text to check.")
            return
        
        # Disable button and start progress
        self.check_button.config(state='disabled')
        self.progress.start()
        self.status_label.config(text="Checking grammar...", style='Error.TLabel')
        
        # Start thread
        thread = threading.Thread(target=self.check_grammar_worker, args=(text,))
        thread.daemon = True
        thread.start()
        
        # Check queue periodically
        self.root.after(100, self.check_queue)
    
    def check_grammar_worker(self, text):
        """Worker function for grammar checking"""
        try:
            errors, suggestions = self.grammar_checker.check_grammar(text)
            self.queue.put(('success', errors, suggestions, text))
        except Exception as e:
            self.queue.put(('error', str(e)))
    
    def check_queue(self):
        """Check the queue for results from worker thread"""
        try:
            result = self.queue.get_nowait()
            if result[0] == 'success':
                errors, suggestions, text = result[1], result[2], result[3]
                self.display_results(errors, suggestions, text)
            elif result[0] == 'error':
                messagebox.showerror("Error", f"An error occurred: {result[1]}")
                self.status_label.config(text="Error occurred", style='Error.TLabel')
        except queue.Empty:
            # No result yet, check again later
            self.root.after(100, self.check_queue)
            return
        
        # Re-enable button and stop progress
        self.check_button.config(state='normal')
        self.progress.stop()
    
    def display_results(self, errors, suggestions, text):
        """Display the grammar check results"""
        # Clear previous results
        self.errors_text.delete('1.0', tk.END)
        self.suggestions_text.delete('1.0', tk.END)
        self.stats_text.delete('1.0', tk.END)
        
        # Display errors
        if errors:
            self.errors_text.insert(tk.END, f"Found {len(errors)} grammar errors:\n\n")
            
            error_types = {}
            for i, error in enumerate(errors, 1):
                error_type = error['type']
                if error_type not in error_types:
                    error_types[error_type] = []
                error_types[error_type].append(error)
                
                self.errors_text.insert(tk.END, f"{i}. {error_type}: {error['message']}\n")
                if 'sentence' in error:
                    self.errors_text.insert(tk.END, f"   (Sentence {error['sentence'] + 1})\n")
                self.errors_text.insert(tk.END, "\n")
            
            # Color code error types
            self.highlight_errors()
        else:
            self.errors_text.insert(tk.END, "No grammar errors found! ✓\n\n")
            self.errors_text.insert(tk.END, "Your text appears to be grammatically correct.")
        
        # Display suggestions
        if suggestions:
            self.suggestions_text.insert(tk.END, "Suggestions for improvement:\n\n")
            for i, suggestion in enumerate(suggestions, 1):
                self.suggestions_text.insert(tk.END, f"{i}. {suggestion}\n\n")
        else:
            self.suggestions_text.insert(tk.END, "No specific suggestions available.\n\n")
            if not errors:
                self.suggestions_text.insert(tk.END, "Your text is well-written!")
        
        # Display statistics
        self.display_statistics(errors, suggestions, text)
        
        # Update status
        if errors:
            self.status_label.config(text=f"Found {len(errors)} errors", style='Error.TLabel')
        else:
            self.status_label.config(text="No errors found!", style='Success.TLabel')
    
    def highlight_errors(self):
        """Add color highlighting to different error types"""
        # Configure text tags for different error types
        self.errors_text.tag_configure("capitalization", foreground="red")
        self.errors_text.tag_configure("punctuation", foreground="blue")
        self.errors_text.tag_configure("agreement", foreground="purple")
        self.errors_text.tag_configure("article", foreground="orange")
        self.errors_text.tag_configure("repetition", foreground="brown")
        self.errors_text.tag_configure("structure", foreground="green")
        self.errors_text.tag_configure("mistake", foreground="maroon")
        self.errors_text.tag_configure("spelling", foreground="navy")
        
        # Apply tags based on error types
        content = self.errors_text.get('1.0', tk.END)
        lines = content.split('\n')
        
        for i, line in enumerate(lines):
            if "Capitalization:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("capitalization", start, end)
            elif "Punctuation:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("punctuation", start, end)
            elif "Agreement:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("agreement", start, end)
            elif "Article:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("article", start, end)
            elif "Repetition:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("repetition", start, end)
            elif "Structure:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("structure", start, end)
            elif "Mistake:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("mistake", start, end)
            elif "Spelling:" in line:
                start = f"{i+1}.0"
                end = f"{i+1}.end"
                self.errors_text.tag_add("spelling", start, end)
    
    def display_statistics(self, errors, suggestions, text):
        """Display text statistics"""
        # Basic text statistics
        sentences = len(re.split(r'[.!?]+', text))
        words = len(text.split())
        characters = len(text)
        characters_no_spaces = len(text.replace(' ', ''))
        
        # Error statistics
        error_types = {}
        for error in errors:
            error_type = error['type']
            if error_type not in error_types:
                error_types[error_type] = 0
            error_types[error_type] += 1
        
        # Calculate readability metrics
        avg_sentence_length = words / sentences if sentences > 0 else 0
        avg_word_length = characters_no_spaces / words if words > 0 else 0
        
        # Display statistics
        self.stats_text.insert(tk.END, "TEXT STATISTICS\n")
        self.stats_text.insert(tk.END, "=" * 50 + "\n\n")
        
        self.stats_text.insert(tk.END, f"Total Characters: {characters}\n")
        self.stats_text.insert(tk.END, f"Characters (no spaces): {characters_no_spaces}\n")
        self.stats_text.insert(tk.END, f"Total Words: {words}\n")
        self.stats_text.insert(tk.END, f"Total Sentences: {sentences}\n")
        self.stats_text.insert(tk.END, f"Average Sentence Length: {avg_sentence_length:.1f} words\n")
        self.stats_text.insert(tk.END, f"Average Word Length: {avg_word_length:.1f} characters\n\n")
        
        self.stats_text.insert(tk.END, "ERROR BREAKDOWN\n")
        self.stats_text.insert(tk.END, "=" * 50 + "\n\n")
        
        if error_types:
            self.stats_text.insert(tk.END, f"Total Errors: {len(errors)}\n")
            self.stats_text.insert(tk.END, f"Error Rate: {len(errors)/words*100:.1f} errors per 100 words\n\n")
            
            for error_type, count in sorted(error_types.items()):
                self.stats_text.insert(tk.END, f"{error_type}: {count}\n")
        else:
            self.stats_text.insert(tk.END, "No errors found!\n")
            self.stats_text.insert(tk.END, "Your text is grammatically correct.\n")
        
        # Readability assessment
        self.stats_text.insert(tk.END, "\nREADABILITY ASSESSMENT\n")
        self.stats_text.insert(tk.END, "=" * 50 + "\n\n")
        
        if avg_sentence_length < 15:
            readability = "Easy to read"
        elif avg_sentence_length < 20:
            readability = "Moderately easy to read"
        elif avg_sentence_length < 25:
            readability = "Moderate difficulty"
        else:
            readability = "May be difficult to read"
        
        self.stats_text.insert(tk.END, f"Readability: {readability}\n")
        self.stats_text.insert(tk.END, f"Recommendation: ")
        
        if avg_sentence_length > 25:
            self.stats_text.insert(tk.END, "Consider breaking long sentences into shorter ones.\n")
        elif avg_sentence_length < 8:
            self.stats_text.insert(tk.END, "Consider combining some short sentences for better flow.\n")
        else:
            self.stats_text.insert(tk.END, "Sentence length is appropriate.\n")

def main():
    """Main function to run the application"""
    root = tk.Tk()
    app = ModernGrammarCheckerGUI(root)
    
    # Center the window on screen
    root.update_idletasks()
    width = root.winfo_width()
    height = root.winfo_height()
    x = (root.winfo_screenwidth() // 2) - (width // 2)
    y = (root.winfo_screenheight() // 2) - (height // 2)
    root.geometry(f'{width}x{height}+{x}+{y}')
    
    # Set minimum window size
    root.minsize(800, 600)
    
    # Handle window closing
    def on_closing():
        root.quit()
        root.destroy()
    
    root.protocol("WM_DELETE_WINDOW", on_closing)
    
    # Start the GUI
    try:
        root.mainloop()
    except KeyboardInterrupt:
        print("\nApplication interrupted by user")
    except Exception as e:
        print(f"Application error: {e}")
    finally:
        try:
            root.destroy()
        except:
            pass

if __name__ == "__main__":
    main()