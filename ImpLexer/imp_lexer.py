from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import *

class ImpLexer(RegexLexer):
    name = 'Imp'
    aliases = ['imp']
    filenames = ['*.imp']

    tokens = {
        'main stuff': [
            (r'\s+', Text),
            (r'\b(local|case|of|while|is|null)\b', Keyword),
            (r'(\->|:=|\+\+|<\+|\-)', Operator),
            (r'[\(\)\[\];,→]', Punctuation),
            (r'(\w+)(\()([\w, ]+?)(\))( )(on)( )([\w, ]+?)',
             bygroups(
                 Name.Function,
                 Punctuation,
                 Name.Variable,
                 Punctuation,
                 Whitespace,
                 Operator.Word,
                 Whitespace,
                 Name.Variable)),
            (r'([\w:\']+?)', Name.Variable),
            (r'\b\d+\b', Number),
            (r'"[^"]*"', String),
            (r'//.*?$', Comment.Singleline),
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
        ],
        'function' : [
            include('main stuff'),
            (r'\}', Punctuation, '#pop'),
        ],
        'parameters' : [
            (r'(,)', Punctuation),
            (r'( )', Whitespace),
            (r'(\w+)', Name.Variable),
            (r'\)', Punctuation, '#pop'),
        ],
        'function_def' : [
            (r'(\w+)(\()',
             bygroups(
                 Name.Function,
                 Punctuation),
             'parameters'),
            (r'( )(=)( )({)',
             bygroups(
                 Whitespace,
                 Punctuation,
                 Whitespace,
                 Punctuation),
             'function'),
        ],
        'root': [
            include('function_def'),
            include('main stuff'),
        ],
    }

