
#Lispin. A lisp interpreter.
#    Copyright (C) 2018  e-dt
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
def pretty_print(lst):
    new = []
    for i in iterate(lst):
        if type(i) != Cons: 
            new.append(i)
        else:
            new.append(pretty_print(i))
    return new
def pretty(lt):
    #for debugging purposes only
    print(pretty_print(lt))
class Cons: #Pair object
    """A pair."""
    def __init__(self, car, cdr):
        self.car = car #first
        self.cdr = cdr #rest (in linked list)
    def __str__(self):
        return "(" + str(self.car) + " . " + str(self.cdr) + ")"
    def __repr__(self):
        return "(" + repr(self.car) + " . " + repr(self.cdr) +")"
class LispFunction:
    """A callable lisp function."""
    def __init__(self, lambdaexpr, env, macros):
        self.lambdaexpr = lambdaexpr #get body of self
        self.args = list(iterate(self.lambdaexpr.cdr.car))
        #cdr.car (lambda (x) ...) = (x)
        self.returnv = self.lambdaexpr.cdr.cdr.car
        #cdr.cdr.car of (lambda (x y) (do x y z...))
        #is cdr.car of ((x y) (do x y z)) is (do x y z)
        self.env = env #Le epic lexical scoping
        #PLEASE NOTE -- lambdas only take one expression.
        #if you want to use more than one, use a (begin ...)
        self.macros = macros
    def __call__(self, *vals):
        newenv = Environment(self.env, parent=self.env)
        newenv.update(zip(self.args,vals)) #defines the arguments' values to be the values passed in
        return evaluate(self.returnv, newenv, Environment(self.macros,parent=self.macros))
    def __repr__(self):
        return "func" #debugging
        

class Environment(dict):
    def __init__(self, *args, **kwargs):
        self.parent = kwargs.pop('parent') #parent is the parent environment, e.g. global environment
        dict.__init__(self, *args, **kwargs)

    def innermost(self, var):
        """Gets innermost environment in which `var` appears."""
        return self if var in self else self.parent.innermost(var)

def syntax_sugar(program):
    new = []
    index = 0
    while index < len(program):
        val = program[index]
        if val == "'": #'x -> (quote x)
            new.append(["quote", program[index+1]])
            index += 1 #skip next value, as it is already used in the quote expr
        elif type(val) == list:
            new.append(syntax_sugar(val))#recurse
        else:
            new.append(val)
        index += 1
    return recursive_mll(new)
            
            
def make_linked_list(iterable):
    """Makes a linked list out of an iterable."""
    if len(iterable) == 0:
        return None #EMPTY LIST == Nil not (Nil, Nil)
    current = Cons(None, None)
    start = current #remember the start of the linked list (works because cons are mutable)
    for i in iterable:
        if current.car == None:
            current.car = i
        else:
            current.cdr = Cons(i, None)
            current = current.cdr
    return start

def recursive_mll(iterable):
    """Makes a linked list out of an iterable. Any iterables inside the 
    iterable are make into linked lists too, and so on."""
    new = []
    for i in iterable:
        if type(i) != list: 
            new.append(i)
        else:
            new.append(recursive_mll(i))
    return make_linked_list(new)
def iterate(linked): #makes an iterable out of a linked list. needed because linked lists are just Cons
    class LLIterable:
        def __init__(self, linkedlist):
            self.linkedlist = linkedlist
        def __iter__(self):
            class LLIterator:
                def __init__(self, ll):
                    self.current = ll
                def __next__(self):
                    if self.current == None:
                        raise StopIteration
                    this = self.current.car
                    self.current = self.current.cdr
                    return this
            return LLIterator(self.linkedlist)
    return LLIterable(linked)

#NOT CURRENTLY USED, BUT COULD COME IN HANDY

#def append_to_linkedlist(linked, obj):
#    current = linked
#    while current.cdr != None:
#        current = current.cdr
#    current.cdr = Cons(obj, None)
#    return current

def tokenise(code_str):
    tokenised = []
    token_built = ""
    for i in code_str: 
        if i == "(":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append("(") #tokens are represented by themselves for now
        elif i == ")":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(")")
        elif i == " " or i == "\n":
            if (token_built != ""): tokenised.append(token_built) 
            token_built = ""
        elif i == "'":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append("'")
        else:
            token_built += i

    return tokenised

   
def parse(tokenised):
    """Turns tokens into objects, parses parens."""
    #I forgot how this works and why I named this listy.
    #I was sleep-deprived, okay?
    listy = [[]]
    for token in tokenised:
        if token == "(":
            listy.append([])
        elif token == ")":
            listy[-2].append(listy.pop())
        else:
            try:
                listy[-1].append(int(token))
            except:
                try:
                    listy[-1].append(float(token))
                except:
                    listy[-1].append(token)
    
    return listy[0]


def func_n_vals(function):
    """Takes function application and seperates into function and values."""
    func = function.car
    vals = [i for i in iterate(function.cdr)] #list comprehensions r fun!
    return func, vals

def apply(function, localvars, macros):
    """Applies a function in an environment."""
    try:
        function = macroexpand(function, localvars, macros)
    except:
        pass
    if function.car == 'quote': #required to fix a bug
        return function.cdr.car #maybe? unnecessary but who knows
    func, vals = func_n_vals(function)
    if func == 'cond':
        for cons in vals:
            if evaluate(cons.car, localvars, macros) != None:    #None is the only false value; it is also the empty list.
                return evaluate(cons.cdr.car, localvars, macros) #
        raise Exception('no default in cond, and it fell through!!') #should this be the behaviour? discuss.
    elif func == 'lambda': 
        return LispFunction(function, localvars, macros)
    elif func == 'define':
        localvars[vals[0]] = evaluate(vals[1], localvars, macros)
        return localvars[vals[0]]
    elif func == 'set!':
        localvars.innermost(vals[0])[vals[0]] = evaluate(vals[1], localvars, macros)
    elif func == 'defmacro':
        macros[vals[0]] =  evaluate(vals[1], localvars, macros)
    else: #defined
        func = evaluate(func, localvars, macros)
        vals = [evaluate(val,localvars, macros) for val in vals]
        return func(*vals) #for builtins
def evaluate(expr,localvars,macros):
    if type(expr) == Cons:
        return apply(expr,localvars, macros)
    else:
        if type(expr) == str:
            return localvars.innermost(expr)[expr]#variables!
        else:
            return expr
def macroexpand(expr, localvars, macros):
    func, vals = func_n_vals(expr)
    macro = macros.innermost(func)[func]
    return macro(*vals)
def defaultenv():
    """Creates a sane environment."""
    env = Environment({}, parent=None)
    def eq(val0, val1):                              #MCCARTHY PAPER VERSION
        if type(val0) == Cons or type(val1) == Cons:
            raise Exception("eq doesn't take a cons") #why not? i don't know, but this is helpful to me
        return "t" if val0 == val1 else None 
    def begin(*vals):
        return vals[-1] #all arguments are evaluated, the last is returned. nice
    def lst(*vals):
        return make_linked_list(vals)
    
    env.update([
        ('eq?', eq), #('eq?', (lambda x,y: x is y), #SCHEME VERSION
        ('car', (lambda x: x.car)),
        ('cdr', (lambda x: x.cdr)),
        ('cons', (lambda x, y: Cons(x, y))),
        ('atom?', (lambda x: not isinstance(x, Cons))),
        ('begin', begin),
        ('write', (lambda x: print(x) or x)), #dirty hack -- print always returns None :. this always returns x, but prints x first.
        #MATHS
        ('+', (lambda x, y: x+y)),
        ('-', (lambda x, y: x-y)),
        ('*', (lambda x, y: x*y)),
        ('/', (lambda x, y: x/y)),
        #LOGIC
        ('and', (lambda x, y: (x != None) and (y != None))), #PLEASE NOTE NONTY SPECIAL FORM
        #TODO: write as le epic macro !
        ('not', (lambda x: "t" if x == None else None)),
        ('number?', (lambda x: type(x) == int or type(x) == float)),
        #LISTS
        ('list', lst)
    ])
    return env

globals = defaultenv()
macros = Environment({}, parent=None)
def execute(parsed):         #executes program
    """Executes program."""
    for i in iterate(parsed):
        evaluate(i, globals, macros)    #run each lisp.

def run(program):
    execute(syntax_sugar(parse(tokenise(program))))

if __name__ == "__main__": #auto-test
    test = """
(defmacro or (lambda (x y) ((lambda ($gensym?$my-long-name$add-hygiene-later$)
                                    (list 'cond (list $gensym?$my-long-name$add-hygiene-later$ $gensym?$my-long-name$add-hygiene-later$) (list '(quote t) y))) x)))
(define fibonacci
 (lambda (n)
  (cond
   ((eq? n 1) 0)
   ((eq? n 2) 1)
   ('t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
  )
 )
)

(write (fibonacci 7))
(write (or '() 2))
"""
    run(test)
