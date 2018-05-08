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

##TODO:
##* dynamic variables
##  pass around a `dynvars` variable
##  dynmacros?
##  will complicate environment functions, but not much
##
import copy
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
    def __call__(self, *vals, inheritedcontext=None):
        if inheritedcontext != None:
            env = inheritedcontext
        else:
            env = self.env
        newenv = Environment(env, parent=env)
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

class Symbol(str):
    pass #is the same

def syntax_sugar(program):
    new = []
    index = 0
    while index < len(program):
        val = program[index]
        if val == Symbol("'"): #'x -> (quote x)
            new.append([Symbol("quote"), program[index+1]])
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
    string_mode = False
    for i in code_str:
        if string_mode:
            if i != '"':
                token_built += i
                continue
            else:
                string_mode = False
                continue
        if i == '"':
            string_mode = True
            continue
        if i == "(":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Symbol("(")) #tokens are represented by themselves for now
        elif i == ")":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Symbol(")"))
        elif i == " " or i == "\n":
            if (token_built != ""): tokenised.append(token_built) 
            token_built = ""
        elif i == "'":
            if (token_built != ""): tokenised.append(token_built)
            token_built = ""
            tokenised.append(Symbol("'"))
        else:
            token_built = Symbol(token_built + Symbol(i))

    return tokenised

   
def parse(tokenised):
    """Turns tokens into objects, parses parens."""
    #I forgot how this works and why I named this listy.
    #I was sleep-deprived, okay?
    listy = [[]]
    for token in tokenised:
        if token == Symbol("("):
            listy.append([])
        elif token == Symbol(")"):
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
    global startvars
    try:
        function = macroexpand(function, localvars, macros)
    except:
        pass
    if function.car == Symbol('quote'): #required to fix a bug
        return function.cdr.car #maybe? unnecessary but who knows
    func, vals = func_n_vals(function)
    if func == Symbol('cond'):
        for cons in vals:
            if evaluate(cons.car, localvars, macros) != None:    #None is the only false value; it is also the empty list.
                return evaluate(cons.cdr.car, localvars, macros) #
        raise Exception('no default in cond, and it fell through!!') #should this be the behaviour? discuss.
    elif func == Symbol('lambda'): 
        return LispFunction(function, localvars, macros)
    elif func == Symbol('define'):
        localvars[vals[0]] = evaluate(vals[1], localvars, macros)
        return localvars[vals[0]]
    elif func == Symbol('set!'):
        localvars.innermost(vals[0])[vals[0]] = evaluate(vals[1], localvars, macros)
        return localvars.innermost(vals[0])[vals[0]]
    elif func == Symbol('defmacro'):
        macros[vals[0]] =  evaluate(vals[1], localvars, macros)
    elif func == Symbol('current-env'):
        return (copy.deepcopy(localvars),#here the mutable nature of dicts
                copy.deepcopy(macros))   #works against us, requiring a copy
    elif func == Symbol('let-env'):
        newvars, newmacros = evaluate(vals[0], localvars, macros)
        a=LispFunction(recursive_mll(["lambda", [[]], vals[1]]),
                       newvars, newmacros)
        return a()
    elif func == Symbol('set-env!'):
        newvars, newmacros = evaluate(vals[0], localvars, macros)
        if localvars is startvars: #i.e. if top-level (dicts mutable)
            startvars = newvars
        localvars = newvars
        macros = newmacros
        return None
    else: #defined
        func = evaluate(func, localvars, macros)
        vals = [evaluate(val, localvars, macros) for val in vals]
        return func(*vals) #for builtins
def evaluate(expr,localvars,macros):
    if type(expr) == Cons:
        return apply(expr,localvars, macros)
    else:
        if type(expr) == Symbol:
            #print(expr)
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
        return Symbol("t") if val0 == val1 else None 
    def begin(*vals):
        return vals[-1] #all arguments are evaluated, the last is returned. nice
    def lst(*vals):
        return make_linked_list(vals)
    def set_parent(environment, parentenv):
        environment[0].parent, environment[1].parent = parentenv
        return environment
    def modify_parent(environment, parentenv):
        newenv = copy.deepcopy(environment)
        newenv[0].parent, newenv[1].parent = parentenv
        return newenv
    def set_env(environment, a, b):
        environment[0].innermost(a)[a] = b
        return environment
    def modify_env(environment, a, b):
        newenv = copy.deepcopy(environment)
        newenv.innermost[0](a)[a] = b
        return newenv
    def define_env(environment, a, b):
        environment[0][a] = b
        return environment
    def defmodify_env(environment, a, b):
        newenv = copy.deepcopy(environment)
        newenv[0][a] = b
        return environment
    #cant be arsed to do macro env functions rn, TODO
    def in_envp(environment, a):
        try:
            environment[0].innermost(a)
            return Symbol("t")
        except:
            return None
    def in_top_envp(environment, a):
        try:
            environment[0][a]
            return Symbol("t")
        except:
            return None
    env.update([(Symbol(i[0]),i[1]) for i in [
        ('eq?', eq), #('eq?', (lambda x,y: x is y), #SCHEME VERSION
        ('car', (lambda x: x.car)),
        ('cdr', (lambda x: x.cdr)),
        ('cons', (lambda x, y: Cons(x, y))),
        ('atom?', (lambda x: not isinstance(x, Cons))),
        ('begin', begin),
        ('exit', (lambda x=None: exit() if x == None else exit(x))),
        ('write', (lambda x: print(x) or x)), #dirty hack -- print always returns None :. this always returns x, but prints x first.
        #ENV
        ('environment?', (lambda x: Symbol("t") if\
                          ((type(x[0]) == Environment) and\
                          (type(x[1]) == Environment) and\
                           (type(x) == tuple)) else None)),
        ('set-parent!', set_parent),
        ('modify-parent', modify_parent),
        ('parent', (lambda x: (x[0].parent, x[1].parent))),
        ('parent?', (lambda x, y: Symbol("t") if ((x[0].parent is y[0]) and (x[1].parent is y[1])) else None)),
        ('set-in-env!', set_env),
        ('modify-in-env', modify_env),
        ('define-in-env!', define_env),
        ('modify-define-in-env', defmodify_env),
        ('in-env', (lambda x, y: x.innermost(y)[y])),
        ('in-env?', in_envp),
        ('in-top-env?', in_top_envp),
        #MATHS
        ('+', (lambda x, y: x+y)),
        ('-', (lambda x, y: x-y)),
        ('*', (lambda x, y: x*y)),
        ('/', (lambda x, y: x/y)),
        #LOGIC
        ('and', (lambda x, y: (x != None) and (y != None))), #PLEASE NOTE NONTY SPECIAL FORM
        #TODO: write as macro !
        ('not', (lambda x: Symbol("t") if x == None else None)),
        ('number?', (lambda x: type(x) == int or type(x) == float)),
        #LISTS
        ('list', lst)
    ]])
    #MACROS
    macros = Environment({}, parent = None)
    #Hard to have builtin macros because hard to eval in builtin macros
    return env, macros

startvars, macros = defaultenv()
def execute(parsed):          #executes program
    """Executes program."""
    for i in iterate(parsed):
        evaluate(i, startvars, macros)    #run each lisp.

def run(program):
    execute(syntax_sugar(parse(tokenise(program))))

if __name__ == "__main__": #auto-test
    test ="""
(write "hello")
(defmacro or
 (lambda (x y)
  ((lambda (t)
   (list 'cond
    (list t t)
    (list '(quote t) y)))
  x)
 )
)
(define fibonacci
 (lambda (n)
  (cond
   ((eq? n 1) 0)
   ((eq? n 2) 1)
   ('t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
  )
 )
)
(write 'fibonacci-test-7)
(write (fibonacci 7))
(write 'macro-or-test-2)
(write ((lambda (t) (or t 2)) '()))
(write 'evaluation-test-x)
(write ((lambda (x) x) 'x)) 
(write 'let-env-basic-test-2)
(let-env (current-env) (write ((lambda (t) (or '() t)) 2)))
(define x 2)
(define env (current-env))
(write 'set-env!-basic-test-2)
(set-env! env)
(write x)
(define env (current-env))
(define x 1)
(set-env! env)
(write 'set-env!-advanced-test-2)
(write x)
(define env (current-env))
(write 'parent-env-test-t)
(write (parent? env (parent env)))
(exit 0)
"""
    run(test)
